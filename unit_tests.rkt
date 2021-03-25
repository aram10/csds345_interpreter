#lang racket/base

(require rackunit
        rackunit/text-ui
        rackunit/gui
        "./helpers.rkt"
        "./simpleParser.rkt"
        "./main.rkt")

(define class-tests
    (test-suite
        "Class tests"
        (check-equal? (assigned? 'x (createnewstate)) #f "Test 0")
        (check-equal? (assigned? 'x '(((x)(#&5)))) #t "Test 1")
        (check-equal? (assigned? 'x '(((x)(#&())))) #f "Test 2")
        (check-equal? (assigned? 'x '(((w y z) (#&7 #&true #&())) ((k l)(#&() #&8)))) #f "Test 3")
        (check-equal? (assigned? 'x '(((y x z)(#&() #&true #&9)))) #t "Test 4")
        (check-equal? (assigned? 'x '(((y z)(#&8 #&())) ((x)(#&())))) #f "Test 5")
        (check-equal? (assigned? 'x '(((y z)(#&8 #&())) ((x)(#&())))) #f "Test 6")
        (check-equal? (assigned? 'x '(((y z)(#&8 #&())) ((q r x)(#&() #&7 #&())))) #f "Test 7")
        (check-equal? (assigned? 'x '(((y z)(#&8 #&())) ((q r x)(#&() #&7 #&false)))) #t "Test 8")

        (check-equal? (declared? 'x (createnewstate)) #f "Test 9")
        (check-equal? (declared? 'x '(((x)(#&5)))) #t "Test 10")
        (check-equal? (declared? 'x '(((x)(#&())))) #t "Test 11")
        (check-equal? (declared? 'x '(((w y z) (#&7 #&true #&())) ((k l)(#&() #&8)))) #f "Test 12")
        (check-equal? (declared? 'x '(((y x z)(#&() #&true #&9)))) #t "Test 13")
        (check-equal? (declared? 'x '(((y z)(#&8 #&())) ((x)(#&())))) #t "Test 14")
        (check-equal? (declared? 'x '(((y z)(#&8 #&())) ((x)(#&())))) #t "Test 15")
        (check-equal? (declared? 'x '(((y z)(#&8 #&())) ((q r x)(#&() #&7 #&())))) #t "Test 16")
        (check-equal? (declared? 'x '(((y z)(#&8 #&())) ((q r x)(#&() #&7 #&false)))) #t "Test 17")

        (check-exn exn:fail? (lambda () (get-val 'x (createnewstate))) "Test 18")
        (check-equal? (get-val 'x '(((x)(#&5)))) 5 "Test 19")
        (check-equal? (get-val 'x '(((x)(#&())))) '() "Test 20")
        (check-exn exn:fail? (lambda () (get-val 'x '(((w y z) (#&7 #&true #&())) ((k l)(#&() #&8))))) "Test 21")
        (check-equal? (get-val 'k '(((w y z) (#&7 #&true #&())) ((k l)(#&() #&8)))) '() "Test 22")
        (check-equal? (get-val 'x '(((y x z)(#&() #&true #&9)))) 'true "Test 23")
        (check-equal? (get-val 'x '(((y z)(#&8 #&())) ((q r x)(#&() #&7 #&())))) '() "Test 24")
    ))

(define main-tests
  (test-suite "Class Tests"
              (check-equal? (add 'x 4 (createnewstate)) '(((x)(#&4))) "Test add empty")
              (check-equal? (add 'y 4 '(((x)(#&5)))) '(((y x)(#&4 #&5))) "Test add existing")
              (check-equal? (add 'y 4 '((()()) ((x)(#&5)))) '(((y)(#&4)) ((x)(#&5))) "Test add empty first layer")
              (check-exn exn:fail? (lambda () (add 'y 4 '(((w y z) (#&7 #&true #&())) ((k l)(#&() #&8))))) "Test 21")

              (check-equal? (get-box 'x '(((x)(#&5)))) (box 5))
              
  ))
              

(run-tests class-tests 'verbose)
;(test/gui class-tests)
(run-tests main-tests 'verbose)
