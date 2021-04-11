#lang racket/base

(require rackunit
        rackunit/text-ui
        rackunit/gui
        "./main.rkt"
        "./functionParser.rkt"
        "./helpers.rkt")

(define emptystate '(()()))

(define class-tests
    (test-suite
     "Functional Tests"
             ; v3
        (test-case "Test 38" (check-equal? (interpret-w-func "./tests_v3/0.txt") 10))
        (test-case "Test 39" (check-equal? (interpret-w-func "./tests_v3/1.txt") 14))
        (test-case "Test 40" (check-equal? (interpret-w-func "./tests_v3/2.txt") 45))
        (test-case "Test 41" (check-equal? (interpret-w-func "./tests_v3/3.txt") 55))
        (test-case "Test 42" (check-equal? (interpret-w-func "./tests_v3/4.txt") 1))
        (test-case "Test 43" (check-equal? (interpret-w-func "./tests_v3/5.txt") 115))
        (test-case "Test 44" (check-equal? (interpret-w-func "./tests_v3/6.txt") 'true))
        (test-case "Test 45" (check-equal? (interpret-w-func "./tests_v3/7.txt") 20))
        (test-case "Test 46" (check-equal? (interpret-w-func "./tests_v3/8.txt") 24))
        (test-case "Test 47" (check-equal? (interpret-w-func "./tests_v3/9.txt") 2))
        (test-case "Test 48" (check-equal? (interpret-w-func "./tests_v3/10.txt") 35))
        (test-case "Test 49" (check-exn exn:fail? (lambda () (interpret-w-func "./tests_v3/11.txt"))))
        (test-case "Test 50" (check-equal? (interpret-w-func "./tests_v3/12.txt") 90))
        (test-case "NICE" (check-equal? (interpret-w-func "./tests_v3/13.txt") 69))
        (test-case "Test 51" (check-equal? (interpret-w-func "./tests_v3/14.txt") 87))
        (test-case "Test 52" (check-equal? (interpret-w-func "./tests_v3/15.txt") 64))
        (test-case "Test 53" (check-exn exn:fail? (lambda () (interpret-w-func "./tests_v3/16.txt"))))
        (test-case "Test 54" (check-equal? (interpret-w-func "./tests_v3/17.txt") 125))
        (test-case "Test 55" (check-equal? (interpret-w-func "./tests_v3/18.txt") 100))
        (test-case "Test 56" (check-equal? (interpret-w-func "./tests_v3/19.txt") 2000400))
        
        
        
        
        
        

    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)