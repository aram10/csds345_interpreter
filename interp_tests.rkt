#lang racket/base

(require rackunit
        rackunit/text-ui
        rackunit/gui
        "./main.rkt"
        "./simpleParser.rkt"
        "./helpers.rkt")

(define emptystate '(()()))

(define class-tests
    (test-suite
        "Class tests"
        
        ; v1
        (test-case "Test 0" (check-equal? (interpret "./tests/0.txt") 150 ))
        (test-case "Test 1" (check-equal? (interpret "./tests/1.txt")  -4))
        (test-case "Test 2" (check-equal? (interpret "./tests/2.txt")  10))
        (test-case "Test 3" (check-equal? (interpret "./tests/3.txt")  16))
        (test-case "Test 4" (check-equal? (interpret "./tests/4.txt")  220))
        (test-case "Test 5" (check-equal? (interpret "./tests/5.txt")  5))
        (test-case "Test 6" (check-equal? (interpret "./tests/6.txt")  6))
        (test-case "Test 7" (check-equal? (interpret "./tests/7.txt")  10))
        (test-case "Test 8" (check-equal? (interpret "./tests/8.txt")  5 ))
        (test-case "Test 9" (check-equal? (interpret "./tests/9.txt")  -39))
        (test-case "Test 10" (check-exn exn:fail? (lambda () (interpret "./tests/10.txt"))))
        (test-case "Test 11" (check-exn exn:fail? (lambda () (interpret "./tests/11.txt"))))
        (test-case "Test 12" (check-exn exn:fail? (lambda () (interpret "./tests/12.txt"))))
        (test-case "Test 13" (check-exn exn:fail? (lambda () (interpret "./tests/13.txt"))))
        (test-case "Test 14" (check-equal? (interpret "./tests/14.txt")  'true))
        (test-case "Test 15" (check-equal? (interpret "./tests/15.txt")  100))
        (test-case "Test 16" (check-equal? (interpret "./tests/16.txt")  'false))
        (test-case "Test 17" (check-equal? (interpret "./tests/17.txt")  'true))
        (test-case "Test 18" (check-equal? (interpret "./tests/18.txt")  128))
        (test-case "Test 19" (check-equal? (interpret "./tests/19.txt")  12))
        (test-case "Test 20" (check-equal? (interpret "./tests/28.txt")  100))
        (test-case "Test 21" (check-equal? (interpret "./tests/29.txt")  5))
        (test-case "Test 22" (check-equal? (interpret "./tests/30.txt")  5))
        (test-case "Test 23" (check-equal? (interpret "./tests/31.txt")  5))

        ; v2
        (test-case "Test 24" (check-equal? (interpret "./tests_v2/0.txt") 20))
        

    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)