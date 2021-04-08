#lang racket/base

(require rackunit
        rackunit/text-ui
        rackunit/gui
        "./main.rkt"
        "./simpleParser.rkt"
        "./functionParser.rkt"
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
        (test-case "Test 25" (check-equal? (interpret "./tests_v2/1.txt") 164))
        (test-case "Test 26" (check-equal? (interpret "./tests_v2/2.txt") 32))
        (test-case "Test 27" (check-equal? (interpret "./tests_v2/3.txt") 2))
        (test-case "Test 28" (check-exn exn:fail? (lambda () (interpret "./tests_v2/4.txt"))))
        (test-case "Test 28" (check-equal? (interpret "./tests_v2/5.txt") 25))
        (test-case "Test 29" (check-equal? (interpret "./tests_v2/6.txt") 21))
        (test-case "Test 30" (check-equal? (interpret "./tests_v2/7.txt") 6))
        (test-case "Test 31" (check-equal? (interpret "./tests_v2/8.txt") -1))
        (test-case "Test 32" (check-equal? (interpret "./tests_v2/9.txt") 789))
        (test-case "Test 33" (check-equal? (interpret "./tests_v2/14.txt") 125))
        (test-case "Test 34" (check-equal? (interpret "./tests_v2/15.txt") 110))
        (test-case "Test 35" (check-equal? (interpret "./tests_v2/16.txt") 2000400))
        (test-case "Test 36" (check-equal? (interpret "./tests_v2/17.txt") 101))
        (test-case "Test 37" (check-exn exn:fail? (lambda () (interpret "./tests_v2/18.txt"))))

        ; custom
        (test-case "Test custom_1" (check-equal? (interpret "./rambo_tests/1.txt") 13))
        (test-case "Test custom_2" (check-equal? (interpret "./rambo_tests/2.txt") 5))
        (test-case "Test custom_3" (check-equal? (interpret "./rambo_tests/3.txt") 25))
        (test-case "Test custom_4" (check-equal? (interpret "./rambo_tests/4.txt") 10))
        (test-case "Test custom_5" (check-equal? (interpret "./rambo_tests/5.txt") -1))
        (test-case "Test custom_6" (check-equal? (interpret "./rambo_tests/6.txt") -60))
        (test-case "Test custom_7" (check-equal? (interpret "./rambo_tests/7.txt") 30))
        (test-case "Test custom_8" (check-equal? (interpret "./rambo_tests/8.txt") 2))
    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)