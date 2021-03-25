#lang racket/base

(require rackunit
        rackunit/text-ui
        rackunit/gui
        "./main.rkt"
        "./simpleParser.rkt")

(define emptystate '(()()))

(define class-tests
    (test-suite
        "Class tests"
        (check-equal? (interpret "./tests_v1/0.txt") 150 "Test 0")
        (check-equal? (interpret "./tests_v1/1.txt")  -4 "Test 1")
        (check-equal? (interpret "./tests_v1/2.txt")  10 "Test 2")
        (check-equal? (interpret "./tests_v1/3.txt")  16 "Test 3")
        (check-equal? (interpret "./tests_v1/4.txt")  220 "Test 4")
        (check-equal? (interpret "./tests_v1/5.txt")  5 "Test 5")
        (check-equal? (interpret "./tests_v1/6.txt")  6 "Test 6")
        (check-equal? (interpret "./tests_v1/7.txt")  10 "Test 7")
        (check-equal? (interpret "./tests_v1/8.txt")  5 "Test 8")
        (check-equal? (interpret "./tests_v1/9.txt")  -39 "Test 9")
        (check-exn exn:fail? (lambda () (interpret "./tests_v1/10.txt")) "Test 10")
        (check-exn exn:fail? (lambda () (interpret "./tests_v1/11.txt")) "Test 11")
        (check-exn exn:fail? (lambda () (interpret "./tests_v1/12.txt")) "Test 12")
        (check-exn exn:fail? (lambda () (interpret "./tests_v1/13.txt")) "Test 13")
        (check-equal? (interpret "./tests_v1/14.txt")  'true "Test 14")
        (check-equal? (interpret "./tests_v1/15.txt")  100 "Test 15")
        (check-equal? (interpret "./tests_v1/16.txt")  'false "Test 16")
        (check-equal? (interpret "./tests_v1/17.txt")  'true "Test 17")
        (check-equal? (interpret "./tests_v1/18.txt")  128 "Test 18")
        (check-equal? (interpret "./tests_v1/19.txt")  12 "Test 19")
        (check-equal? (interpret "./tests_v1/28.txt")  100 "Test 28")
        (check-equal? (interpret "./tests_v1/29.txt")  5 "Test 29")
        (check-equal? (interpret "./tests_v1/30.txt")  5 "Test 30")
        (check-equal? (interpret "./tests_v1/31.txt")  5 "Test 31")
    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)