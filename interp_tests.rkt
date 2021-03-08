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
        (check-equal? (interpret "./tests/0.txt") 150 "Test 0")
        (check-equal? (interpret "./tests/1.txt")  -4 "Test 1")
        (check-equal? (interpret "./tests/2.txt")  10 "Test 2")
        (check-equal? (interpret "./tests/3.txt")  16 "Test 3")
        (check-equal? (interpret "./tests/4.txt")  220 "Test 4")
        (check-equal? (interpret "./tests/5.txt")  5 "Test 5")
        (check-equal? (interpret "./tests/6.txt")  6 "Test 6")
        (check-equal? (interpret "./tests/7.txt")  10 "Test 7")
        (check-equal? (interpret "./tests/8.txt")  5 "Test 8")
        (check-equal? (interpret "./tests/9.txt")  -39 "Test 9")
        (check-exn exn:fail? (lambda () (interpret "./tests/10.txt")) "Test 10")
        (check-exn exn:fail? (lambda () (interpret "./tests/11.txt")) "Test 11")
        (check-exn exn:fail? (lambda () (interpret "./tests/12.txt")) "Test 12")
        (check-exn exn:fail? (lambda () (interpret "./tests/13.txt")) "Test 13")
        (check-equal? (interpret "./tests/14.txt")  'true "Test 14")
        (check-equal? (interpret "./tests/15.txt")  100 "Test 15")
        (check-equal? (interpret "./tests/16.txt")  'false "Test 16")
        (check-equal? (interpret "./tests/17.txt")  'true "Test 17")
        (check-equal? (interpret "./tests/18.txt")  128 "Test 18")
        (check-equal? (interpret "./tests/19.txt")  12 "Test 19")
        (check-equal? (interpret "./tests/28.txt")  100 "Test 28")
        (check-equal? (interpret "./tests/29.txt")  5 "Test 29")
        (check-equal? (interpret "./tests/30.txt")  5 "Test 30")
        (check-equal? (interpret "./tests/31.txt")  5 "Test 31")
    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)