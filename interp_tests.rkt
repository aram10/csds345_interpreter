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
        (check-equal? (M-state (parser "./tests/0.txt") emptystate) 150 "Test 0")
        (check-equal? (M-state (parser "./tests/1.txt") emptystate) -4 "Test 1")
        (check-equal? (M-state (parser "./tests/2.txt") emptystate) 10 "Test 2")
        (check-equal? (M-state (parser "./tests/3.txt") emptystate) 16 "Test 3")
        (check-equal? (M-state (parser "./tests/4.txt") emptystate) 220 "Test 4")
        (check-equal? (M-state (parser "./tests/5.txt") emptystate) 5 "Test 5")
        (check-equal? (M-state (parser "./tests/6.txt") emptystate) 6 "Test 6")
        (check-equal? (M-state (parser "./tests/7.txt") emptystate) 10 "Test 7")
        (check-equal? (M-state (parser "./tests/8.txt") emptystate) 5 "Test 8")
        (check-equal? (M-state (parser "./tests/9.txt") emptystate) -39 "Test 9")
        (check-exn exn:fail? (lambda () (M-state (parser "./tests/10.txt") emptystate)) "Test 10")
        (check-exn exn:fail? (lambda () (M-state (parser "./tests/11.txt") emptystate)) "Test 11")
        (check-exn exn:fail? (lambda () (M-state (parser "./tests/12.txt") emptystate)) "Test 12")
        (check-exn exn:fail? (lambda () (M-state (parser "./tests/13.txt") emptystate)) "Test 13")
        (check-equal? (M-state (parser "./tests/14.txt") emptystate) 'true "Test 14")
        (check-equal? (M-state (parser "./tests/15.txt") emptystate) 100 "Test 15")
        (check-equal? (M-state (parser "./tests/16.txt") emptystate) 'false "Test 16")
        (check-equal? (M-state (parser "./tests/17.txt") emptystate) 'true "Test 17")
        (check-equal? (M-state (parser "./tests/18.txt") emptystate) 128 "Test 18")
        (check-equal? (M-state (parser "./tests/19.txt") emptystate) 12 "Test 19")
    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)