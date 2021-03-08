#lang racket/base

(require rackunit
        rackunit/text-ui
        "./main.rkt"
        "./simpleParser.rkt")

(define emptystate '(()()))

(define class-tests
    (test-suite
        "Class tests"
        (check-equal? (M-state (parser "./tests/0.txt") emptystate) 150)
        (check-equal? (M-state (parser "./tests/1.txt") emptystate) -4)
        (check-equal? (M-state (parser "./tests/2.txt") emptystate) 10)
        (check-equal? (M-state (parser "./tests/3.txt") emptystate) 16)
        (check-equal? (M-state (parser "./tests/4.txt") emptystate) 220)
        (check-equal? (M-state (parser "./tests/5.txt") emptystate) 5)
        (check-equal? (M-state (parser "./tests/6.txt") emptystate) 6)
        (check-equal? (M-state (parser "./tests/7.txt") emptystate) 10)
        (check-equal? (M-state (parser "./tests/8.txt") emptystate) 5)
        (check-equal? (M-state (parser "./tests/9.txt") emptystate) -39)
        (check-equal? (M-state (parser "./tests/14.txt") emptystate) 'true)
        (check-equal? (M-state (parser "./tests/15.txt") emptystate) 100)
        (check-equal? (M-state (parser "./tests/16.txt") emptystate) 'false)
        (check-equal? (M-state (parser "./tests/17.txt") emptystate) 'true)
        (check-equal? (M-state (parser "./tests/18.txt") emptystate) 128)
        (check-equal? (M-state (parser "./tests/19.txt") emptystate) 12)
    ))

(run-tests class-tests 'verbose)