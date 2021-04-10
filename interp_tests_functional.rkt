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

    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)