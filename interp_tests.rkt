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
        (test-case "Test 10" (check-exn exn:fail? (λ () (interpret "./tests/10.txt"))))
        (test-case "Test 11" (check-exn exn:fail? (λ () (interpret "./tests/11.txt"))))
        (test-case "Test 12" (check-exn exn:fail? (λ () (interpret "./tests/12.txt"))))
        (test-case "Test 13" (check-exn exn:fail? (λ () (interpret "./tests/13.txt"))))
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
        (test-case "Test 28" (check-exn exn:fail? (λ () (interpret "./tests_v2/4.txt"))))
        (test-case "Test 28" (check-equal? (interpret "./tests_v2/5.txt") 25))
        (test-case "Test 29" (check-equal? (interpret "./tests_v2/6.txt") 21))
        (test-case "Test 30" (check-equal? (interpret "./tests_v2/7.txt") 6))
        (test-case "Test 31" (check-equal? (interpret "./tests_v2/8.txt") -1))
        (test-case "Test 32" (check-equal? (interpret "./tests_v2/9.txt") 789))
        (test-case "Test 33" (check-equal? (interpret "./tests_v2/14.txt") 125))
        (test-case "Test 34" (check-equal? (interpret "./tests_v2/15.txt") 110))
        (test-case "Test 35" (check-equal? (interpret "./tests_v2/16.txt") 2000400))
        (test-case "Test 36" (check-equal? (interpret "./tests_v2/17.txt") 101))
        (test-case "Test 37" (check-exn exn:fail? (λ () (interpret "./tests_v2/18.txt"))))

         ; v3
        (test-case "Test 38" (check-equal? (interpret "./tests_v3/0.txt") 10))
        (test-case "Test 39" (check-equal? (interpret "./tests_v3/1.txt") 14))
        (test-case "Test 40" (check-equal? (interpret "./tests_v3/2.txt") 45))
        (test-case "Test 41" (check-equal? (interpret "./tests_v3/3.txt") 55))
        (test-case "Test 42" (check-equal? (interpret "./tests_v3/4.txt") 1))
        (test-case "Test 43" (check-equal? (interpret "./tests_v3/5.txt") 115))
        (test-case "Test 44" (check-equal? (interpret "./tests_v3/6.txt") 'true))
        (test-case "Test 45" (check-equal? (interpret "./tests_v3/7.txt") 20))
        (test-case "Test 46" (check-equal? (interpret "./tests_v3/8.txt") 24))
        (test-case "Test 47" (check-equal? (interpret "./tests_v3/9.txt") 2))
        (test-case "Test 48" (check-equal? (interpret "./tests_v3/10.txt") 35))
        (test-case "Test 49" (check-exn exn:fail? (λ () (interpret "./tests_v3/11.txt"))))
        (test-case "Test 50" (check-equal? (interpret "./tests_v3/12.txt") 90))
        (test-case "NICE" (check-equal? (interpret "./tests_v3/13.txt") 69))
        (test-case "Test 51" (check-equal? (interpret "./tests_v3/14.txt") 87))
        (test-case "Test 52" (check-equal? (interpret "./tests_v3/15.txt") 64))
        (test-case "Test 53" (check-exn exn:fail? (λ () (interpret "./tests_v3/16.txt"))))
        (test-case "Test 54" (check-equal? (interpret "./tests_v3/17.txt") 125))
        (test-case "Test 55" (check-equal? (interpret "./tests_v3/18.txt") 100))
        (test-case "Test 56" (check-equal? (interpret "./tests_v3/19.txt") 2000400))


        ; custom
        (test-case "Test custom_1" (check-equal? (interpret "./custom_tests/1.txt") 13))
        (test-case "Test custom_2" (check-equal? (interpret "./custom_tests/2.txt") 5))
        (test-case "Test custom_3" (check-equal? (interpret "./custom_tests/3.txt") 25))
        (test-case "Test custom_4" (check-equal? (interpret "./custom_tests/4.txt") 10))
        (test-case "Test custom_5" (check-equal? (interpret "./custom_tests/5.txt") -1))
        (test-case "Test custom_6" (check-equal? (interpret "./custom_tests/6.txt") -60))
        (test-case "Test custom_7" (check-equal? (interpret "./custom_tests/7.txt") 30))
        (test-case "Test custom_8" (check-equal? (interpret "./custom_tests/8.txt") 2))
        (test-case "Test custom_11" (check-equal? (interpret "./custom_tests/11.txt") 11))
    ))

(run-tests class-tests 'verbose)
(test/gui class-tests)