#lang racket


(define M-integer
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-operator)))))

(define M-boolean
  (lambda (expression)
    (cond
      ((boolean? expression) expression)
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression)) (M-boolean (rightoperand expression))))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression)) (M-boolean (rightoperand expression))))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression))))
      (else (error 'bad-operator)))))

; M-value: combination of M-integer and M-value

; state: list of names, list of values

; ((x y z w...) (5 true 12 '()...))
; 

; Declares a value and a type, and sets its value to be null
;THIS DOES NOT WORK: (declare 'x '(() ()))  ==>  '((x) ())
;expected: (declare 'x '(() ()))  ==> '((x) (()))
(define declare
  (lambda (var state)
    (cond
      ((member? var (vars state)) (error 'bad-declaration))
      (else
       (cons (cons var (vars state)) (cons (cons '() (vals state)) '()))))))

    
; HELPER FUNCTIONS

(define member?
  (lambda (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)

(define vars (lambda (state) (car state)))
(define vals (lambda (state) (cadr state)))




