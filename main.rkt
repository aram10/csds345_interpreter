#lang racket

#|
NOTES

state: ((list of names) (list of values))  e.g. ((x y z w...) (5 true 12 '()...))

|#


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

; Declares a new variable, and sets its value to null
(define declare
  (lambda (x state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons '() (vals state)) '()))))))

; Assigns a value to a variable. Errors if the variable does not exist
#|
DOES NOT WORK AS INTENDED. Still struggling with appending lists to lists
|#
(define assign
  (lambda (x v state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (error 'bad-assignment))
      ((eq? x (firstvar (vars state))) (cons (vars state) (cons (cons v (restvals (vals state))) '())))
      (else (cons (cons x (vars (assign x v (cons (restvars (vars state)) (restvals (vals state)))))) (cons v (vals (assign x v (cons (restvars (vars state)) (restvals (vals state)))))))))))

    
#| HELPER FUNCTIONS |#

(define member?
  (lambda (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

; Arithmetic/boolean expression helpers
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)

; Retrieve sublist of variables and sublist of values from the state, respectively
(define vars (lambda (state) (car state)))
(define vals (lambda (state) (cadr state)))

; Retrieve the first variable of the variables sublist
(define firstvar (lambda (vars) (car vars)))
; Retrieve the first value of the values sublist
(define firstval (lambda (vals) (car vals)))

; Retrieve all variables except the first
(define restvars (lambda (vars) (cdr vars)))
; Retrieve all values expect the first
(define restvals (lambda (vals) (cdr vals)))




