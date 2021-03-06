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

; M-state
#|
  case =: assign x M_value(exp) M_state(exp)
|#

; M-value: combination of M-integer and M-value

; Declares a new variable, and sets its value to null
(define declare
  (lambda (x state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons '() (vals state)) '()))))))

; add a new variable, and sets its value to val
(define add
  (lambda (x v state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons v (vals state)) '()))))))

; Assigns a value to a variable. Errors if the variable does not exist
#|
DOES NOT WORK AS INTENDED. Still struggling with appending lists to lists

CASES: 
  - (= x (+ 5 3)
  - (= x (+ y 3))
  - (= x (= y (+ y 1)))
  - 

|#
(define assign
  (lambda (x v state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) state)
      ; ((eq? x (firstvar (vars state))) (cons (vars state) (cons (cons v (restvals (vals state))) '())))
      ; ;(else (cons (cons x (vars (assign x v (cons (restvars (vars state)) (restvals (vals state)))))) (cons v (vals (assign x v (cons (restvars (vars state)) (restvals (vals state)))))))))))
      ; (else (assign x v (cons (restvars (vars state)) (cons (restvals (vals state)) '())))))))
      (else (add x v (remove x state))))))

(define remove-cps
  (lambda (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return '(() ())))
      ((eq? x (firstvar (vars state))) (return (cons (restvars (vars state)) (cons (restvals (vals state)) '()))))
      (else (remove-cps x (cons (restvars (vars state)) (cons (restvals (vals state)) '())) 
            (lambda (s) (return (cons (cons (firstvar (vars state)) (vars s)) (cons (cons (firstval (vals state)) (vals s)) '())))))))))
          
(define M-compare
  (lambda (expression)
    (cond
      ((boolean? expression) (M-boolean expression))
      ((eq? (operator expression) '==) (= (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '!=) (not (= (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))))
      ((eq? (operator expression) '>=) (>= (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '<=) (<= (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '>) (> (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '<) (< (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-comparison)))))

#|
  M_state(while <condition> <loop_body>):
    // execute the side effect
    state_cond = M_state(<condition>, state)

    if M_boolean(<condition>, state_cond) is true
        return M_state(while <condition> <loop_body>, M_state(<loop_body>, state_cond))
    else
        return state_cond
  Sample tree
  (parser "csds345_interpreter\\tests\\26.txt")
  '((var x 0) (while (< (= x (+ x 1)) 21) (= x x)) (return x))
|#

#|
(define M-state-while
    (lambda (condition body state)
        (if (M-boolean condition state)
            (M-state-while condition body (M-state body state))
            state
        )
    )
)
#|


(define remove
  (lambda (x state) (remove-cps x state (lambda (v) v))))


    
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




