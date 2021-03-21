#lang racket

(provide (all-defined-out))
(require "simpleParser.rkt" "helpers.rkt")

#|
CSDS 345 Simple Language Interpreter Project

Phan Trinh Ha
Alexander Rambasek
Stamatis Papadopoulos

3/8/2021
|#

; This is Alex - I'm adding this comment to test the Discord webhook testing12345

#|
INTERPRETER

Receives a list of statements in prefix notation from the parser, and passes them to M-state
|#

(define createnewstate (lambda () '(()())))

(define interpret
  (lambda (filename)
    (get-val 'return
             (call/cc
              (lambda (k)
                (M-state (parser filename) (createnewstate) k))))))


#|
M-VALUE EXPRESSIONS
|#

; Evaluates the result of a boolean algebra expression
(define M-boolean
  (lambda (expression state)
    (cond
      ((isbool? expression) expression)
      ((declared? expression state) (get-val expression state))
      ((comparison? expression) (M-compare expression state))
      ((eq? (operator expression) '&&) (boolstringop (M-value (leftoperand expression) state) (M-value (rightoperand expression) state) (lambda(x y) (and x y))))
      ((eq? (operator expression) '||) (boolstringop (M-value (leftoperand expression) state) (M-value (rightoperand expression) state) (lambda(x y) (or x y))))
      ((eq? (operator expression) '!) (boolstringsingle (M-value (leftoperand expression) state) (lambda (x) (not x))))
      (else (error 'bad-operator)))))

; Evaluates the result of a comparison between 2 arithmetic expressions
(define M-compare
  (lambda (expression state)
    (cond
      ((boolean? expression) (M-boolean expression state))
      ((eq? (operator expression) '==) (booltoname (= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '!=) (booltoname (not (= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))))
      ((eq? (operator expression) '>=) (booltoname (>= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '<=) (booltoname (<= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '>) (booltoname (> (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '<) (booltoname (< (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state))))
      (else (error 'bad-comparison)))))

; Evaluates the result of an arithmetic expression   
(define M-integer
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((assigned? expression state) (get-val expression state))
      ((declared? expression state) (error 'value-not-found))
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (= 3 (length expression))) (- (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (= 2 (length expression))) (* -1 (M-integer (operand expression) state)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-operator)))))

; General expression evaluater: entry point into M-compare, M-integer, M-boolean    
(define M-value
  (lambda (expression state)
    (cond
      ((declared? expression state) (get-val expression state))
      ((isbool? expression) expression)
      ((arithmetic? expression) (M-integer expression state))
      ((boolalg? expression) (M-boolean expression state))
      ((comparison? expression) (M-compare expression state))
      (else (error 'bad-argument)))))



#|
M-STATE EXPRESSIONS
|#

; Evaluates an assignment expression that may contain arithmetic/boolean expressions and updates the state
(define M-state-assign
  (lambda (expression state)
    (cond
      ((not (assign? expression)) (error 'not-an-assignment))
      ((not (declared? (assignvar expression) state)) (error 'variable-not-declared))
      ((declared? (assignexp expression) state) (assign (assignvar expression) (get-val (assignexp expression) state) state))
      ((and (variable? (assignexp expression)) (not (declared? (assignexp expression) state))) (error 'assigning-variable-not-declared))
      ((arithmetic? (assignexp expression)) (assign (assignvar expression) (M-integer (assignexp expression) state) state))
      ((boolalg? (assignexp expression)) (assign (assignvar expression) (M-boolean (assignexp expression) state) state))
      (else (error 'bad-assignment)))))

; Adds a variable with the given name and the value '() to the state
(define M-state-declare
  (lambda (expression state)
    (cond
      ((not (declare? expression)) (error 'not-a-declaration))
      ((eq? (length expression) 2) (declare (operand expression) state))
      ((eq? (length expression) 3) (add (leftoperand expression) (M-value (rightoperand expression) state) state))
      (else (error 'bad-declaration)))))

; Evaluates the result of an if statement and updates the state accordingly
(define M-state-if
  (lambda (expression state return-func)
    (if (nametobool (M-boolean (condition expression) state))
      (M-state (body expression) state return-func)
      (M-state (else-case expression) state return-func))))

#|
Entry point into all other M-state expressions: accepts an expression which may be a list of expressions, performs the
necessary updates to the state, and evaluates to the special variable 'return, once it is declared/assigned
|#
(define M-state
  (lambda (expression state return-func)
    (cond
      ((declared? 'return state) (return-func state))
      ((null? expression) state)
      ((declare? expression) (M-state-declare expression state))
      ((assign? expression) (M-state-assign expression state))
      ((while? expression) (M-state-while expression state return-func))
      ((if? expression) (M-state-if expression state return-func))
      ((return? expression) (return expression state))
      ((statement? expression) (M-state (cdr expression) (M-state (car expression) state return-func) return-func))
      (else error 'unsupported-statement)
    )))

; Evaluates the result of a while statement and updates the state accordingly
(define M-state-while
  (lambda (expression state return-func)
    (if (nametobool (M-boolean (condition expression) state))
      (M-state-while expression (M-state (body expression) state return-func) return-func)
      state)))



#|
M-STATE HELPER FUNCTIONS
|#

#|
  (define sideeffect
  (lambda (expression state)
    (cond 
      ((and (eq? 2 (length expression)) (assign? (operand expression))) M-state((operand expression) state))
      ((and (eq? 3 (length expression)) (and (assign? (leftoperand expression)) (not (assign? (rightoperand expression)))))
            (M-state(leftoperand expression) state))
      ((and (eq? 3 (length expression)) (and (assign? (rightoperand expression)) (not (assign? (leftoperand expression)))))
            (M-state (rightoperand expression) state))
      ((and (eq? 3 (length expression)) (and (assign? (leftoperand expression)) (assign? (rightoperand expression))))
            (M-state (rightoperand expression) (M-state (leftoperand expression) state)))
      (else state))))
|#
; Creates a new binding in the state with the given variable name and the given value; corresponds to a simultaneous declaration and assignment
(define add
  (lambda (x v state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons (box v) (vals state)) '()))))))

; Updates the binding of a declared variable in the state with the given value
(define assign
  (lambda (x v state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) state)
      (else (add x v (remove x state))))))

; Creates a new binding in the state with the given variable name and the value '()
(define declare
  (lambda (x state) (add x (box '()) state)))

; Entry point into remove-cps
(define remove
  (lambda (x state) (remove-cps x state (lambda (v) v))))

; Removes a binding from the state if it exists, in continuation passing style
(define remove-cps
  (lambda (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state)) (return (reststate state)))
      (else (remove-cps x (reststate state)
            (lambda (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))

; Evaluates a return expression, and creates a new binding in the state with the result and the special variable name 'return
(define return
  (lambda (expression state)
    (cond
      ((null? expression) (declare 'return state))
      (else (add 'return (M-value (operand expression) state) state)))))

(define update
  (lambda (x v state) (update-cps x v state (lambda (q) q))))
 
(define update-cps
  (lambda (x v state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state))
       (begin
         (set-box! (firstbox state) v)
         (return state)
       ))
      (else (update-cps x v (reststate state)
            (lambda (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))
      


