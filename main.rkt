#lang racket

(provide (all-defined-out))
(require "simpleParser.rkt")

#|
CSDS 345 Simple Language Interpreter Project

Phan Trinh Ha
Alexander Rambasek
Stamatis Papadopoulos

3/8/2021
|#



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
      ((declared? expression (vars state)) (get-val expression state))
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
      ((declared? expression (vars state)) (error 'value-not-found))
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
      ((declared? expression (vars state)) (get-val expression state))
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
      ((not (declared? (assignvar expression) (vars state))) (error 'variable-not-declared))
      ((declared? (assignexp expression) (vars state)) (assign (assignvar expression) (get-val (assignexp expression) state) state))
      ((and (variable? (assignexp expression)) (not (declared? (assignexp expression) (vars state)))) (error 'assigning-variable-not-declared))
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
      ((declared? 'return (vars state)) (return-func state))
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
       (cons (cons x (vars state)) (cons (cons v (vals state)) '()))))))

; Updates the binding of a declared variable in the state with the given value
(define assign
  (lambda (x v state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) state)
      (else (add x v (remove x state))))))

; Creates a new binding in the state with the given variable name and the value '()
(define declare
  (lambda (x state) (add x '() state)))

; Entry point into remove-cps
(define remove
  (lambda (x state) (remove-cps x state (lambda (v) v))))

; Removes a binding from the state if it exists, in continuation passing style
(define remove-cps
  (lambda (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar (vars state))) (return (cons (restvars (vars state)) (cons (restvals (vals state)) '()))))
      (else (remove-cps x (cons (restvars (vars state)) (cons (restvals (vals state)) '())) 
            (lambda (s) (return (cons (cons (firstvar (vars state)) (vars s)) (cons (cons (firstval (vals state)) (vals s)) '())))))))))

; Evaluates a return expression, and creates a new binding in the state with the result and the special variable name 'return
(define return
  (lambda (expression state)
    (cond
      ((null? expression) (declare 'return state))
      (else (add 'return (M-value (operand expression) state) state)))))



#|
STATEMENT ANATOMY HELPERS
|#

; Retrieves the operand of a unary expression
(define operand (lambda (expression) (cadr expression)))

; Retrieves the variable from an assignment statement
(define assignvar (lambda (expression) (cadr expression)))

; Retrieves the expression to be assigned from an assignment statement
(define assignexp (lambda (expression) (caddr expression)))

; Retrieves the operator from any kind of expression
(define operator (lambda (expression) (car expression)))

; Retrieves the left operand of a binary expression 
(define leftoperand cadr)

; Retrieves the right operand of a binary expression
(define rightoperand caddr)

; Rrtrieves the condition of an if statement or a while loop
(define condition cadr)

; Retrieves the body of an if statement or a while loop
(define body caddr)

; Retrieves the 'else' portion of an if statement
(define else-case 
  (lambda (expr)
    (if (= 4 (length expr))
      (cadddr expr)
      '())))



#|
EXPRESSION TYPE HELPERS
|#

; Determines whether an expression is arithmetic
(define arithmetic?
  (lambda (expr)
    (cond
      ((number? expr) #t)
      ((member? (operator expr) '(+ - * / %)) #t)
      (else #f))))

; Determines whether an expression is an assignment
(define assign?
  (lambda (expr) (eq? (operator expr) '=)))

; Determines whether an expression is a boolean algebra expression
(define boolalg?
  (lambda (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      ((member? (operator expr) '(&& || !)) #t)
      (else #f))))

; Determines whether an expression is a comparison
(define comparison?
  (lambda (expr)
    (if (member? (operator expr) '(== != >= > <= <))
        #t
        #f)))

; Determines whether an expression is a declaration
(define declare?
  (lambda (expr) (eq? (operator expr) 'var)))

; Determines whether an expression is an if statement
(define if?
  (lambda (expr) (eq? (operator expr) 'if)))

; Determines whether an expression is a return statement
(define return?
  (lambda (expr) (eq? (operator expr) 'return)))

; Determines whether an expression is any kind of statement
(define statement?
  (lambda (expr) (list? (operator expr))))

; Checks if the given construct is a variable name
(define variable?
  (lambda (x)
    (and (not (or (isbool? x) (number? x))) (atom? x))))

; Determines whether an expression is a while loop
(define while?
  (lambda (expr) (eq? (operator expr) 'while)))



#|
STATE INTERFACING HELPER FUNCTIONS
|#

; Checks if a given variable has been declared
(define declared?
  (lambda (x vars)
    (cond
      ((null? vars) #f)
      ((not (atom? x)) #f)
      ((eq? (firstvar vars) x) #t)
      (else (declared? x (cdr vars))))))

(define assigned?
  (lambda (x state) (and (declared? x (vars state)) (not (null? (get-val x state))))))
; Returns the value in the state bound to a given variable
(define get-val
  (lambda (x state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) '())
      ((eq? (firstvar (vars state)) x) (firstval (vals state)))
      (else (get-val x (cons (restvars (vars state)) (cons (restvals (vals state)) '())))))))

; Retrieves the first value of the values sublist of the state
(define firstval (lambda (vals) (car vals)))

; Retrieves the first variable of the variables sublist of the state
(define firstvar (lambda (vars) (car vars)))

; Retrieve all values in the state expect the first
(define restvals (lambda (vals) (cdr vals)))

; Retrieve all variables in the state except the first
(define restvars (lambda (vars) (cdr vars)))

; Retrieves sublist of values from the state
(define vals (lambda (state) (cadr state)))

; Retrieve sublist of variables from the state 
(define vars (lambda (state) (car state)))



#|
BOOLEAN HELPER FUNCTIONS
|#

; Helper function for evaluating boolean expressions while making appropriate transformations with booltoname and nametobool
(define boolstringop
  (lambda (x y f)
    (booltoname (f (nametobool x) (nametobool y)))))

; Same as boolstringop, but for unary operations
(define boolstringsingle
  (lambda (x f)
    (booltoname (f (nametobool x)))))

; Converts booleans to the associated tokens 'true or 'false
(define booltoname
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else (error 'not-a-bool)))))

; Checks if the given construct is an atomic boolean
(define isbool?
  (lambda (a)
    (cond
      ((or (list? a) (number? a)) #f)
      ((or (eq? a 'true) (eq? a 'false)) #t)
      (else #f))))

; Converts the tokens 'true and 'false to their respective boolean equivalencies
(define nametobool
  (lambda (a)
    (cond
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (error 'not-a-bool)))))



#|
GENERIC HELPER FUNCTIONS
|#

; Returns true if the construct is an atom, and false otherwise
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Returns true if the atom is a member of the list, and false otherwise
(define member?
  (lambda (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))
