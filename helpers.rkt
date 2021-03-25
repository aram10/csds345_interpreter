#lang racket

(provide (all-defined-out))

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

(define block?
  (lambda (expr) (eq? (operator expr) 'begin)))



#|
STATE INTERFACING HELPER FUNCTIONS
|#
; Checks if a given variable has been declared
(define declared?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((declared-layer? x (firstlayer state)) #t)
      (else (declared? x (restlayers state))))))

; Checks if a given variable has been declared
(define declared-layer?
  (lambda (x layer)
    (cond
      ((null? (vars layer)) #f)
      ((not (atom? x)) #f)
      ((eq? (firstvar layer) x) #t)
      (else (declared-layer? x (restpairs layer))))))

(define assigned?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((assigned-layer? x (firstlayer state)) #t)
      (else (assigned? x (restlayers state))))))

(define assigned-layer?
  (lambda (x layer)
    (and (declared-layer? x layer) (not (null? (get-val-layer x layer))))))

(define addlayer (lambda (state) (cons (createnewlayer) state)))

(define removelayer (lambda (state) (restlayers state)))

(define createnewlayer (lambda () '(()())))

(define createnewstate (lambda () '((()()))))

; Returns the value in the state bound to a given variable
(define get-val
  (lambda (x state)
    (cond
      ((declared-layer? x (firstlayer state)) (get-val-layer x (firstlayer state)))
      (else (get-val x (restlayers state))))))

(define get-val-layer
  (lambda (x layer)
    (cond
      ((layernull? layer) (error 'variable-not-found))
      ((eq? (firstvar layer) x) (firstval layer))
      (else (get-val-layer x (restpairs layer))))))

; Retrieves the first value of the values sublist of the state
(define firstval (lambda (state) (unbox (car (vals state)))))   

; Given a variable, get its box

(define get-box-layer
  (lambda (x layer)
    (cond
      ((or (null? (vars layer)) (null? (vals layer))) '())
      ((eq? (firstvar layer) x) (firstbox layer))
      (else (get-box-layer x (restpairs layer))))))

(define get-box-layer-cps
  (lambda (x layer return)
    (cond
      ((or (null? (vars layer)) (null? (vals layer))) (return '()))
      ((eq? (firstvar layer) x) (return (firstbox layer)))
      (else (get-box-layer-cps x (restpairs layer) return)))))


; state-> (((x y) (5 6)) ((z) (10))) -> ((()()) state2) -> '()
(define get-box-cps
  (lambda (x state return)
    (if (null? state)
        (return '())
        (get-box-layer-cps x (firstlayer state)
                        (lambda (v) (if (null? v) (get-box x (restlayers state) return) (return v)))))))

(define get-box
  (lambda (x state) (get-box-cps x state (lambda (v) v))))
      

(define firstbox (lambda (layer) (car (vals layer))))

(define firstlayer (lambda (state) (car state)))

(define layernull? (lambda (layer) (or (null? layer) (null? (vars layer)) (null? (vals layer)))))

(define restlayers cdr)

; Retrieves the first variable of the variables sublist of the state
(define firstvar (lambda (layer) (car (vars layer))))

; Retrieves the state without the first variable/value pair
(define restpairs
  (lambda (layer)
    (cons (restvars layer) (cons (restvals layer) '()))))

; Retrieve all values in the state expect the first
; THIS RETURNS BOXES NOW!!!!
(define restvals (lambda (layer) (cdr (vals layer))))

; Retrieve all variables in the state except the first
(define restvars (lambda (layer) (cdr (vars layer))))

; Retrieves sublist of values from the state
(define vals (lambda (layer) (cadr layer)))

; Retrieve sublist of variables from the state 
(define vars (lambda (layer) (car layer)))



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