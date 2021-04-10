#lang racket

(provide (all-defined-out))

#|
FUNCTION ANATOMY HELPERS
|#

; (function fib (a) ((if (== a 0) (return 0) (if (== a 1) (return 1) (return (+ (funcall fib (- a 1)) (funcall fib (- a 2)))))))) (function main () ((return (funcall fib 10))))


(define function?
  (lambda (expression)
    (eq? 'function (operator expression))))

(define closure-state-function
  (lambda (closure) (caddr closure)))

(define closure-params
  (lambda (closure) (car closure)))

(define closure-body
  (lambda (closure) (cadr closure)))

(define actualparams
  (lambda (expression) (cddr expression)))




#|
STATEMENT ANATOMY HELPERS
|#



; Retrieves the variable from an assignment statement
(define assignvar (lambda (expression) (cadr expression)))

; Retrieves the expression to be assigned from an assignment statement
(define assignexp (lambda (expression) (caddr expression)))

; Retrieves the body of an if statement/while loop
(define body caddr)

; Rrtrieves the condition of an if statement or a while loop
(define condition cadr)

; Retrieves the 'else' portion of an if statement
(define else-case 
  (lambda (expr)
    (if (= 4 (length expr))
      (cadddr expr)
      '())))

; Retrieves first statement from a block of code
(define firststatement (lambda (expression) (car expression)))

(define funcbody (lambda (expression) (cadddr expression)))

(define funcname (lambda (expression) (cadr expression)))

; Retrieves the left operand of a binary expression 
(define leftoperand cadr)

; Retrieves the operand of a unary expression
(define operand (lambda (expression) (cadr expression)))

; Retrieves the operator from any kind of expression
(define operator (lambda (expression) (car expression)))

(define params (lambda (expression) (caddr expression)))

; Retrieves every statement but the first from a block of code
(define reststatement (lambda (expression) (cdr expression)))

; Retrieves the right operand of a binary expression
(define rightoperand caddr)

; Retrieves actual statements from a 'begin' expression
(define statements (lambda (expression) (cdr expression)))

; Retrives the try block of a try-catch-finally
(define tryblock (lambda (expression) (cadr expression)))

; Retrieves the entire 'catch' portion of try-catch-finally
(define caddy (lambda (expression) (caddr expression)))

; Retrieves the catch block of try-catch-finally
(define catchblock (lambda (expression) (caddr(caddy expression))))

; Retrieves the variable passed into the catch block
(define catchvar (lambda (expression) (car (cadr (caddy expression)))))

; Retrieves the entire 'finally' portion of try-catch-finally
(define finny (lambda (expression) (cdddr expression)))

; Retrieves the finally block of try-catch-finally
(define finallyblock
  (lambda (expression)
    (if (hasfinally? expression)
        (cadr (car (finny expression)))
        '())))

; Retrieves the expression thrown in a 'throw' statement
(define throwvalue (lambda (expression) (operand expression)))


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

; Determines whether an expression is a block of code
(define block?
  (lambda (expr) (eq? (operator expr) 'begin)))

; Determines whether an expression is a boolean algebra expression
(define boolalg?
  (lambda (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      ((member? (operator expr) '(&& || !)) #t)
      (else #f))))

; Determines whether an expression is a goto for breaking out of a while loop
(define break?
  (lambda (expr) (eq? (operator expr) 'break)))

; Determines whether an expression is a goto for continuing the while loop
(define continue?
  (lambda (expr) (eq? (operator expr) 'continue)))

; Determines whether an expression is a comparison
(define comparison?
  (lambda (expr)
    (if (member? (operator expr) '(== != >= > <= <))
        #t
        #f)))

; Determines whether an expression is a declaration
(define declare?
  (lambda (expr) (eq? (operator expr) 'var)))

(define funcall?
  (lambda (expression) (eq? (operator expression) 'funcall)))

; Determined whether a try-catch-finally has a 'finally' expression
(define hasfinally?
  (lambda (expr) (not (null? (car (finny expr))))))
    
; Determines whether an expression is an if statement
(define if?
  (lambda (expr) (eq? (operator expr) 'if)))

; Determines whether an expression is a return statement
(define return?
  (lambda (expr) (eq? (operator expr) 'return)))

; Determines whether an expression is any kind of statement
(define statement?
  (lambda (expr) (list? (operator expr))))

; Determines whether an expression is a try-catch-finally
(define trycatch?
  (lambda (expr) (eq? (operator expr) 'try)))

; Determines whether an expression is a throw statement
(define throw?
  (lambda (expr) (eq? (operator expr) 'throw)))

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

; Adds a layer to the front of the state
(define addlayer (lambda (state) (cons (createnewlayer) state)))

; Checks if a given variable has been assigned a value
(define assigned?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((assigned-layer? x (firstlayer state)) #t)
      (else (assigned? x (restlayers state))))))

; Checks if a given variable has a binding in a layer of the state
(define assigned-layer?
  (lambda (x layer)
    (and (declared-layer? x layer) (not (null? (get-val-layer x layer))))))

; Abstraction of a layer of the state
(define createnewlayer (lambda () '(()())))

; Abstraction of the entire state
(define createnewstate (lambda () '((()()))))

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

; Retrives the leftmost box of a layer
(define firstbox (lambda (layer) (car (vals layer))))

; Retrieves the leftmost layer of the state
(define firstlayer (lambda (state) (car state)))

; Retrieves the first value of the values sublist of the state
(define firstval (lambda (state) (unbox (car (vals state)))))

; Retrieves the first variable of the variables sublist of the state
(define firstvar (lambda (layer) (car (vars layer))))

; Entry point into get-box-cps
(define get-box
  (lambda (x state) (get-box-cps x state (lambda (v) v))))

; Retrives the box of a variable in the state
(define get-box-cps
  (lambda (x state return)
    (if (null? state)
        (return '())
        (get-box-layer-cps x (firstlayer state)
                        (lambda (v) (if (null? v) (get-box-cps x (restlayers state) return) (return v)))))))

; Given a variable, get its box
(define get-box-layer-cps
  (lambda (x layer return)
    (cond
      ((or (null? (vars layer)) (null? (vals layer))) (return '()))
      ((eq? (firstvar layer) x) (return (firstbox layer)))
      (else (get-box-layer-cps x (restpairs layer) return)))))

; Returns the value in the state bound to a given variable
(define get-val
  (lambda (x state)
    (cond
      ((declared-layer? x (firstlayer state)) (get-val-layer x (firstlayer state)))
      (else (get-val x (restlayers state))))))

; Returns the value in a layer of the state bound to a given variable
(define get-val-layer
  (lambda (x layer)
    (cond
      ((layernull? layer) (error 'variable-not-found))
      ((eq? (firstvar layer) x) (firstval layer))
      (else (get-val-layer x (restpairs layer))))))

; Retrieves the last layer of the state
(define lastlayer
  (lambda (state)
    (cond
      ((null? state) (error 'bad-argument))
      ((null? (restlayers state)) (firstlayer state))
      (else (lastlayer (restlayers state))))))

; Retrieves every layer of the state, save the rightmost one
(define lastlayers
  (lambda (state)
    (drop-right state)))

; Checks if a layer of the state is empty
(define layernull?
  (lambda (layer)
    (or (null? layer) (null? (vars layer)) (null? (vals layer)))))

; Removes the frontmost layer from the state
(define removelayer (lambda (state) (restlayers state)))

; Retrieves the tail of the state
(define restlayers (lambda (state) (cdr state)))

; Retrieves the state without the first variable/value pair
(define restpairs
  (lambda (layer)
    (cons (restvars layer) (cons (restvals layer) '()))))

; Retrieves all values (actually boxes) in the state expect the first
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