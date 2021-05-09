#lang racket

(require racket/match)
(provide (all-defined-out))

; λ: 'Ctrl + \'

;(class A () ((var x 1) (var y 2) (function m () ((return (funcall (dot this m2))))) (function m2 () ((return (+ x y)))))
;(class B (extends A) ((var y 22) (var z 3) (function m () ((return (funcall (dot super m))))) (function m2 () ((return (+ (+ x y) z))))))


 
    
(define instance-type
  (lambda (closure) (car closure)))

(define instance-closure?
  (lambda (expr) (and (eq? (length expr) 3) (eq? (caddr expr) 'instance-closure))))

(define super-class
  (λ (expression)
    (if (null? (caddr expression))
        '()
        (operand (caddr expression)))))

(define class-name
  (λ (expression) (cadr expression)))

(define class-body
  (λ (expression) (cadddr expression)))

(define class-closure-body
  (λ (expression) (cadr expression)))

(define class-closure-var-names cadr)
(define class-closure-var-exprs caddr)
(define class-closure-func-names cadddr)
(define class-closure-func-closures 
  (lambda (closure) (list-ref closure 4)))



#|
FUNCTION ANATOMY HELPERS
|#

; Gets the actual params of a function call
(define actualparams
  (λ (expression) (cddr expression)))

; Gets the function body
(define closure-body
  (λ (closure) (cadr closure)))

; Gets the function params out of the closure
(define closure-params
  (λ (closure) (car closure)))

; Gets the λ out of the closure
(define closure-state-function
  (λ (closure) (caddr closure)))

; Gets the λ-classlookup out of the closure
(define closure-class-lookup-function
  (λ (closure) (cadddr closure)))

; Checks whether a statement is a function
(define function?
  (λ (expression)
    (eq? 'function (operator expression))))

; Checks whether a statement is a function
(define static-function?
  (λ (expression)
    (eq? 'static-function (operator expression))))

(define static-function-closure?
  (λ (closure) (list-ref closure 4)))


#|
STATEMENT ANATOMY HELPERS
|#

; Gets the variable from an assignment statement
(define assignvar (λ (expression) (cadr expression)))

; Gets the expression to be assigned from an assignment statement
(define assignexp (λ (expression) (caddr expression)))

; Gets the body of an if statement/while loop
(define body caddr)

; Rrtrieves the condition of an if statement or a while loop
(define condition cadr)

; Gets the 'else' portion of an if statement
(define else-case 
  (λ (expr)
    (if (= 4 (length expr))
      (cadddr expr)
      '())))

; Gets first statement from a block of code
(define firststatement (λ (expression) (car expression)))

; Gets the body of a function
(define funcbody (λ (expression) (cadddr expression)))

; Gets the name of a function
(define funcname (λ (expression) (cadr expression)))

; Gets the name of a class
(define classname (λ (expression) (cadr expression)))

; Get the keyword of an expression
(define keyword (λ (expression) (car expression)))

; Gets the left operand of a binary expression 
(define leftoperand cadr)

; Gets the operand of a unary expression
(define operand (λ (expression) (cadr expression)))

; Gets the operator from any kind of expression
(define operator (λ (expression) (car expression)))

; Gets the parameters of a function signature
(define params (λ (expression) (caddr expression)))

; Gets every statement but the first from a block of code
(define reststatement (λ (expression) (cdr expression)))

; Gets the right operand of a binary expression
(define rightoperand caddr)

; Gets actual statements from a 'begin' expression
(define statements (λ (expression) (cdr expression)))

; Retrives the try block of a try-catch-finally
(define tryblock (λ (expression) (cadr expression)))

; Gets the entire 'catch' portion of try-catch-finally
(define caddy (λ (expression) (caddr expression)))

; Gets the catch block of try-catch-finally
(define catchblock (λ (expression) (caddr(caddy expression))))

; Gets the variable passed into the catch block
(define catchvar (λ (expression) (car (cadr (caddy expression)))))

; Gets the entire 'finally' portion of try-catch-finally
(define finny (λ (expression) (cdddr expression)))

; Gets the finally block of try-catch-finally
(define finallyblock
  (λ (expression)
    (if (hasfinally? expression)
        (cadr (car (finny expression)))
        '())))

; Gets the expression thrown in a 'throw' statement
(define throwvalue (λ (expression) (operand expression)))

(define newruntimetype cadr)

#|
EXPRESSION TYPE HELPERS
|#

; Determines whether an expression is arithmetic
(define arithmetic?
  (λ (expr)
    (cond
      ((number? expr) #t)
      ((member? (operator expr) '(+ - * / %)) #t)
      (else #f))))

; Determines whether an expression is an assignment
(define assign?
  (λ (expr) (eq? (operator expr) '=)))

; Determines whether an expression is a block of code
(define block?
  (λ (expr) (eq? (operator expr) 'begin)))

; Determines whether an expression is a boolean algebra expression
(define boolalg?
  (λ (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      ((member? (operator expr) '(&& || !)) #t)
      (else #f))))

; Determines whether an expression is a goto for breaking out of a while loop
(define break?
  (λ (expr) (eq? (operator expr) 'break)))

(define class?
  (λ (expr) (eq? (keyword expr) 'class)))

; Determines whether an expression is a goto for continuing the while loop
(define continue?
  (λ (expr) (eq? (operator expr) 'continue)))

; Determines whether an expression is a comparison
(define comparison?
  (λ (expr)
    (if (member? (operator expr) '(== != >= > <= <))
        #t
        #f)))

; Determines whether an expression is a declaration
(define declare?
  (λ (expr) (eq? (operator expr) 'var)))

(define dot?
  (λ (expr) (eq? (operator expr) 'dot)))

; Determines whether an expression is a function call
(define funcall?
  (λ (expression) (eq? (operator expression) 'funcall)))

; Determined whether a try-catch-finally has a 'finally' expression
(define hasfinally?
  (λ (expr) (not (null? (car (finny expr))))))
    
; Determines whether an expression is an if statement
(define if?
  (λ (expr) (eq? (operator expr) 'if)))

; Determines if an expression is constructing an object with 'new'
(define new?
  (λ (expr) (eq? (operator expr) 'new)))

; Determines whether an expression is a return statement
(define return?
  (λ (expr) (eq? (operator expr) 'return)))

; Determines whether an expression is any kind of statement
(define statement?
  (λ (expr) (list? (operator expr))))

; Determines whether an expression is a try-catch-finally
(define trycatch?
  (λ (expr) (eq? (operator expr) 'try)))

; Determines whether an expression is a throw statement
(define throw?
  (λ (expr) (eq? (operator expr) 'throw)))

; Checks if the given construct is a variable name
(define variable?
  (λ (x)
    (and (not (or (isbool? x) (number? x))) (atom? x))))

; Determines whether an expression is a while loop
(define while?
  (λ (expr) (eq? (operator expr) 'while)))



#|
STATE INTERFACING HELPER FUNCTIONS
|#

; Adds a layer to the front of the state
(define addlayer (λ (state) (cons (createnewlayer) state)))

; Checks if a given variable has been assigned a value
(define assigned?
  (λ (x state)
    (cond
      ((null? state) #f)
      ((assigned-layer? x (firstlayer state)) #t)
      (else (assigned? x (restlayers state))))))

; Checks if a given variable has a binding in a layer of the state
(define assigned-layer?
  (λ (x layer)
    (and (declared-layer? x layer) (not (null? (get-val-layer x layer))))))

; Abstraction of a layer of the state
(define createnewlayer (λ () '(()())))

; Abstraction of the entire state
(define createnewstate (λ () '((()()))))

; Checks if a given variable has been declared
(define declared?
  (λ (x state)
    (cond
      ((null? state) #f)
      ((declared-layer? x (firstlayer state)) #t)
      (else (declared? x (restlayers state))))))

; Checks if a given variable has been declared
(define declared-layer?
  (λ (x layer)
    (cond
      ((null? (vars layer)) #f)
      ((not (atom? x)) #f)
      ((eq? (firstvar layer) x) #t)
      (else (declared-layer? x (restpairs layer))))))

; Retrives the leftmost box of a layer
(define firstbox (λ (layer) (car (vals layer))))

; Gets the leftmost layer of the state
(define firstlayer (λ (state) (car state)))

; Gets the first value of the values sublist of the state
(define firstval (λ (state) (unbox (car (vals state)))))

; Gets the first variable of the variables sublist of the state
(define firstvar (λ (layer) (car (vars layer))))

; Entry point into get-box-cps
(define get-box
  (λ (x state) (get-box-cps x state (λ (v) v))))

; Retrives the box of a variable in the state
(define get-box-cps
  (λ (x state return)
    (if (null? state)
        (return '())
        (get-box-layer-cps x (firstlayer state)
                        (λ (v) (if (null? v) (get-box-cps x (restlayers state) return) (return v)))))))

; Given a variable, get its box
(define get-box-layer-cps
  (λ (x layer return)
    (cond
      ((or (null? (vars layer)) (null? (vals layer))) (return '()))
      ((eq? (firstvar layer) x) (return (firstbox layer)))
      (else (get-box-layer-cps x (restpairs layer) return)))))

; Returns the value in the state bound to a given variable
(define get-val
  (λ (x state)
    (cond
      ((declared-layer? x (firstlayer state)) (get-val-layer x (firstlayer state)))
      (else (get-val x (restlayers state))))))

; Returns the value in a layer of the state bound to a given variable
(define get-val-layer
  (λ (x layer)
    (cond
      ((layernull? layer) (error 'variable-not-found))
      ((eq? (firstvar layer) x) (firstval layer))
      (else (get-val-layer x (restpairs layer))))))

; Gets the last layer of the state
(define lastlayer
  (λ (state)
    (cond
      ((null? state) (error 'bad-argument))
      ((null? (restlayers state)) (firstlayer state))
      (else (lastlayer (restlayers state))))))

; Gets every layer of the state, save the rightmost one
(define lastlayers
  (λ (state)
    (drop-right state)))

; Checks if a layer of the state is empty
(define layernull?
  (λ (layer)
    (or (null? layer) (null? (vars layer)) (null? (vals layer)))))

; Removes the frontmost layer from the state
(define removelayer
  (λ (state)
    (if (declared-layer? 'main (firstlayer state))
        state
        (restlayers state))))

; Gets the tail of the state
(define restlayers (λ (state) (cdr state)))

; Gets the state without the first variable/value pair
(define restpairs
  (λ (layer)
    (cons (restvars layer) (cons (restvals layer) '()))))

; Gets all values (actually boxes) in the state expect the first
(define restvals (λ (layer) (cdr (vals layer))))

; Retrieve all variables in the state except the first
(define restvars (λ (layer) (cdr (vars layer))))

; Gets sublist of values from the state
(define vals (λ (layer) (cadr layer)))

; Retrieve sublist of variables from the state 
(define vars (λ (layer) (car layer)))



#|
BOOLEAN HELPER FUNCTIONS
|#

; Helper function for evaluating boolean expressions while making appropriate transformations with booltoname and nametobool
(define boolstringop
  (λ (x y f)
    (booltoname (f (nametobool x) (nametobool y)))))

; Same as boolstringop, but for unary operations
(define boolstringsingle
  (λ (x f)
    (booltoname (f (nametobool x)))))

; Converts booleans to the associated tokens 'true or 'false
(define booltoname
  (λ (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else (error 'not-a-bool)))))

; Checks if the given construct is an atomic boolean
(define isbool?
  (λ (a)
    (cond
      ((or (list? a) (number? a)) #f)
      ((or (eq? a 'true) (eq? a 'false)) #t)
      (else #f))))

; Converts the tokens 'true and 'false to their respective boolean equivalencies
(define nametobool
  (λ (a)
    (cond
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (error 'not-a-bool)))))

(define reverse
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (append (reverse (cdr lat)) (cons (car lat) '()))))))

#|
GENERIC HELPER FUNCTIONS
|#

; Returns true if the construct is an atom, and false otherwise
(define atom?
  (λ (x)
    (not (or (pair? x) (null? x)))))

; Returns true if the atom is a member of the list, and false otherwise
(define member?
  (λ (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))