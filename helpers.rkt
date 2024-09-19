#lang racket

(require racket/match)
;(require errortrace)
(provide (all-defined-out))

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

; Creates a new binding in the state with the given variable name and the given value; corresponds to a simultaneous declaration and assignment
(define add
  (λ (x v state)
    (cond
      ((member? x (vars (firstlayer state))) (error 'bad-declaration))
      (else (cons (add-to-layer x v (firstlayer state)) (restlayers state))))))

; Creates a variable binding in a particular layer of the state
(define add-to-layer
  (λ (x v layer)
    (cond
      ((member? x (vars layer)) (error 'variable-exists))
      (else (cons (cons x (vars layer)) (cons (cons (box v) (vals layer)) '()))))))

; Updates the binding of a declared variable in the state with the given value
(define assign
  (λ (x v state) (begin (set-box! (get-box x state) v) state)))

; Gets the expression to be assigned from an assignment statement
(define assignexp (λ (expression) (caddr expression)))

; Gets the variable from an assignment statement
(define assignvar (λ (expression) (cadr expression)))

; Gets the body of an if statement/while loop
(define body caddr)

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

; Abstraction of a layer of the state
(define createnewlayer (λ () '(()())))

; Abstraction of the entire state
(define createnewstate (λ () '((()()))))

; Gets the entire 'catch' portion of try-catch-finally
(define caddy (λ (expression) (caddr expression)))

; Gets the catch block of try-catch-finally
(define catchblock (λ (expression) (caddr(caddy expression))))

; Gets the variable passed into the catch block
(define catchvar (λ (expression) (car (cadr (caddy expression)))))

; Rrtrieves the condition of an if statement or a while loop
(define condition cadr)

; Returns the portion of the state that contains x
(define cut-until-layer
  (λ (x state)
    (if (has-var-layer x (firstlayer state))
        state
        (cut-until-layer x (restlayers state)))))

; Creates a new binding in the state with the given variable name and the value '()
(define declare
  (λ (x state) (add x '() state)))

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
                                                                
; Gets the 'else' portion of an if statement
(define else-case 
  (λ (expr)
    (if (= 4 (length expr))
      (cadddr expr)
      '())))

; Gets the finally block of try-catch-finally
(define finallyblock
  (λ (expression)
    (if (hasfinally? expression)
        (cadr (car (finny expression)))
        '())))

; Gets the entire 'finally' portion of try-catch-finally
(define finny (λ (expression) (cdddr expression)))

; Gets first statement from a block of code
(define firststatement (λ (expression) (car expression)))

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
      ((null? state) (begin (println x) (error 'var-not-found)))
      ((declared-layer? x (firstlayer state)) (get-val-layer x (firstlayer state)))
      (else (get-val x (restlayers state))))))

; Returns the value in a layer of the state bound to a given variable
(define get-val-layer
  (λ (x layer)
    (cond
      ((layernull? layer) (error 'variable-not-found))
      ((eq? (firstvar layer) x) (firstval layer))
      (else (get-val-layer x (restpairs layer))))))



; Determined whether a try-catch-finally has a 'finally' expression
(define hasfinally?
  (λ (expr) (not (null? (car (finny expr))))))

(define has-var
  (λ (x state)
    (cond
      ((null? state) #f)
      ((declared-layer? x (firstlayer state)) (has-var-layer x (firstlayer state)))
      (else (has-var x (restlayers state))))))

; Checks whether or not a binding is present in a specified layer
(define has-var-layer
  (λ (x layer)
    (cond
      ((layernull? layer) #f)
      ((eq? (firstvar layer) x) #t)
      (else (has-var-layer x (restpairs layer))))))

; Get the keyword of an expression
(define keyword (λ (expression) (car expression)))

; Gets the left operand of a binary expression 
(define leftoperand cadr)

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

; Returns true if the atom is a member of the list, and false otherwise
(define member?
  (λ (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

; Converts the tokens 'true and 'false to their respective boolean equivalencies
(define nametobool
  (λ (a)
    (cond
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (error 'not-a-bool)))))

; Gets the operand of a unary expression
(define operand (λ (expression) (cadr expression)))

; Gets the operator from any kind of expression
(define operator (λ (expression) (car expression)))

; Gets the parameters of a function signature
(define params (λ (expression) (caddr expression)))

; Entry point into remove-cps
(define remove-layer
  (λ (x state) (remove-layer-cps x state (λ (v) v))))

; Removes a binding from the state if it exists, in continuation passing style
(define remove-layer-cps
  (λ (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state)) (return (restpairs state)))
      (else (remove-layer-cps x (restpairs state)
            (λ (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))

; Gets every statement but the first from a block of code
(define reststatement (λ (expression) (cdr expression)))

; Gets the right operand of a binary expression
(define rightoperand caddr)

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

(define reverse
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (append (reverse (cdr lat)) (cons (car lat) '()))))))

; Gets actual statements from a 'begin' expression
(define statements (λ (expression) (cdr expression)))

; Retrives the try block of a try-catch-finally
(define tryblock (λ (expression) (cadr expression)))

; Gets the expression thrown in a 'throw' statement
(define throwvalue (λ (expression) (operand expression)))

; Entry point into update-cps
(define update
  (λ (x v state) (update-cps x v state (λ (q) q))))


(define update-instance-field
  (λ (obj fieldname new-val compiletype state)
    ; step 1: find class closure
    ; step 2: instance (A (box val, box val)) class: (() (name1, name2) (expr1, expr2) ....)
    (letrec
      [
         (class-closure (get-val compiletype state))
         (field-index (class-field-index compiletype fieldname state))
         (field-value-box (list-ref (instance-values obj) field-index))
      ]
      (begin
        (set-box! field-value-box new-val)
        state))))
        

(define get-instance-field
  (λ (obj fieldname compiletype state)
    (letrec
      [
         (class-closure (get-val compiletype state))
         (field-index (class-field-index compiletype fieldname state))
         (field-value-box (list-ref (instance-values obj) field-index))
      ]
      (unbox field-value-box))))


; Updates the binding of the given variable in the state
(define update-cps
  (λ (x v state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state))
       (begin
         (set-box! (firstbox state) v)
         (return state)
       ))
      (else (update-cps x v (restpairs state)
            (λ (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))

; Updates the binding of a declared variable in a single layer of the state
(define update-layer
 (λ (x v layer)
   (if (eq? x (firstvar layer))
       (set-box! (firstval layer) v)
       (update-layer x v (restpairs layer)))))

; Gets sublist of values from the state
(define vals (λ (layer) (cadr layer)))

; Retrieve sublist of variables from the state 
(define vars (λ (layer) (car layer)))


#|
owo what's this
|#
(define arithmetic?
  (λ (expr)
    (cond
      ((number? expr) #t)
      ((member? (operator expr) '(+ - * / %)) #t)
      (else #f))))

(define assign?
  (λ (expr) (eq? (operator expr) '=)))

(define atom?
  (λ (x)
    (not (or (pair? x) (null? x)))))

(define block?
  (λ (expr) (eq? (operator expr) 'begin)))

(define bool?
  (λ (a)
    (cond
      ((or (list? a) (number? a)) #f)
      ((or (eq? a 'true) (eq? a 'false)) #t)
      (else #f))))

(define boolalg?
  (λ (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      ((member? (operator expr) '(&& || !)) #t)
      (else #f))))

(define break?
  (λ (expr) (eq? (operator expr) 'break)))

(define class?
  (λ (expr) (eq? (keyword expr) 'class)))

(define continue?
  (λ (expr) (eq? (operator expr) 'continue)))

(define comparison?
  (λ (expr)
    (if (member? (operator expr) '(== != >= > <= <))
        #t
        #f)))

(define declare?
  (λ (expr) (eq? (operator expr) 'var)))

(define dot?
  (λ (expr) (and (list? expr) (eq? (operator expr) 'dot))))

(define funcall?
  (λ (expression) (eq? (operator expression) 'funcall)))

(define function?
  (λ (expression)
    (eq? 'function (operator expression))))

(define if?
  (λ (expr) (eq? (operator expr) 'if)))

(define instance-closure?
  (lambda (expr) (and (eq? (length expr) 3) (eq? (caddr expr) 'instance-closure))))

(define new?
  (λ (expr) (eq? (operator expr) 'new)))

(define return?
  (λ (expr) (eq? (operator expr) 'return)))

(define statement?
  (λ (expr) (list? (operator expr))))

(define static-function?
  (λ (expression)
    (eq? 'static-function (operator expression))))

(define static-function-closure?
  (λ (closure) (list-ref closure 4)))

(define throw?
  (λ (expr) (eq? (operator expr) 'throw)))

(define trycatch?
  (λ (expr) (eq? (operator expr) 'try)))

(define variable?
  (λ (x)
    (and (not (or (bool? x) (number? x))) (atom? x))))

(define while?
  (λ (expr) (eq? (operator expr) 'while)))



#|
MODULAR HELPERS
|#

(define class-body
  (λ (expression) (cadddr expression)))

(define class-closure-body
  (λ (expression) (cadr expression)))

(define class-closure-func-names cadddr)

(define class-closure-var-names cadr)

(define class-closure-var-exprs caddr)

(define class-closure-func-closures 
  (lambda (closure) (list-ref closure 4)))

; helper to, given a class and a var name, returns the index that the var name is stored at
(define class-field-index
  (λ (cname field state)
    (- (- (class-chain-length cname state) 
        (class-field-index-helper cname (class-closure-var-names (get-val cname state)) field 0 state)) 1)))

(define class-field-index-helper
  (λ (curr-class class-vars field acc state)
    (cond
      ;[(null? ) (begin (println (list curr-class class-vars field acc)) (error 'hit-top-level))]
      [(null? class-vars) (class-field-index-helper (get-super-class (get-val curr-class state)) 
                                                    (class-closure-var-names (get-super-class-closure curr-class state))
                                                    field acc state)]
      [(eq? field (car class-vars)) acc]
      [else (class-field-index-helper curr-class (cdr class-vars) field (+ 1 acc) state)])))

(define class-chain-length
  (λ (curr-class state)
    (if (null? (get-super-class (get-val curr-class state)))
        (length (class-closure-var-names (get-val curr-class state)))
        (+ (length (class-closure-var-names (get-val curr-class state))) (class-chain-length (get-super-class (get-val curr-class state)) state)))))
      

(define class-name
  (λ (expression) (cadr expression)))

; Get super class from closure
(define get-super-class
  (λ (closure) (car closure)))

(define get-super-class-closure
  (lambda (classname state) (get-val (get-super-class (get-val classname state)) state)))

(define instance-type
  (lambda (closure) (car closure)))

(define instance-values
  (lambda (closure) (cadr closure)))

; Checks if a variable is a member of a class
(define local-var?
  (λ (classname x state)
    (or (member? x (class-closure-var-names (get-val classname state)))
        (and
         (not (null? (get-super-class (get-val classname state))))
         (local-var? (get-super-class (get-val classname state)) x state)))))
    

(define newruntimetype cadr)

; parse class property by inserting a function that takes a class body and a state, returns a layer
(define parse-class-property
  (λ (body parser-func) (parser-func body (createnewstate))))

(define super-class
  (λ (expression)
    (if (null? (caddr expression))
        '()
        (operand (caddr expression)))))


#|
FUNCTIONAL HELPERS
|#

; Gets the actual params of a function call
(define actualparams
  (λ (expression) (cddr expression)))

; Gets the function body
(define closure-body
  (λ (closure) (cadr closure)))

; Gets the λ-classlookup out of the closure
(define closure-class-lookup-function
  (λ (closure) (cadddr closure)))

; Gets the function params out of the closure
(define closure-params
  (λ (closure) (car closure)))

; Gets the λ out of the closure
(define closure-state-function
  (λ (closure) (caddr closure)))

; Function closure 3-tuple (params, body, λ(state) -> state)
(define create-closure
  (λ (name params body state cls is_static) (list params body (λ (v) (cut-until-layer name v)) (lambda (s) cls) is_static)))

; Gets the body of a function
(define funcbody (λ (expression) (cadddr expression)))

; Gets the name of a function
(define funcname (λ (expression) (cadr expression)))