#lang racket

(provide (all-defined-out))
(require racket/match)
(require "classParser.rkt" "helpers.rkt")

#|
CSDS 345 Simple Language Interpreter Project

Phan Trinh Ha
Stamatis Papadopoulos
Alexander Rambasek

4/12/2021
|#


#|
INTERPRETER

Receives a list of statements in prefix notation from the parser, and passes them to M-state
|#


(define interpret2
  (λ (filename)
    (M-state-init (parser filename) (createnewstate) (λ (v) v))))


(define interpret
  (λ (filename classname)
    (letrec
        ((globalstate (M-state-init (parser filename) (createnewstate) (λ (v) v) )))
      (M-state '(dot classname main) globalstate (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception))))))

#|
M-VALUE EXPRESSIONS
|#

; Evaluates the result of a boolean algebra expression
(define M-boolean
  (λ (expression state return-func next break continue throw)
    (cond
      ((isbool? expression) expression)
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((declared? expression state) (get-val expression state))
      ((comparison? expression) (M-compare expression state return-func next break continue throw))
      ((eq? (operator expression) '&&) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw)
                                                     (λ(x y) (and x y))))
      ((eq? (operator expression) '||) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw)
                                                     (λ(x y) (or x y))))
      ((eq? (operator expression) '!) (boolstringsingle (M-value (leftoperand expression) state return-func next break continue throw)
                                                        (λ (x) (not x))))
      (else (error 'bad-operator)))))

; Evaluates the result of a comparison between 2 arithmetic expressions
(define M-compare
  (λ (expression state return-func next break continue throw)
    (cond
      ((boolean? expression) (M-boolean expression state return-func next break continue throw))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((eq? (operator expression) '==) (booltoname (= (M-value (leftoperand expression) state return-func next break continue throw)
                                                      (M-value (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '!=) (booltoname (not (= (M-value (leftoperand expression) state return-func next break continue throw)
                                                           (M-value (rightoperand expression) state return-func next break continue throw)))))
      ((eq? (operator expression) '>=) (booltoname (>= (M-value (leftoperand expression) state return-func next break continue throw)
                                                       (M-value (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '<=) (booltoname (<= (M-value (leftoperand expression) state return-func next break continue throw)
                                                       (M-value (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '>) (booltoname (> (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '<) (booltoname (< (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw))))
      (else (error 'bad-comparison)))))

; Evaluates the result of an arithmetic expression   
(define M-integer
  (λ (expression state return-func next break continue throw)
    (cond
      ((number? expression) expression)
      ((assigned? expression state) (get-val expression state))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((declared? expression state) (error 'value-not-found))
      ((eq? (operator expression) '+) (+ (M-value (leftoperand expression) state return-func next break continue throw)
                                         (M-value (rightoperand expression) state return-func next break continue throw)))
      ((and (eq? (operator expression) '-) (= 3 (length expression)))
       (- (M-value (leftoperand expression) state return-func next break continue throw) (M-value (rightoperand expression) state return-func next break continue throw)))
      ((and (eq? (operator expression) '-) (= 2 (length expression)))
       (* -1 (M-integer (operand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '*) (* (M-value (leftoperand expression) state return-func next break continue throw)
                                         (M-value (rightoperand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '/) (quotient (M-value (leftoperand expression) state return-func next break continue throw)
                                                (M-value (rightoperand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '%) (remainder (M-value (leftoperand expression) state return-func next break continue throw)
                                                 (M-value (rightoperand expression) state return-func next break continue throw)))
      (else (error 'bad-operator)))))

; General expression evaluater: entry point into M-compare, M-integer, M-boolean    
(define M-value
  (λ (expression state return-func next break continue throw)
    (cond
      ((declared? expression state) (get-val expression state))
      ((isbool? expression) expression)
      ((arithmetic? expression) (M-integer expression state return-func next break continue throw))
      ((boolalg? expression) (M-boolean expression state return-func next break continue throw))
      ((comparison? expression) (M-compare expression state return-func next break continue throw))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((new? expression) (create-instance-closure)) ;TODO
      (else (error 'bad-argument)))))

#|
M-STATE EXPRESSIONS
|#

#|
Entry point into all other M-state expressions: accepts an expression which may be a list of expressions, performs the
necessary updates to the state, and evaluates to the special variable 'return, once it is declared/assigned
|#
(define M-state
  (λ (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      ((return? expression) (return-func (M-value (operand expression) state return-func next break continue throw) state))
      ((declare? expression) (call/cc (λ (throw-cc) (next (M-state-declare expression state return-func next break continue (λ (e s) (throw-cc (throw e s))))))))
      ((assign? expression) (call/cc (λ (throw-cc) (next (M-state-assign expression state return-func next break continue (λ (e s) (throw-cc (throw e s))))))))
      ((while? expression) (call/cc (λ (k)
                                      (M-state-while expression state return-func next k continue throw))))
      ((if? expression) (M-state-if expression state return-func next break continue throw))
      ((statement? expression) (M-state (car expression) state return-func (λ (s)
                                                                             (M-state (cdr expression) s return-func next break continue throw)) break continue throw))
      ((block? expression) (M-state-block (statements expression) (addlayer state) return-func (λ (s)
                                                                                                 (next (removelayer s))) (λ (s)
                                                                                                                           (break (removelayer s))) (λ (s)
                                                                                                                                                      (continue (removelayer s))) throw))
      ((trycatch? expression) (M-state-try-catch-finally expression (addlayer state) return-func (λ (s)
                                                                                                   (next (removelayer s))) (λ (s)
                                                                                                                             (break (removelayer s))) (λ (s)
                                                                                                                                                        (continue (removelayer s))) throw))
      ((throw? expression) (throw (M-value (throwvalue expression) state return-func next break continue throw) state))
      ((break? expression) (break state))
      ((continue? expression) (continue state))
      ((function? expression) (next (M-state-function expression state)))
      ((funcall? expression) (M-state-funcall expression state return-func next break continue throw))
      ((dot? expression) -1) 
      (else error 'unsupported-statement)
    )))



; The "vanguard" of the interpreter: initial pass-through to bind global variables and functions
(define M-state-init
  (λ (expression state next)
    (cond
      ((null? expression) (next state))
      ((statement? expression) (M-state-init (car expression) state (λ (s) (M-state-init (cdr expression) s next))))
      ((class? expression) (next (M-state-class expression state)))
      (else (error 'bad-class-declaration)))))

; @author Alex (let me know if this idea sux)
; Creates the bindings for a single class
(define M-class-init
  (λ (expression state next)
    (cond
      ((null? expression) (next state))
      ((statement? expression) (M-class-init (car expression) state (λ (s) (M-class-init (cdr expression) s next))))
      ((declare? expression) (next (class-declaration expression state)))
      ((function? expression) (next (M-state-function expression state)))
      ((static-function? expression) (next (M-state-function expression state)))
      (else (error 'bad)))))


; Like M-state-declare, except that it doesn't evaluate the expression if it's a declare + assign
(define class-declaration
  (λ (expression state)
    (match expression
      ;((not (declare? expression)) (error 'not-a-declaration))
      ((list 'var name) (declare (operand expression) state))
      ((list 'var name expr) (add (leftoperand expression) (rightoperand expression) state))
      (_ (error 'bad-declaration)))))

; Generate state with class closure
(define M-state-class
  (λ (expression state) 
    (add (classname expression) (create-class-closure (super-class expression) (class-body expression)) state)))
#|
(define M-state-instance
  (λ (closure state)
    (letrec
        ((body (class-closure-body closure))
         (newstate (M-state 
|#

(define M-state-instance
  (λ (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      ((statement? expression) (M-state-instance (car expression) state return-func (λ (s)
                                                                             (M-state-instance (cdr expression) s return-func next break continue throw)) break continue throw))
      ((declare? expression) (next (M-state-declare expression state return-func next break continue throw)))
      ((assign? expression) (next (M-state-assign expression state return-func next break continue throw)))
      ((function? expression) (next (M-state-function expression state)))
      ((static-function? expression) (next (M-state-function expression state)))
      (else (error 'm-state-instance)))))

; Evaluates an assignment expression that may contain arithmetic/boolean expressions and updates the state
(define M-state-assign
  (λ (expression state return-func next break continue throw)
    (cond
      ((not (assign? expression)) (error 'not-an-assignment))
      ((not (declared? (assignvar expression) state)) (error 'variable-not-declared))
      ((declared? (assignexp expression) state) (assign (assignvar expression) (get-val (assignexp expression) state) state))
      ((and (variable? (assignexp expression)) (not (declared? (assignexp expression) state))) (error 'assigning-variable-not-declared))
      ((arithmetic? (assignexp expression)) (assign (assignvar expression) (M-integer (assignexp expression) state return-func next break continue throw) state))
      ((boolalg? (assignexp expression)) (assign (assignvar expression) (M-boolean (assignexp expression) state return-func next break continue throw) state))
      ((funcall? (assignexp expression)) (assign (assignvar expression) (M-value-function (assignexp expression) state return-func next break continue throw) state))
      (else (error 'bad-assignment)))))

; Evaluates the result of executing a block of code
(define M-state-block
  (λ (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      (else (M-state (firststatement expression) state return-func (λ (s)
                                                                     (M-state-block (reststatement expression) s return-func next break continue throw)) break continue throw)))))

; Adds a variable with the given name and the value '() to the state
(define M-state-declare
  (λ (expression state return-func next break continue throw)
    (cond
      ((not (declare? expression)) (error 'not-a-declaration))
      ((eq? (length expression) 2) (declare (operand expression) state))
      ((eq? (length expression) 3) (add (leftoperand expression) (M-value (rightoperand expression) state return-func next break continue throw) state))
      (else (error 'bad-declaration)))))


; Evaluates the result of an if statement and updates the state accordingly
(define M-state-if
  (λ (expression state return-func next break continue throw)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw))
      (M-state (body expression) state return-func next break continue throw)
      (M-state (else-case expression) state return-func next break continue throw))))

; Evaluates the result of a try-catch/try-catch-finally block
(define M-state-try-catch-finally
  (λ (expression state return-func next break continue throw)
    (M-state (tryblock expression) state
             ;return-func
             (λ (v s) (M-state (finallyblock expression) s return-func (λ (s1) (return-func v s1)) break continue throw))
             ;next
             (λ (s) (M-state (finallyblock expression) s return-func next break continue throw))
             ;break
             (λ (s) (M-state (finallyblock expression) s return-func break break continue throw))
             ;continue
             (λ (s) (M-state (finallyblock expression) s return-func continue break continue throw))
             ;throw
             (λ (e s) (M-state (catchblock expression) (add (catchvar expression) e s)
                                     ;return-func
                                     (λ (v1 s1) (M-state (finallyblock expression) s1 return-func (λ (s2) (return-func v1 s2)) break continue throw))
                                     ;next
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func next break continue throw))
                                     ;break
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func break break continue throw))
                                     ;continue
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func continue break continue throw))
                                     ;newThrow
                                     (λ (e1 s1) (M-state (finallyblock expression) s1 return-func (λ (s2) (throw e1 s2)) break continue throw)))))))

; Evaluates the result of a while statement
(define M-state-while
  (λ (expression state return-func next break continue throw)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw))
      (M-state (body expression) state return-func 
            (λ (s1) (M-state-while expression s1 return-func next break continue throw))
            (λ (s) (next s))
            (λ (s2) (M-state-while expression s2 return-func next break continue throw)) throw)
      (next state))))

#|
FUNCTION STUFF
|#

; Bind the formal parameters to the actual parameters
(define create-bindings
  (λ (formalparams actualparams state fstate1 return-func next break continue throw)
    (cond
      ((and (null? formalparams) (null? actualparams)) fstate1)
      ((eq? (length formalparams) (length actualparams)) (create-bindings (cdr formalparams)
                                                   (cdr actualparams)
                                                   state
                                                   (add (car formalparams) (M-value (car actualparams) state return-func next break continue throw) fstate1)
                                                   return-func next break continue throw))
      (else (error 'mismatch-params)))))

(define M-state-function
  (λ (expression state)
    (add (funcname expression) (create-closure (funcname expression) (params expression) (funcbody expression) state) state)))

; Function evaluator, when the function stands alone
(define M-state-funcall
  (λ (expression state return-func next break continue throw)
    (M-state (closure-body (get-val (operand expression) state))
             (create-bindings
                   (closure-params (get-val (operand expression) state))
                   (actualparams expression)
                   state
                   (addlayer ((closure-state-function (get-val (operand expression) state)) state))
                   return-func next break continue throw)
             (λ (val s) (next state))
             (λ (s) (next state))
             (λ (v) (error 'not-a-loop))
             (λ (v) (error 'not-a-loop))
             (λ (e s) (throw e state)))))

#|
M-state-funcall:
	M-state (closure-body (operator exp) state) (create-bindings ....)

closure-body -> parse tree for function body
create-bindings -> return state with function params added in an extra layer

if operator is (dot obj funcname) --> 
	obj-function-closure <- find function closure from instance closure
	(closure-body) -> get the function body from instance closure
	(create-binding closure) -> gets the function params from the instance closure, adds obj as this to list of actual-params, add layer to state 
if operator is (name) --> 
|#

; Function evaluator, when it is used in an assignment statement
(define M-value-function
  (λ (expression state return-func next break continue throw)
         (M-state (closure-body (get-val (operand expression) state))
                  (create-bindings
                        (closure-params (get-val (operand expression) state))
                        (actualparams expression)
                        state
                        (addlayer ((closure-state-function (get-val (operand expression) state)) state))
                        return-func next break continue throw)
                  (λ (val s) val)
                  (λ (s) (next state))
                  (λ (v) (error 'not-a-loop))
                  (λ (v) (error 'not-a-loop))
                  (λ (e s) (throw e state)))))

; Get the variables defined in a class body
; TESTING
(define parse-class-var
  (λ (body state)
    (if (null? body)
        state
        (match (car body)
          ((list 'var name) (parse-class-var (cdr body) (add name (list 'var name) state)))
          ((list 'var name expr) (parse-class-var (cdr body) (add name (list 'var name expr) state)))
          (_ (parse-class-var (cdr body) state))))))


; Get the functions defined in a class body
; TESTING
(define parse-class-func
  (λ (body state)
    (if (null? body)
        state
        (match (car body)
          ((list 'function name param funcbody) (parse-class-func (cdr body) (M-state-function (list 'function name param funcbody) state)))
          ((list 'static-function name param funcbody) (parse-class-func (cdr body) (M-state-function (list 'static-function name param funcbody) state)))
          (_ (parse-class-func (cdr body) state))))))

; helper function: parse class property by inserting a function that takes a class body and a state
; returns a LAYER i.e ((var_names .... ) (parser_result....)) 
(define parse-class-property
  (λ (body parser-func) (firstlayer (parser-func body (createnewstate)))))
             

#|
M-STATE HELPER FUNCTIONS
|#


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

; Creates a new binding in the state with the given variable name and the value '()
(define declare
  (λ (x state) (add x '() state)))

; Function closure 3-tuple (params, body, λ(state) -> state)
(define create-closure
  (λ (name params body state) (list params body (λ (v) (cut-until-layer name v)))))

; TESTING
(define create-instance-closure
  (λ (rt-type state) (list rt-type (instance-val (cadr (get-val rt-type state)) (createnewlayer) state))))

; Run M-value on instances inside the class closure to generate the fields for instance closure
; returns a layer i.e ((var_names .... ) (m_value_result....))
; DOES NOT WORK
(define instance-val
  (λ (class-val layer state)
    (if (null? (vars class-val))
        layer
        (instance-val (restpairs class-val) (add-to-layer (firstvar class-val)
                                                          (M-state (firstval class-val) state (λ (val s) val) (λ (v) v)
                                                                   (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)))
                                                          layer) (M-state (firstval class-val) state (λ (val s) val) (λ (v) v)
                                                                   (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)))))))
    
    
; (expression state next)
  ; find class closure
  ; call m-state-init on it
  ; class-closure super name body ((x y z) ()) (main
  ; for var, expr in var_fields: next_state = M-state-init expr state; state=next_state
#|
M-state-init: Parses classes, and classes only
M-state-class:
     if declare: add (var name, expr) to state
     if ...: add (name, expr) to state
M-state-instance (expression correpondng to fields and methods in class-closure) : Need to generate fields with interpreted value ((x, y z) (#&4, ....))
  for var, expr in var_fields: next_state = M-state-init expr state; state=next_state
|#
                                                                  
  
; TESTING
(define create-class-closure
  (λ (super-class body) (list super-class (parse-class-property (reverse body) parse-class-var)  (parse-class-property (reverse body) parse-class-func))))

(define get-class-bindings
  (λ (body) (M-class-init body (createnewstate) (λ (v) v))))

; Returns the portion of the state that contains x
(define cut-until-layer
  (λ (x state)
    (if (has-var-layer x (firstlayer state))
        state
        (cut-until-layer x (restlayers state)))))

; Checks whether or not a binding is present in a specified layer
(define has-var-layer
  (λ (x layer)
    (cond
      ((layernull? layer) #f)
      ((eq? (firstvar layer) x) #t)
      (else (has-var-layer x (restpairs layer))))))

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

; Entry point into update-cps
(define update
  (λ (x v state) (update-cps x v state (λ (q) q))))

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