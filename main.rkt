#lang racket

(provide (all-defined-out))
(require "simpleParser.rkt" "functionParser.rkt" "helpers.rkt")

#|
CSDS 345 Simple Language Interpreter Project

Phan Trinh Ha
Stamatis Papadopoulos
Alexander Rambasek

3/29/2021
|#


#|
INTERPRETER

Receives a list of statements in prefix notation from the parser, and passes them to M-state
|#

(define interpret
  (lambda (filename)
    (M-state (parser-simple filename) (createnewstate) (lambda (val s) val) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (e v) (error 'uncaught-exception)))))

(define interpret-w-func
  (lambda (filename)
    (letrec
        ((globalstate (M-state-init (parser filename) (createnewstate) (lambda (val s) val) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (e v) (error 'uncaught-exception)))))
      (M-value-function '(funcall main) globalstate (lambda (val s) val) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (e v) (error 'uncaught-exception))))))
      

#|
M-VALUE EXPRESSIONS
|#

; Evaluates the result of a boolean algebra expression
(define M-boolean
  (lambda (expression state return-func next break continue throw)
    (cond
      ((isbool? expression) expression)
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((declared? expression state) (get-val expression state))
      ((comparison? expression) (M-compare expression state return-func next break continue throw))
      ((eq? (operator expression) '&&) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw)
                                                     (lambda(x y) (and x y))))
      ((eq? (operator expression) '||) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw)
                                                     (M-value (rightoperand expression) state return-func next break continue throw)
                                                     (lambda(x y) (or x y))))
      ((eq? (operator expression) '!) (boolstringsingle (M-value (leftoperand expression) state return-func next break continue throw)
                                                        (lambda (x) (not x))))
      (else (error 'bad-operator)))))

; Evaluates the result of a comparison between 2 arithmetic expressions
(define M-compare
  (lambda (expression state return-func next break continue throw)
    (cond
      ((boolean? expression) (M-boolean expression state return-func next break continue throw))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((eq? (operator expression) '==) (booltoname (= (M-integer (leftoperand expression) state return-func next break continue throw)
                                                      (M-integer (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '!=) (booltoname (not (= (M-integer (leftoperand expression) state return-func next break continue throw)
                                                           (M-integer (rightoperand expression) state return-func next break continue throw)))))
      ((eq? (operator expression) '>=) (booltoname (>= (M-integer (leftoperand expression) state return-func next break continue throw)
                                                       (M-integer (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '<=) (booltoname (<= (M-integer (leftoperand expression) state return-func next break continue throw)
                                                       (M-integer (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '>) (booltoname (> (M-integer (leftoperand expression) state return-func next break continue throw)
                                                     (M-integer (rightoperand expression) state return-func next break continue throw))))
      ((eq? (operator expression) '<) (booltoname (< (M-integer (leftoperand expression) state return-func next break continue throw)
                                                     (M-integer (rightoperand expression) state return-func next break continue throw))))
      (else (error 'bad-comparison)))))

; Evaluates the result of an arithmetic expression   
(define M-integer
  (lambda (expression state return-func next break continue throw)
    (cond
      ((number? expression) expression)
      ((assigned? expression state) (get-val expression state))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      ((declared? expression state) (error 'value-not-found))
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state return-func next break continue throw)
                                         (M-integer (rightoperand expression) state return-func next break continue throw)))
      ((and (eq? (operator expression) '-) (= 3 (length expression)))
       (- (M-integer (leftoperand expression) state return-func next break continue throw) (M-integer (rightoperand expression) state return-func next break continue throw)))
      ((and (eq? (operator expression) '-) (= 2 (length expression)))
       (* -1 (M-integer (operand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state return-func next break continue throw)
                                         (M-integer (rightoperand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state return-func next break continue throw)
                                                (M-integer (rightoperand expression) state return-func next break continue throw)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state return-func next break continue throw)
                                                 (M-integer (rightoperand expression) state return-func next break continue throw)))
      (else (error 'bad-operator)))))

; General expression evaluater: entry point into M-compare, M-integer, M-boolean    
(define M-value
  (lambda (expression state return-func next break continue throw)
    (cond
      ((declared? expression state) (get-val expression state))
      ((isbool? expression) expression)
      ((arithmetic? expression) (M-integer expression state return-func next break continue throw))
      ((boolalg? expression) (M-boolean expression state return-func next break continue throw))
      ((comparison? expression) (M-compare expression state return-func next break continue throw))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw))
      (else (error 'bad-argument)))))

; funcall name (actualparams)

(define M-value-function
  (lambda (expression state return-func next break continue throw)
    (letrec
        ((closure (get-val (operand expression) state))
         (fstate1 ((closure-state-function closure) state))
         (formalparams (closure-params closure))
         (fstate2 (create-bindings formalparams (actualparams expression) state (addlayer fstate1) return-func next break continue throw)))
         (M-state (closure-body closure) fstate2 return-func (lambda (s) (next state)) (lambda (v) (error 'not-a-loop)) (lambda (v) (error 'not-a-loop)) throw)
      )))

(define M-state-funcall
  (lambda (expression state return-func next break continue throw)
    (letrec
        ((closure (get-val (operand expression) state))
         (fstate1 ((closure-state-function closure) state))
         (formalparams (closure-params closure))
         (fstate2 (create-bindings formalparams (actualparams expression) state (addlayer fstate1) return-func next break continue throw)))
         (M-state (closure-body closure) fstate2 (lambda (val s) (next state)) (lambda (s) (next state)) (lambda (v) (error 'not-a-loop)) (lambda (v) (error 'not-a-loop)) throw)
      )))
         



#|
M-STATE EXPRESSIONS
|#

#|
Entry point into all other M-state expressions: accepts an expression which may be a list of expressions, performs the
necessary updates to the state, and evaluates to the special variable 'return, once it is declared/assigned
|#
(define M-state
  (lambda (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      ((return? expression) (return-func (M-value (operand expression) state return-func next break continue throw) state))
      ((declare? expression) (next (M-state-declare expression state return-func next break continue throw)))
      ((assign? expression) (next (M-state-assign expression state return-func next break continue throw)))
      ((while? expression) (call/cc (lambda (k)
                                      (M-state-while expression state return-func next k continue throw))))
      ((if? expression) (M-state-if expression state return-func next break continue throw))
      ((statement? expression) (M-state (car expression) state return-func (lambda (s)
                                                                             (M-state (cdr expression) s return-func next break continue throw)) break continue throw))
      ((block? expression) (M-state-block (statements expression) (addlayer state) return-func (lambda (s)
                                                                                                 (next (removelayer s))) (lambda (s)
                                                                                                                           (break (removelayer s))) (lambda (s)
                                                                                                                                                      (continue (removelayer s))) throw))
      ((trycatch? expression) (M-state-try-catch-finally expression (addlayer state) return-func (lambda (s)
                                                                                                   (next (removelayer s))) (lambda (s)
                                                                                                                             (break (removelayer s))) (lambda (s)
                                                                                                                                                        (continue (removelayer s))) throw))
      ((throw? expression) (throw (throwvalue expression) state))
      ((break? expression) (break state))
      ((continue? expression) (continue state))
      ((function? expression) (next (M-state-function expression state)))
      ((funcall? expression) (M-state-funcall expression state return-func next break continue throw))
      (else error 'unsupported-statement)
    )))

(define M-state-init
  (lambda (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      ((statement? expression) (M-state-init (car expression) state return-func (lambda (s)
                                                                             (M-state-init (cdr expression) s return-func next break continue throw)) break continue throw))
      ((declare? expression) (next (M-state-declare expression state return-func next break continue throw)))
      ((assign? expression) (next (M-state-assign expression state return-func next break continue throw)))
      ((function? expression) (next (M-state-function expression state)))
      (else (M-state-init (cdr expression) state return-func next break continue throw)))))
      
    

; Evaluates an assignment expression that may contain arithmetic/boolean expressions and updates the state
(define M-state-assign
  (lambda (expression state return-func next break continue throw)
    (cond
      ((not (assign? expression)) (error 'not-an-assignment))
      ((not (declared? (assignvar expression) state)) (error 'variable-not-declared))
      ((declared? (assignexp expression) state) (assign (assignvar expression) (get-val (assignexp expression) state) state))
      ((and (variable? (assignexp expression)) (not (declared? (assignexp expression) state))) (error 'assigning-variable-not-declared))
      ((arithmetic? (assignexp expression)) (assign (assignvar expression) (M-integer (assignexp expression) state return-func next break continue throw) state))
      ((boolalg? (assignexp expression)) (assign (assignvar expression) (M-boolean (assignexp expression) state return-func next break continue throw) state))
      ((funcall? (assignexp expression)) (assign (assignvar expression) (M-value (assignexp expression) state return-func next break continue throw) state))
      (else (error 'bad-assignment)))))

; Evaluates the result of executing a block of code
(define M-state-block
  (lambda (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      (else (M-state (firststatement expression) state return-func (lambda (s)
                                                                     (M-state-block (reststatement expression) s return-func next break continue throw)) break continue throw)))))

; Adds a variable with the given name and the value '() to the state
(define M-state-declare
  (lambda (expression state return-func next break continue throw)
    (cond
      ((not (declare? expression)) (error 'not-a-declaration))
      ((eq? (length expression) 2) (declare (operand expression) state))
      ((eq? (length expression) 3) (add (leftoperand expression) (M-value (rightoperand expression) state return-func next break continue throw) state))
      (else (begin (println expression) (error 'bad-declaration))))))

; (( (x y fib) (#&5 #&true #&(params body lambda)) ))

(define M-state-function
  (lambda (expression state)
    (add (funcname expression) (create-closure (funcname expression) (params expression) (funcbody expression) state) state)))


    

; Evaluates the result of an if statement and updates the state accordingly
(define M-state-if
  (lambda (expression state return-func next break continue throw)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw))
      (M-state (body expression) state return-func next break continue throw)
      (M-state (else-case expression) state return-func next break continue throw))))

#|
Notation:
                    --------caddy---------      ---------finny--------
(try <tryblock>    (catch (e) <catchblock>)    (finally <finallyblock>)   )
|#
(define M-state-try-catch-finally
  (lambda (expression state return-func next break continue throw)
    (M-state (tryblock expression) state
             ;return-func
             (lambda (v s) (M-state (finallyblock expression) s return-func (lambda (s1) (return-func v s1)) break continue throw))
             ;next
             (lambda (s) (M-state (finallyblock expression) s return-func next break continue throw))
             ;break
             (lambda (s) (M-state (finallyblock expression) s return-func break break continue throw))
             ;continue
             (lambda (s) (M-state (finallyblock expression) s return-func continue break continue throw))
             ;throw
             (lambda (e s) (M-state (catchblock expression) (add (catchvar expression) e s)
                                     ;return-func
                                     (lambda (v1 s1) (M-state (finallyblock expression) s1 return-func (lambda (s2) (return-func v1 s2)) break continue throw))
                                     ;next
                                     (lambda (s1) (M-state (finallyblock expression) s1 return-func next break continue throw))
                                     ;break
                                     (lambda (s1) (M-state (finallyblock expression) s1 return-func break break continue throw))
                                     ;continue
                                     (lambda (s1) (M-state (finallyblock expression) s1 return-func continue break continue throw))
                                     ;newThrow
                                     (lambda (e1 s1) (M-state (finallyblock expression) s1 return-func (lambda (s2) (throw e1 s2)) break continue throw)))))))


; Evaluates the result of a while statement and updates the state accordingly
(define M-state-while
  (lambda (expression state return-func next break continue throw)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw))
      (M-state (body expression) state return-func 
            (lambda (s1) (M-state-while expression s1 return-func next break continue throw))
            (lambda (s) (next s))
            (lambda (s2) (M-state-while expression s2 return-func next break continue throw)) throw)
      (next state))))

#|
FUNCTION STUFF
|#

(define create-bindings
  (lambda (formalparams actualparams state fstate1 return-func next break continue throw)
    (cond
      ((and (null? formalparams) (null? actualparams)) fstate1)
      ((eq? (length formalparams) (length actualparams)) (create-bindings (cdr formalparams)
                                                   (cdr actualparams)
                                                   state
                                                   (add (car formalparams) (M-value (car actualparams) state return-func next break continue throw) fstate1)
                                                   return-func next break continue throw))
      (else (error 'mismatch-params)))))


#|
M-STATE HELPER FUNCTIONS
|#


; Creates a new binding in the state with the given variable name and the given value; corresponds to a simultaneous declaration and assignment
(define add
  (lambda (x v state)
    (cond
      ((member? x (vars (firstlayer state))) (begin (println state) (error 'bad-declaration)))
      (else (cons (add-to-layer x v (firstlayer state)) (restlayers state))))))

; Creates a variable binding in a particular layer of the state
(define add-to-layer
  (lambda (x v layer)
    (cond
      ((member? x (vars layer)) (error 'variable-exists))
      (else (cons (cons x (vars layer)) (cons (cons (box v) (vals layer)) '()))))))

; Updates the binding of a declared variable in the state with the given value
(define assign
  (lambda (x v state) (begin (set-box! (get-box x state) v) state)))

; Creates a new binding in the state with the given variable name and the value '()
(define declare
  (lambda (x state) (add x '() state)))

(define create-closure
  (lambda (name params body state) (cons params (cons body (cons (lambda (v) (cut-until-layer name v)) '())))))

(define cut-until-layer
  (lambda (x state)
    (if (has-var-layer x (firstlayer state))
        state
        (cut-until-layer x (restlayers state)))))

(define has-var-layer
  (lambda (x layer)
    (cond
      ((layernull? layer) #f)
      ((eq? (firstvar layer) x) #t)
      (else (has-var-layer x (restpairs layer))))))

; Entry point into remove-cps
(define remove-layer
  (lambda (x state) (remove-layer-cps x state (lambda (v) v))))

; Removes a binding from the state if it exists, in continuation passing style
(define remove-layer-cps
  (lambda (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state)) (return (restpairs state)))
      (else (remove-layer-cps x (restpairs state)
            (lambda (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))

; Evaluates a return expression, and creates a new binding in the state with the result and the special variable name 'return
; DEPRECATED
#|
(define return
  (lambda (expression state return-func next break continue throw)
    (cond
      ((null? expression) (declare 'return state))
      (else (add 'return (M-value (operand expression) state return-func next break continue throw) state)))))
|#

; Entry point into update-cps
(define update
  (lambda (x v state) (update-cps x v state (lambda (q) q))))

; Updates the binding of the given variable in the state
(define update-cps
  (lambda (x v state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return (createnewstate)))
      ((eq? x (firstvar state))
       (begin
         (set-box! (firstbox state) v)
         (return state)
       ))
      (else (update-cps x v (restpairs state)
            (lambda (s) (return (cons (cons (firstvar state) (vars s)) (cons (cons (firstbox state) (vals s)) '())))))))))

; Updates the binding of a declared variable in a single layer of the state
(define update-layer
 (lambda (x v layer)
   (if (eq? x (firstvar layer))
       (set-box! (firstval layer) v)
       (update-layer x v (restpairs layer)))))