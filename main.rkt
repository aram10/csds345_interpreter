#lang racket

(provide (all-defined-out))
(require "functionParser.rkt" "helpers.rkt")

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

(define interpret
  (λ (filename)
    (letrec
        ((globalstate (M-state-init (parser filename) (createnewstate) (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)))))
      (M-value-function '(funcall main) globalstate (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception))))))
      

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
      (else error 'unsupported-statement)
    )))

; The "vanguard" of the interpreter: initial pass-through to bind global variables and functions
(define M-state-init
  (λ (expression state return-func next break continue throw)
    (cond
      ((null? expression) (next state))
      ((statement? expression) (M-state-init (car expression) state return-func (λ (s)
                                                                             (M-state-init (cdr expression) s return-func next break continue throw)) break continue throw))
      ((declare? expression) (next (M-state-declare expression state return-func next break continue throw)))
      ((assign? expression) (next (M-state-assign expression state return-func next break continue throw)))
      ((function? expression) (next (M-state-function expression state)))
      (else (M-state-init (cdr expression) state return-func next break continue throw)))))

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

#|
class A{
var h = 10;

function x(a, b)
{
   return a + b;
}

function y(c, d)
{
   return c - d;
}
}

( (main A) ((return 10) )

( (() ((h x y) (10 (a b (return a + b) λ(s)) (c d (return c - d) λ(s))))) )

a = new A();
b = new A();

|#


(define create-instance-closure
  (λ (rt-type values) (list rt-type values)))

(define create-class-closure
  (λ (super-class bindings) (list super-class bindings)))

(define get-class-bindings
  (λ (body) (M-state-init body (createnewstate) (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)))))

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