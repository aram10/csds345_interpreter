#lang racket

(provide (all-defined-out))
(require racket/match)
(require "classParser.rkt" "helpers.rkt")

#|
CSDS 345 Object-Oriented Language Interpreter Project

Phan Trinh Ha
Stamatis Papadopoulos
Alexander Rambasek

5/9/2021
|#

#|

COMMENTS

We were having a lot of issue getting 'super' to work with functions. It doesn't get the correct compile-time type.
However, it does work with variables.

We are able to successfully pass the first 6 tests. The vast majority of the remaining tests were giving the same error
relating to our implementation of super, and it is likely that had we been able to resolve this, they would have fallen
into place.

We did use 'letrec' more than we would have liked. However, this was in an attempt to make the code easier to read and
build upon; all of this could have easily been translated into a strictly functional style but we made the judgement
call not to.

|#


#|
INTERPRETER

Receives a list of statements in prefix notation from the parser, and passes them to M-state
|#

(define interpret
  (λ (filename classname)
    (letrec
        [(globalstate (M-state-init (parser filename) (createnewstate) (λ (v) v) ))
         (mainclassclosure (get-val classname globalstate))
         (funcnames (class-closure-func-names mainclassclosure))
         (funcclosures (class-closure-func-closures mainclassclosure))
         (mainfuncclosure (get-val-layer 'main (list funcnames funcclosures)))
         (globalstatemain (add 'main mainfuncclosure (addlayer globalstate)))]
      (M-value-function-main '(funcall main) globalstatemain (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)) classname '()))))

#|
M-VALUE EXPRESSIONS
|#

; Evaluates the result of a boolean algebra expression
(define M-boolean
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((bool? expression) expression)
      ((funcall? expression) (M-value-function expression state return-func next break continue throw compiletype this_obj))
      ((declared? expression state) (get-val expression state))
      ((comparison? expression) (M-compare expression state return-func next break continue throw compiletype this_obj))
      ((eq? (operator expression) '&&) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (λ(x y) (and x y))))
      ((eq? (operator expression) '||) (boolstringop (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (λ(x y) (or x y))))
      ((eq? (operator expression) '!) (boolstringsingle (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                        (λ (x) (not x))))
      (else (error 'bad-operator)))))

; Evaluates the result of a comparison between 2 arithmetic expressions
(define M-compare
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((boolean? expression) (M-boolean expression state return-func next break continue throw compiletype this_obj))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw compiletype this_obj))
      ((eq? (operator expression) '==) (booltoname (= (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                      (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj))))
      ((eq? (operator expression) '!=) (booltoname (not (= (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                           (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))))
      ((eq? (operator expression) '>=) (booltoname (>= (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                       (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj))))
      ((eq? (operator expression) '<=) (booltoname (<= (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                       (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj))))
      ((eq? (operator expression) '>) (booltoname (> (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj))))
      ((eq? (operator expression) '<) (booltoname (< (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                     (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj))))
      (else (error 'bad-comparison)))))

; Evaluates the result of an arithmetic expression   
(define M-integer
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((number? expression) expression)
      ((assigned? expression state) (get-val expression state))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw compiletype this_obj))
      ((declared? expression state) (error 'value-not-found))
      ((eq? (operator expression) '+) (+ (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                         (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))
      ((and (eq? (operator expression) '-) (= 3 (length expression)))
       (- (M-value (leftoperand expression) state return-func next break continue throw) (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))
      ((and (eq? (operator expression) '-) (= 2 (length expression)))
       (* -1 (M-integer (operand expression) state return-func next break continue throw compiletype this_obj)))
      ((eq? (operator expression) '*) (* (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                         (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))
      ((eq? (operator expression) '/) (quotient (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))
      ((eq? (operator expression) '%) (remainder (M-value (leftoperand expression) state return-func next break continue throw compiletype this_obj)
                                                 (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj)))
      (else (error 'bad-operator)))))

; General expression evaluater: entry point into M-compare, M-integer, M-boolean    
(define M-value
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((declared? expression state) (get-val expression state))
      ((local-var? compiletype expression state) (get-instance-field this_obj expression compiletype state))
      ((bool? expression) expression)
      ((arithmetic? expression) (M-integer expression state return-func next break continue throw compiletype this_obj))
      ((boolalg? expression) (M-boolean expression state return-func next break continue throw compiletype this_obj))
      ((comparison? expression) (M-compare expression state return-func next break continue throw compiletype this_obj))
      ((funcall? expression) (M-value-function expression state return-func next break continue throw compiletype this_obj))
      ((new? expression) (create-instance-closure (newruntimetype expression) state))
      ((instance-closure? expression) expression)
      ((dot? expression) (get-instance-field
                                  (dot-closure expression state return-func next break continue throw compiletype this_obj)
                                  (dot-member-name expression) (get-compile-type-dot expression compiletype state) state))
      (else (error 'bad-argument)))))

(define get-compile-type-dot
  (lambda (dotexpr compiletype state)
    (match dotexpr
      ((list 'dot 'this _) compiletype)
      ((list 'dot 'super _) (get-super-class (get-val compiletype state)))
      (_ compiletype))))
#|
M-STATE EXPRESSIONS
|#

#|
Entry point into all other M-state expressions: accepts an expression which may be a list of expressions, performs the
necessary updates to the state, and evaluates to the special variable 'return, once it is declared/assigned
|#
(define M-state
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((null? expression) (next state))
      ((return? expression) (return-func (M-value (operand expression) state return-func next break continue throw compiletype this_obj) state))
      ((declare? expression) (call/cc (λ (throw-cc) (next (M-state-declare expression state return-func next break continue (λ (e s) (throw-cc (throw e s))) compiletype this_obj)))))
      ((assign? expression) (call/cc (λ (throw-cc) (next (M-state-assign expression state return-func next break continue (λ (e s) (throw-cc (throw e s))) compiletype this_obj)))))
      ((while? expression) (call/cc (λ (k)
                                      (M-state-while expression state return-func next k continue throw compiletype this_obj))))
      ((if? expression) (M-state-if expression state return-func next break continue throw compiletype this_obj))
      ((statement? expression) (M-state (car expression) state return-func (λ (s)
                                                                             (M-state (cdr expression) s return-func next break continue throw compiletype this_obj)) break continue throw compiletype this_obj))
      ((block? expression) (M-state-block (statements expression) (addlayer state) return-func (λ (s)
                                                                                                 (next (removelayer s))) (λ (s)
                                                                                                                           (break (removelayer s))) (λ (s)
                                                                                                                                                      (continue (removelayer s))) throw compiletype this_obj))
      ((trycatch? expression) (M-state-try-catch-finally expression (addlayer state) return-func (λ (s)
                                                                                                   (next (removelayer s))) (λ (s)
                                                                                                                             (break (removelayer s))) (λ (s)
                                                                                                                                                        (continue (removelayer s))) throw compiletype this_obj))
      ((throw? expression) (throw (M-value (throwvalue expression) state return-func next break continue throw compiletype this_obj) state))
      ((break? expression) (break state))
      ((continue? expression) (continue state))
      ((function? expression) (next (M-state-function expression state compiletype #t)))
      ((static-function? expression) (next (M-state-function expression state compiletype #f)))
      ((funcall? expression) (M-state-funcall expression state return-func next break continue throw compiletype this_obj))
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

; Evaluates an assignment expression that may contain arithmetic/boolean expressions and updates the state
(define M-state-assign
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((not (assign? expression)) (error 'not-an-assignment)) 
      ((not (declared? (assignvar expression) state)) (error 'variable-not-declared))
      ((declared? (assignexp expression) state) (assign (assignvar expression) (get-val (assignexp expression) state) state))
      ((and (variable? (assignexp expression)) (not (declared? (assignexp expression) state))) (error 'assigning-variable-not-declared))
      ((arithmetic? (assignexp expression)) (assign (assignvar expression) (M-integer (assignexp expression) state return-func next break continue throw compiletype this_obj) state))
      ((boolalg? (assignexp expression)) (assign (assignvar expression) (M-boolean (assignexp expression) state return-func next break continue throw compiletype this_obj) state))
      ((funcall? (assignexp expression)) (assign (assignvar expression) (M-value-function (assignexp expression) state return-func next break continue throw compiletype this_obj) state))
      ((dot? (assignvar expression)) (update-instance-field
                                  (dot-closure (assignvar expression) state return-func next break continue throw compiletype this_obj)
                                  (dot-member-name (assignvar expression))
                                  (M-value (assignexp expression) state return-func next break continue throw compiletype this_obj)
                                  compiletype state))
      (else (error 'bad-assignment)))))

; Evaluates the result of executing a block of code
(define M-state-block
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((null? expression) (next state))
      (else (M-state (firststatement expression) state return-func (λ (s)
                                                                     (M-state-block (reststatement expression) s return-func next break continue throw compiletype this_obj)) break continue throw compiletype this_obj)))))

; Adds a variable with the given name and the value '() to the state
(define M-state-declare
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (cond
      ((not (declare? expression)) (error 'not-a-declaration))
      ((eq? (length expression) 2) (declare (operand expression) state))
      ((eq? (length expression) 3) (add (leftoperand expression) (M-value (rightoperand expression) state return-func next break continue throw compiletype this_obj) state))
      (else (error 'bad-declaration)))))


; Evaluates the result of an if statement and updates the state accordingly
(define M-state-if
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw compiletype this_obj))
      (M-state (body expression) state return-func next break continue throw compiletype this_obj)
      (M-state (else-case expression) state return-func next break continue throw compiletype this_obj))))

; Evaluates the result of a try-catch/try-catch-finally block
(define M-state-try-catch-finally
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (M-state (tryblock expression) state
             ;return-func
             (λ (v s) (M-state (finallyblock expression) s return-func (λ (s1) (return-func v s1)) break continue throw compiletype this_obj))
             ;next
             (λ (s) (M-state (finallyblock expression) s return-func next break continue throw compiletype this_obj))
             ;break
             (λ (s) (M-state (finallyblock expression) s return-func break break continue throw compiletype this_obj))
             ;continue
             (λ (s) (M-state (finallyblock expression) s return-func continue break continue throw compiletype this_obj))
             ;throw
             (λ (e s) (M-state (catchblock expression) (add (catchvar expression) e s)
                                     ;return-func
                                     (λ (v1 s1) (M-state (finallyblock expression) s1 return-func (λ (s2) (return-func v1 s2)) break continue throw compiletype this_obj))
                                     ;next
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func next break continue throw compiletype this_obj))
                                     ;break
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func break break continue throw compiletype this_obj))
                                     ;continue
                                     (λ (s1) (M-state (finallyblock expression) s1 return-func continue break continue throw compiletype this_obj))
                                     ;newThrow
                                     (λ (e1 s1) (M-state (finallyblock expression) s1 return-func (λ (s2) (throw e1 s2)) break continue throw compiletype this_obj)))))))

; Evaluates the result of a while statement
(define M-state-while
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (if (nametobool (M-boolean (condition expression) state return-func next break continue throw compiletype this_obj))
      (M-state (body expression) state return-func 
            (λ (s1) (M-state-while expression s1 return-func next break continue throw compiletype this_obj))
            (λ (s) (next s))
            (λ (s2) (M-state-while expression s2 return-func next break continue throw compiletype this_obj)) throw compiletype this_obj)
      (next state))))


#| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                     MODULAR
   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|#


; Like M-state-declare, except that it doesn't evaluate the expression if it's a declare + assign
(define class-declaration
  (λ (expression state)
    (match expression
      ((list 'var name) (declare (operand expression) state))
      ((list 'var name expr) (add (leftoperand expression) (rightoperand expression) state))
      (_ (error 'bad-declaration)))))

; define class closures
(define create-class-closure
  (λ (super-class body classname) (list super-class 
                              (parse-class-var-names body '())
                              (parse-class-var-exprs body)
                              (parse-class-func-names body)
                              (parse-class-func-closures body classname))))

; contain the instance's class and a list of instance field VALUES.
(define create-instance-closure
  (λ (rt-type state)
    (if (null? rt-type)
        '(() () 'instance-closure)
        (letrec
            [(classclosure (get-val rt-type state))
             (superclass (get-super-class classclosure))
             (superclassclosure (if (null? superclass) superclass (create-instance-closure superclass state)))
             (funcnames (class-closure-func-names classclosure))
             (funcclosures (class-closure-func-closures classclosure))
             (varexprs (class-closure-var-exprs classclosure))
             (statewithfunc (cons (list funcnames funcclosures) state)) 
             (definedvarstate (M-state varexprs (addlayer statewithfunc) (λ (val s) val) (λ (v) v) (λ (v) v) (λ (v) v) (λ (e v) (error 'uncaught-exception)) rt-type '()))]
          (list rt-type (reverse (append (vals (firstlayer definedvarstate)) (reverse (instance-values (create-instance-closure superclass state))))) 'instance-closure)))))

; Gets instance of LHS of dot expression
(define dot-closure
  (lambda (expr state return-func next break continue throw compiletype this_obj)
    (match expr
      ((list 'dot 'this _) this_obj)
      ((list 'dot 'super _) this_obj)
      ((list 'dot objexpr _) (M-value objexpr state return-func next break continue throw compiletype this_obj))
      (_ this_obj))))

; Deals with RHS of dot expression
(define dot-member-name
  (lambda (expr)
    (match expr
      ((list 'dot objexpr mem) mem)
      (_ expr))))

; Generate state with class closure
(define M-state-class
  (λ (expression state) 
    (add (class-name expression) (create-class-closure (super-class expression) (class-body expression) (class-name expression)) state)))

; Create closure of functions in class body
(define parse-class-func-closures
  (λ (body classname)
    (if (null? body)
        body
        (match (car body)
          ((list 'function name param funcbody) (cons (box (create-closure name (cons 'this param) funcbody (createnewstate) classname #f)) (parse-class-func-closures (cdr body) classname)))
          ((list 'static-function name param funcbody) (cons (box (create-closure name param funcbody (createnewstate) classname #t)) (parse-class-func-closures (cdr body) classname)))
          (_ (parse-class-func-closures (cdr body) classname))))))

; Get the functions defined in a class body
(define parse-class-func-names
  (λ (body)
    (if (null? body)
        body
        (match (car body)
          ((list 'function name param funcbody) (cons name (parse-class-func-names (cdr body))))
          ((list 'static-function name param funcbody) (cons name (parse-class-func-names (cdr body))))
          (_ (parse-class-func-names (cdr body)))))))

; Get the variable expressions in a class body
(define parse-class-var-exprs
  (λ (body)
    (if (null? body)
        body
        (match (car body)
          ((list 'var name) (cons (car body) (parse-class-var-exprs (cdr body))))
          ((list 'var name expr) (cons (car body) (parse-class-var-exprs (cdr body))))
          ((list 'function name param funcbody) (cons (car body) (parse-class-var-exprs (cdr body))))
          ((list 'static-function name param funcbody) (cons (car body) (parse-class-var-exprs (cdr body))))
          (_ (parse-class-var-exprs (cdr body)))))))

; Get the variable names in a class body
(define parse-class-var-names
  (λ (body acc)
    (if (null? body)
        acc
        (match (car body)
          ((list 'var name) (parse-class-var-names (cdr body) (cons name acc)))
          ((list 'var name expr) (parse-class-var-names (cdr body) (cons name acc)))
          ((list 'function name param funcbody) (parse-class-var-names (cdr body) (cons name acc)))
          ((list 'static-function name param funcbody) (parse-class-var-names (cdr body) (cons name acc)))
          (_ (parse-class-var-names (cdr body) acc))))))


#| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                     FUNCTIONAL
   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|#

; Bind the formal parameters to the actual parameters
(define create-bindings
  (λ (formalparams actualparams state fstate1 return-func next break continue throw compiletype this_obj)
    (cond
      ((and (null? formalparams) (null? actualparams)) fstate1)
      ((eq? (length formalparams) (length actualparams)) (create-bindings (cdr formalparams)
                                                   (cdr actualparams)
                                                   state
                                                   (add (car formalparams) (M-value (car actualparams) state return-func next break continue throw compiletype this_obj) fstate1)
                                                   return-func next break continue throw compiletype this_obj))
      (else (error 'mismatch-params)))))

; Function evaluator, when the function stands alone
(define M-state-funcall
  (λ (expression state return-func next break continue throw compiletype this_obj)
    (letrec
        [
          (current_obj (dot-closure (operand expression) state return-func next break continue throw compiletype this_obj))
          (classclosure (get-val (instance-type current_obj) state))
          (funcclosure (get-instance-field current_obj (dot-member-name (operand expression)) (get-compile-type-dot (operand expression) compiletype state) state))
          (actual-params (if (static-function-closure? funcclosure) (actualparams expression) (cons current_obj (actualparams expression))))
          (current_compiletype ((closure-class-lookup-function funcclosure) state))
          (currentstate (add (dot-member-name (operand expression)) funcclosure (addlayer state)))
        ]
        (M-state (closure-body funcclosure)
                (create-bindings
                      (closure-params funcclosure)
                      actual-params
                      currentstate
                      (addlayer ((closure-state-function funcclosure) currentstate))
                      return-func next break continue throw current_compiletype current_obj)
                (λ (val s) (next currentstate))
                (λ (s) (next currentstate))
                (λ (v) (error 'not-a-loop))
                (λ (v) (error 'not-a-loop))
                (λ (e s) (throw e currentstate))
                current_compiletype current_obj))))

; Handle initialization of class functions
(define M-state-function
  (λ (expression state compiletype is_static)
    (letrec
      [(funcparams (if is_static 
                        (params expression)
                        (cons 'this (params expression))))]
      (add (funcname expression) (create-closure (funcname expression) funcparams (funcbody expression) state compiletype is_static) state))))

; Handles functions that result in a value
(define M-value-function
  (λ (expression state return-func next break continue throw compiletype this_obj)
      (letrec
        [
          (current_obj (dot-closure (operand expression) state return-func next break continue throw compiletype this_obj))
          (classclosure (get-val (instance-type current_obj) state))
          (funcclosure (get-instance-field current_obj (dot-member-name (operand expression)) (get-compile-type-dot (operand expression) compiletype state) state))
          (actual-params (if (static-function-closure? funcclosure) (actualparams expression) (cons current_obj (actualparams expression))))
          (current_compiletype ((closure-class-lookup-function funcclosure) state))
          (currentstate (add (dot-member-name (operand expression)) funcclosure (addlayer state)))
          ]
        (M-state (closure-body funcclosure)
                (create-bindings
                      (closure-params funcclosure)
                      actual-params
                      currentstate
                      (addlayer ((closure-state-function funcclosure) currentstate))
                      return-func next break continue throw current_compiletype current_obj)
                (λ (val s) val)
                (λ (s) (next currentstate))
                (λ (v) (error 'not-a-loop))
                (λ (v) (error 'not-a-loop))
                (λ (e s) (throw e currentstate))
                current_compiletype current_obj))))

; Evaluates main
(define M-value-function-main
  (λ (expression state return-func next break continue throw compiletype this_obj)
         (M-state (closure-body (get-val (operand expression) state))
                  (create-bindings
                        (closure-params (get-val (operand expression) state))
                        (actualparams expression)
                        state
                        (addlayer ((closure-state-function (get-val (operand expression) state)) state))
                        return-func next break continue throw compiletype this_obj)
                  (λ (val s) val)
                  (λ (s) (next state))
                  (λ (v) (error 'not-a-loop))
                  (λ (v) (error 'not-a-loop))
                  (λ (e s) (throw e state))
                  compiletype this_obj)))