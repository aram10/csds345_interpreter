#lang racket

#|
NOTES

state: ((list of names) (list of values))  e.g. ((x y z w...) (5 true 12 '()...))

|#


(define M-integer
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((declared? expression (vars state)) (get-val expression state))
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-operator)))))

(define M-boolean
  (lambda (expression state)
    (cond
      ((isbool? expression) expression)
      ((declared? expression (vars state)) (get-val expression state))
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression) state)))
      (else (error 'bad-operator)))))
    
(define M-value
  (lambda (expression state)
    (cond
      ((arithmetic? expression) (M-integer expression state))
      ((boolalg? expression) (M-boolean expression state))
      (else (error 'bad-argument)))))


; M-state
#|
  case =: assign x M_value(exp) M_state(exp)
|#

#|
assign statements look like this

(= var (expression))
|#


(define M-state-assign
  (lambda (expression state)
    (cond
      ((not (assign? expression)) (error 'not-an-assignment))
      ((not (declared? (assignvar expression) (vars state))) (error 'variable-not-declared))
      ((arithmetic? (assignexp expression)) (assign (assignvar expression) (M-integer (assignexp expression) state) state))
      ((boolalg? (assignexp expression)) (assign (assignvar expression) (M-boolean (assignexp expression) state) state))
      (else (error 'bad-assignment)))))

(define M-state-declare
  (lambda (expression state)
    (cond
      ((not (declare? expression)) (error 'not-a-declaration))
      ((eq? (length expression) 2) (declare (operand expression) state))
      ((eq? (length expression) 3) (add (leftoperand expression) (M-value (rightoperand expression) state) state))
      (else (error 'bad-declaration)))))
      

; Declares a new variable, and sets its value to null
(define declare
  (lambda (x state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons '() (vals state)) '()))))))

; add a new variable, and sets its value to val
(define add
  (lambda (x v state)
    (cond
      ((member? x (vars state)) (error 'bad-declaration))
      (else
       (cons (cons x (vars state)) (cons (cons v (vals state)) '()))))))

; Assigns a value to a variable
(define assign
  (lambda (x v state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) state)
      (else (add x v (remove x state))))))

(define remove-cps
  (lambda (x state return)
    (cond
      ((or (null? (vars state)) (null? (vals state))) (return '(() ())))
      ((eq? x (firstvar (vars state))) (return (cons (restvars (vars state)) (cons (restvals (vals state)) '()))))
      (else (remove-cps x (cons (restvars (vars state)) (cons (restvals (vals state)) '())) 
            (lambda (s) (return (cons (cons (firstvar (vars state)) (vars s)) (cons (cons (firstval (vals state)) (vals s)) '())))))))))

(define remove
  (lambda (x state) (remove-cps x state (lambda (v) v))))
          
(define M-compare
  (lambda (expression state)
    (cond
      ((boolean? expression) (M-boolean expression state))
      ((eq? (operator expression) '==) (= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (= (M-integer (leftoperand expression)) state (M-integer (rightoperand expression) state))))
      ((eq? (operator expression) '>=) (>= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '<) (< (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-comparison)))))

; Returns the value of the atom
(define M-value-atom
  (lambda (a)
    (cond
      ((null? a) '())
      ((or (number? a) (boolean? a)) a)
      (else (error 'not-an-atom)))))

; Evaluates 'true' and 'false' to respective boolean
(define M-value-bool
  (lambda (b)
    (cond
      ((eq? b 'true) #t)
      ((eq? b 'false) #f)
      (else (error 'not-a-bool)))))

; Returns the value in the state bound to given variable
(define get-val
  (lambda (x state)
    (cond
      ((or (null? (vars state)) (null? (vals state))) '())
      ((eq? (firstvar (vars state)) x) (firstval (vals state)))
      (else (get-val x (cons (restvars (vars state)) (cons (restvals (vals state)) '())))))))
  
    
#| HELPER FUNCTIONS |#

(define member?
  (lambda (a l)
    (cond
      ((null? a) #t)
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

; Arithmetic/boolean expression helpers
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)

; var x 5

(define operand (lambda (expression) (cadr expression)))

; Retrieve sublist of variables and sublist of values from the state, respectively
(define vars (lambda (state) (car state)))
(define vals (lambda (state) (cadr state)))

; Retrieve the first variable of the variables sublist
(define firstvar (lambda (vars) (car vars)))
; Retrieve the first value of the values sublist
(define firstval (lambda (vals) (car vals)))

; Retrieve all variables except the first
(define restvars (lambda (vars) (cdr vars)))
; Retrieve all values expect the first
(define restvals (lambda (vals) (cdr vals)))

; Get variable from assign statement
(define assignvar (lambda (expression) (cadr expression)))

(define assignexp (lambda (expression) (caddr expression)))

#| Determine types of expressions |#

(define arithmetic?
  (lambda (expr)
    (cond
      ((number? expr) #t)
      ((member? (operator expr) '(+ - * / %)) #t)
      (else #f))))

(define boolalg?
  (lambda (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      ((member? (operator expr) '(&& || !)) #t)
      (else #f))))

(define assign?
  (lambda (expr)
    (if (eq? (operator expr) '=)
        #t
        #f)))

(define declare?
  (lambda (expr)
    (if (eq? (operator expr) 'var)
        #t
        #t)))

#|
(define while?
  (lambda (expr)
    (if (eq? ( expr
|#


; Checks if a given variable has been declared
(define declared?
  (lambda (x vars)
    (cond
      ((null? vars) #f)
      ((not (atom? x)) #f)
      ((eq? (firstvar vars) x) #t)
      (else (declared? x (cdr vars))))))

; (booltoname #t) ==> 'true   (booltoname #f) ==> 'false
(define booltoname
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else (error 'not-a-bool)))))

(define nametobool
  (lambda (a)
    (cond
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (error 'not-a-bool)))))

; Check for atomic boolean only
(define isbool?
  (lambda (a)
    (cond
      ((or (list? a) (number? a)) #f)
      ((or (eq? a 'true) (eq? a 'false)) #t)
      (else #f))))

;checks if a construct is an atom
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

    







