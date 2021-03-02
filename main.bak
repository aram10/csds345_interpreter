#lang racket


(define M-integer
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-operator)))))

(define M-boolean
  (lambda (expression)
    (cond
      ((boolean? expression) expression)
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression)) (M-boolean (rightoperand expression))))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression)) (M-boolean (rightoperand expression))))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression))))
      (else (error 'bad-operator)))))

; M-value: combination of M-integer and M-value

; state: list of names, list of values


(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)


