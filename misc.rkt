#lang racket

;pow takes two numbers, x and y, and returns x raised to the power y. You can assume y is a non-negative integer.
(define pow
  (lambda (x y)
    (if (zero? y)
        1
        (* x (pow x (- y 1))))))

;mygcd takes two positive integers and returns the greatest common divisor of the two integers
(define mygcd
  (lambda (a b)
    (if (zero? (remainder a b))
        b
        (mygcd b (remainder a b)))))

;numoccurring takes an atom and a list of atoms and returns the number of times the atom occurs in the list
(define numoccurring
  (lambda (a lat)
    (if (null? lat)
        0
        (if (eq? a (car lat))
            (+ 1 (numoccurring a (cdr lat)))
            (numoccurring a (cdr lat))))))

(define numoccurring2
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (+ 1 (numoccurring2 a (cdr lat))))
      (else (numoccurring2 a (cdr lat))))))

;sumnumbers takes a list of atoms and numbers and returns the sum of all the numbers in the list
(define sumnumbers
  (lambda (lat)
    (if (null? lat)
        0
        (if (number? (car lat))
            (+ (car lat) (sumnumbers (cdr lat)))
            (sumnumbers (cdr lat))))))

(define sumnumbers2
  (lambda (lat)
    (cond
      ((null? lat) 0)
      ((number? (car lat)) (+ (car lat) (sumnumbers2 (cdr lat))))
      (else (sumnumbers2 (cdr lat))))))

;repeat takes an atom and a non-negative integer and returns a list containing that number of copies of the atom.
(define repeat
  (lambda (a int)
    (cond
      ((zero? int) (quote ()))
      (else (cons a (repeat a (- int 1)))))))

;squares takes a list of numbers and returns a list that contains the square of every number in the input list
(define squares
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (* (car lat) (car lat)) (squares (cdr lat))))
      (else (cons (car lat) (squares (cdr lat)))))))

;insertR takes two atoms, new and old, and a list of atoms, lat. The atom 'new' is appended to the right of the first occurrence of the atom 'old' in the list
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

;insertL is identical to insertR, except that the new atom is appended to the left of the old atom instead of to the right.
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

;subst(new old lat) replaces the first occurrence of old in lat with new.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

;subst(new o1 o2 lat) replaces either the first occurrence of o1 or the first occurrence of o2 in lat with new.
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

;multirember(a lat) is like rember, except that it removes all occurrences of a in lat.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

;multiinsertR is like insertR, except that the new atom is appended to the right of all occurrences of old.
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;multisubst is like subst, except that all occurrences of old in lat are replaced by new.
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

;pick(n lat) evaluates to the nth member of lat
(define pick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))


;add-to-end(a lat) appends the atom a to the end of lat
(define add-to-end
  (lambda (a lat)
    (cond
      ((null? lat) (cons a '()))
      (else (cons (car lat) (add-to-end a (cdr lat)))))))


;myreverse: '(a b c d) ==> '(d c b a)
(define myreverse
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (append (myreverse (cdr lat)) (cons (car lat) '()))))))

;mymap: takes a function and a list. Applies the function to every member of the list.
(define mymap
  (lambda (f lis)
    (if (null? lis)
        '()
        (cons (f (car lis)) (mymap f (cdr lis))))))

;replaceall* takes two atoms and a list with sublists, and it replaces every instance of a with b
(define replaceall*
  (lambda (a b lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cons b (replaceall* a b (cdr lat))))
      ((list? (car lat)) (cons (replaceall* a b (car lat)) (replaceall* a b (cdr lat))))
      (else (cons (car lat) (replaceall* a b (cdr lat)))))))

;sumnumbers* takes a list that can contain sublists and sums all numbers inside of it
(define sumnumbers*
  (lambda (lat)
    (cond
      ((null? lat) 0)
      ((number? (car lat)) (+ (car lat) (sumnumbers* (cdr lat))))
      ((list? (car lat)) (+ (sumnumbers* (car lat)) (sumnumbers* (cdr lat))))
      (else (sumnumbers* (cdr lat))))))

;emptyall* removes all (non-empty list) atoms from a list
(define emptyall*
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((list? (car lat)) (cons (emptyall* (car lat)) (emptyall* (cdr lat))))
      (else (emptyall* (cdr lat))))))

;flatten* given a list, returns a list of all of the atoms in the original list, but without any sublist structure
(define flatten*
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((list? (car lat)) (cons (flatten (car lat)) (flatten (cdr lat))))
      (else (cons (car lat) (flatten (cdr lat)))))))

;member*? checks if an atom is anywhere in a list, which may contain sublists
(define member*?
  (lambda (a l)
    (cond
      ((null? l) #t)
      ((pair? (car l)) (or (member*? a (car l)) (member*? a (cdr l))))
      ((eq? a (car l)) #t)
      (else (member*? a (cdr l))))))
      
             
