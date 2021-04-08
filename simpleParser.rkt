; If you are not using racket, comment these two lines, uncomment the (load "lex.scm") line and comment the (require "lex.scm") line
#lang racket
(provide (all-defined-out))

; A simple parser for a Java-ish language
; CSDS 345: Programming Language Concepts
;
; A recursive descent parser and a lexical analyzer for simple Java statements.
; The language allows assignments, all mathematical expressions, if statements,
; while statement with break and continue, and blocks.
;
; To call the parser, use:
;     (parser filename)
;
; The return value is a parse tree in list format

; (load "lex.scm")
(require "lex.rkt")

(define parser-simple
  (lambda (filename)
    (begin (start-lex filename)
           (let ((parse-tree (simple-program-parse)))
             (end-lex)
             parse-tree))))

;===============================================
; The recursive descent parser

(define simple-program-parse
  (lambda ()
    (if (eq? (car (get-next-symbol)) 'EOF)
       '()
       (begin
         (unget-next-symbol)
         (let ((parsetree (statement-parse-simple)))
           (cons parsetree (simple-program-parse)))))))

; parse a statement that can be an if-statement, a while-statement, or a compound statement
; and if none of the above, it is a simple statement

(define statement-parse-simple
  (lambda ()
    (let ((nextsymbol (car (get-next-symbol))))
      (cond
        ((eq? nextsymbol 'if) (simple-if-parse))
        ((eq? nextsymbol 'while) (simple-while-parse))
        ((eq? nextsymbol 'try) (simple-try-parse))
        ((eq? nextsymbol 'LEFTBRACE) (cons 'begin (simplound-statement-parse)))
        (else (begin
                (unget-next-symbol)
                (simpler-statement-parse)))))))

; parse a simple statement that can be a return, break, continue, or an assignment statement

(define simpler-statement-parse
  (lambda ()
    (let ((nextsymbol (get-next-symbol))
          (parse-statement '()))
      (begin
        (cond ((eq? (car nextsymbol) 'return) (set! parse-statement (simple-return-parse)))
              ((eq? (car nextsymbol) 'var) (set! parse-statement (simple-declare-parse)))
              ((eq? (car nextsymbol) 'break) (set! parse-statement (list 'break)))
              ((eq? (car nextsymbol) 'continue) (set! parse-statement (list 'continue)))
              ((eq? (car nextsymbol) 'throw) (set! parse-statement (list 'throw (simple-value-parse))))
              (else (begin (unget-next-symbol) (set! parse-statement (simple-assign-parse)))))
         (if (eq? (car (get-next-symbol)) 'SEMICOLON)
             parse-statement
             (error 'parser "Missing semicolon"))))))

; parse a compound statement.  We already saw the left brace so continue until we see a right brace.

(define simplound-statement-parse
  (lambda ()
    (if (eq? (car (get-next-symbol)) 'RIGHTBRACE)
        '()
        (begin
          (unget-next-symbol)
          (let ((s (statement-parse-simple)))
            (cons s (simplound-statement-parse)))))))

; parse a return statement: return followed by a value.

(define simple-return-parse
  (lambda ()
    (list 'return (simple-value-parse))))

; parse an if statement: a condition inside parentheses, an if statement, and an optional else

(define simple-if-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTPAREN))
        (error 'parser "Missing opening parenthesis")
        (let ((condition (simple-value-parse)))  ; changed
           (if (not (eq? (car (get-next-symbol)) 'RIGHTPAREN))
               (error 'parser "Missing closing parenthesis")
               (let ((if-statement (statement-parse-simple)))
                  (if (eq? (car (get-next-symbol)) 'else)
                      (list 'if condition if-statement (statement-parse-simple))
                      (begin
                        (unget-next-symbol)
                        (list 'if condition if-statement)))))))))

; parse a while statement: a condition followed by a statement

(define simple-while-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTPAREN))
        (error 'parser "Missing opening parenthesis")
        (let ((condition (simple-value-parse)))
          (if (not (eq? (car (get-next-symbol)) 'RIGHTPAREN))
              (error 'parser "Missing closing parenthesis")
              (list 'while condition (statement-parse-simple)))))))

; parse a try block.  The try block is a compound statement followed by catch block and/or
; a finally block

(define simple-try-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
        (error 'parser "Left brace expected")
        (let* ((tryblock (simplound-statement-parse))
               (catchblock (simple-catch-parse))
               (finallyblock (simple-finally-parse)))
          (if (and (null? catchblock) (null? finallyblock))
              (error 'parser "try without catch of finally")
              (list 'try tryblock catchblock finallyblock))))))

; parse a catch block.  The catch block must contain a variable (the exception) inside
; parentheses and then a block of code.

(define simple-catch-parse
  (lambda ()
    (let ((nextsymbol (car (get-next-symbol))))
      (if (not (eq? nextsymbol 'catch))
          (begin
            (unget-next-symbol)
            '())
          (let* ((firstsymbol (get-next-symbol))
                 (secondsymbol (get-next-symbol))
                 (thirdsymbol (get-next-symbol))
                 (fourthsymbol (get-next-symbol)))
            (cond ((not (eq? (car firstsymbol) 'LEFTPAREN)) (error 'parser "Missing left parenthesis"))
                  ((not (eq? (car secondsymbol) 'ID)) (error 'parser "Missing exception parameter"))
                  ((not (eq? (car thirdsymbol) 'RIGHTPAREN)) (error 'parser "Missing closing parenthesis"))
                  ((not (eq? (car fourthsymbol) 'LEFTBRACE)) (error 'parser "Missing opening brace"))
                  (else (list 'catch (list (cdr secondsymbol)) (simplound-statement-parse)))))))))

; parse a finally block.  A finally block is a compound statement that starts with "finally"

(define simple-finally-parse
  (lambda ()
    (let ((nextsymbol (get-next-symbol)))
      (if (not (eq? (car nextsymbol) 'finally))
          (begin
            (unget-next-symbol)
            '())
          (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
              (error 'parser "Missing opening parenthesis")
              (list 'finally (simplound-statement-parse)))))))

; parse a condition: a value followed by a comparison operator followed by a value.

(define simple-cond-parse
  (lambda ()
     (let* ((firstoperand (simple-value-parse))
	    (op (get-next-symbol)))
       (if (and (eq? (car op) 'BINARY-OP) 
                (or (eq? (cdr op) '==) 
                    (eq? (cdr op) '<)
                    (eq? (cdr op) '>)
                    (eq? (cdr op) '<=)
                    (eq? (cdr op) '>=)
                    (eq? (cdr op) '!=)))
	  (list (cdr op) firstoperand (simple-value-parse))
          (error 'parser "Unknown comparison operator")))))

; parse a variable declaration: var then left-hand-side with optional = followed by a value

(define simple-declare-parse
  (lambda ()
    (let* ((lhs (simple-lhs-parse))
           (op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (append (cons 'var lhs) (list (simple-value-parse)))
          (begin
            (unget-next-symbol)
            (cons 'var lhs))))))
    

; parse an assignment statement: a left-hand-side followed by an = followed by a value

(define simple-assign-parse
  (lambda ()
    (let* ((lhs (simple-lhs-parse))
           (op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (append (cons (cdr op) lhs) (list (simple-value-parse)))
          (error 'parser "Unknown assignment operator")))))

; parse the left hand side of an assignment.  Only variables are allowed.

(define simple-lhs-parse
  (lambda ()
    (let ((lhs (get-next-symbol)))
      (if (eq? (car lhs) 'ID)
         (list (cdr lhs))
         (error 'parser "Illegal left hand side of assignment")))))

; parse a value.  The top level of the parse is the assignment operator.

(define simple-value-parse
  (lambda ()
    (let* ((lhs (get-next-symbol))
           (op (get-next-symbol)))
      (if (and (eq? (car lhs) 'ID) (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (list (cdr op) (cdr lhs) (simple-value-parse))
          (begin
            (unget-next-symbol)
            (simple-orterm-parse lhs))))))

; continuing parsing the value.  The second level is the OR operator

(define simple-orterm-parse
  (lambda (firstsymbol)
    (simple-orterm-parse-helper (simple-andterm-parse firstsymbol))))

; parse the OR expression.

(define simple-orterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '\|\|))
          (simple-orterm-parse-helper (list '|| firstoperand (simple-andterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; the third level is the AND expression

(define simple-andterm-parse
  (lambda (firstsymbol)
    (simple-andterm-parse-helper (simple-equalterm-parse firstsymbol))))

; parse the AND expression.

(define simple-andterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '&&))
          (simple-andterm-parse-helper (list (cdr op) firstoperand (simple-equalterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; the next level is the equal operators

(define simple-equalterm-parse
  (lambda (firstsymbol)
    (simple-equalterm-parse-helper (simple-compareterm-parse firstsymbol))))

; parse the equals expression.

(define simple-equalterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '==) (eq? (cdr op) '!=)))
          (simple-equalterm-parse-helper (list (cdr op) firstoperand (simple-compareterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; next we have the comparison operators

(define simple-compareterm-parse
  (lambda (firstsymbol)
    (simple-compareterm-parse-helper (simple-addterm-parse firstsymbol))))

; parse the comparison expression.

(define simple-compareterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '<) (eq? (cdr op) '<=) (eq? (cdr op) '>) (eq? (cdr op) '>=)))
          (simple-compareterm-parse-helper (list (cdr op) firstoperand (simple-addterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; continue parsing the value.  The next level is the addition and subtraction operators.

(define simple-addterm-parse
  (lambda (firstsymbol)
    (simple-addterm-parse-helper (simple-multterm-parse firstsymbol))))

; parse the addition expression.

(define simple-addterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '+) (eq? (cdr op) '-)))
          (simple-addterm-parse-helper (list (cdr op) firstoperand (simple-multterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; continue parsing the value.  The next level is the multiplication and division operators.

(define simple-multterm-parse
  (lambda (firstsymbol)
    (simple-multterm-parse-helper (simple-operand-parse firstsymbol))))

; parse the multiplication expression.

(define simple-multterm-parse-helper
  (lambda (firstoperand)
     (let ((op (get-next-symbol)))
       (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '*) (eq? (cdr op) '/) (eq? (cdr op) '%)))
           (simple-multterm-parse-helper (list (cdr op) firstoperand (simple-operand-parse (get-next-symbol))))
           (begin
             (unget-next-symbol)
             firstoperand)))))

; continue parsing the value.  The final level is the unary operators, variables, numbers, and nested parentheses.

(define simple-operand-parse
  (lambda (firstsymbol)
     ;(let ((firstsymbol (get-next-symbol)))
       (cond
          ((eq? (car firstsymbol) 'LEFTPAREN)
             (let ((retvalue (simple-value-parse)))
               (if (eq? (car (get-next-symbol)) 'RIGHTPAREN)
                  retvalue
                  (error 'parser "Unmatched left parenthesis"))))
          ((and (eq? (car firstsymbol) 'BINARY-OP) (eq? (cdr firstsymbol) '-)) (list '- (simple-operand-parse (get-next-symbol))))  ; this is a new line
          ((and (eq? (car firstsymbol) 'BINARY-OP) (eq? (cdr firstsymbol) '!)) (list '! (simple-operand-parse (get-next-symbol))))  ; this is a new line
          ((eq? (car firstsymbol) 'NUMBER) (cdr firstsymbol))
          ((eq? (car firstsymbol) 'ID) (cdr firstsymbol))
          ((eq? (car firstsymbol) 'BOOLEAN) (cdr firstsymbol))
          (else (error 'parser "Unknown statmement")))));)


