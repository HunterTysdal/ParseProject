#lang racket
(require "Definitions.rkt")
;(require "path-to-Definitions.rkt")


;line		:    idx stmt linetail
(define (line? line)
  (define tokens (append '(idx) (map token (cdr line))))
  (cond
    [(eof-object? line)
     (displayln "Expecting $$ at end of file, program denied.")]
    [(member 'uknown tokens)
     (display (format "Program denied: Unknown operator found: Line  ~a\n" (car line))) #f]
    [(not (index? (car line)))
     (displayln "Program denied: No line numbers found.") #f]
    [(and (index? (car line)) (stmt? (cdr line) (car line)))
     #t]
    [else
     #f]))

;program	:    linelist $$
(define (program? input)
  (and (lineL? input)
    (equal? (car (last input)) "$$")))
(provide program?)

;linelist		:    line linelist | epsilon
(define (lineL? input)
  (or
   (equal? (car (car input)) "$$")
   (and
    (line? (car input))
    (lineL? (cdr input)))))

;linetail		:    stmt | epsilon
(define (lTail? input)
  #t)

;stmt		:    id = expr | if expr| read id | write expr | goto idx | gosub idx | return
(define (stmt? input L-index)
  (begin
    ;(displayln (format "stmt ~a" input))
    (cond
      [(and (equal? (length input) 1) (equal? (car input) "return"))
       (displayln (format "Valid return statement at line: ~a" L-index))]
      [(and (equal? (length input) 1) (equal? (car input) "break"))
       (displayln (format "Valid break statement at line: ~a" L-index))]
      [(and (> (length input) 1) (equal? (car input) "read") (ID? (cadr input)))
       (displayln (format "Valid read statement at line: ~a" L-index))]
      [(and (> (length input) 1) (equal? (car input) "goto") (index? (cadr input)))
       (displayln (format "Valid goto statement at line: ~a" L-index))]
      [(and (> (length input) 1) (equal? (car input) "gosub") (index? (cadr input)))
       (displayln (format "Valid gosub statement at line: ~a" L-index))]
      [(and (equal? (car input) "write") (expression? (cdr input) L-index))
       (displayln (format "Valid write statement at line: ~a" L-index))]
      [(and (> (length input) 1) (ID? (car input)) (equal? (cadr input) ":=") (expression? (cdr (cdr input)) L-index))
       #t]
      [(and (equal? (car input) "if") (expr-then-stmt? (cdr input) L-index))
       (displayln (format "Valid if statement at line: ~a" L-index))]
      [else
       (displayln (format "Program Denied: Invalid Statment grammar at line: ~a" L-index))
       #f])))

;expr:    id etail | num etail | ( expr )
(define (expression? input L-index)
  (begin
    (cond
      [(empty? (car input))
       (displayln (format "Valid expression at line: ~a" L-index))]
      [(and (ID? (car input)) (etail? (cdr input) L-index))
       (displayln (format "Valid expression at line: ~a" L-index))]
      [(and (numbers? (car input)) (etail? (cdr input) L-index))
       (displayln (format "Valid expression at line: ~a" L-index))]
      [(and (equal? (car input) "(") (expr-then-parens? (cdr input) L-index))
       (displayln (format "Valid expression at line: ~a" L-index))]
      [else
       (displayln (format "Program Denied: Improper expression found: Line  ~a" L-index))
       #f])))

;etail		:    + expr | - expr | * expr | / expr | = expr | epsilon
(define (etail? input L-index)
  (begin
    (cond
      [(empty? input)
       #t]
      [(and (equal? (car input) "+") (expression? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "-") (expression? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "*") (expression? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "/") (expression? (cdr input) L-index))
       #t]
      [(and (equal? (car input) ":=") (expression? (cdr input) L-index))
       #t]
      [else
       (displayln (format "Program Denied: invalid etail on line ~a" L-index))
       #f])))

(define (expr-then-parens? input L-index)
  (begin
    (cond
      [(empty? (car input))
       #t]
      [(and (ID? (car input)) (etail-then-parens? (cdr input) L-index))
       #t]
      [(and (numbers? (car input)) (etail-then-parens? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "(") (expr-then-parens? (cdr input) L-index))
       #t]
      [else
       (displayln (format "Program Denied: invalid expression at line ~a" L-index))
       #f])))

(define (etail-then-parens? input L-index)
  (begin
    ;(displayln (format "etail ~a" input))
    (cond
      [(empty? input)
       #t]
      [(equal? (car input) ")")
       #t]
      [(and (equal? (car input) "+") (expr-then-parens? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "-") (expr-then-parens? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "*") (expr-then-parens? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "/") (expr-then-parens? (cdr input) L-index))
       #t]
      [(and (equal? (car input) ":=") (expr-then-parens? (cdr input) L-index))
       #t]
      [else
       (displayln (format "Improper etail on line ~a" L-index))
       #f])))


(define (expr-then-stmt? input L-index)
  (begin
    (cond
      [(empty? (car input))
       #t]
      [(and (ID? (car input)) (etailToStmt (cdr input) L-index))
       #t]
      [(and (numbers? (car input)) (etailToStmt (cdr input) L-index))
       #t]
      [else
       (displayln (format "Program Denied: invalid expression to statement at line ~a" L-index))
       #f])))

(define (etailToStmt input L-index)
  (begin
    ;(displayln (format "etail-then-stmt ~a" input))
    (cond
      [(empty? input)
       #t]
      [(and (equal? (car input) "+") (expr-then-stmt? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "-") (expr-then-stmt? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "*") (expr-then-stmt? (cdr input) L-index))
       #t]
      [(and (equal? (car input) "/") (expr-then-stmt? (cdr input) L-index))
       #t]
      [(and (equal? (car input) ":=") (expr-then-stmt? (cdr input) L-index))
       #t]
      [else
       (displayln (format "Program Denied: invalid etail grammar at line ~a" L-index))
       #f])))


