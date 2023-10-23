#lang racket
(require data/either)
         
;tokenize a list of strings
(define (token string)
  (begin
    (cond
      [(equal? ":=" string) 'equals]
      [(equal? "$$" string) 'eof]
      [(equal? "if" string) 'if]
      [(equal? "read" string) 'read]
      [(equal? "write" string) 'write]
      [(equal? "goto" string) 'goto]
      [(equal? "gosub" string) 'gosub]
      [(equal? "return" string) 'return]
      [(equal? "(" string) 'l-parens]
      [(equal? ")" string) 'r-parens]
      [(equal? "+" string) 'plus]
      [(equal? "-" string) 'minus]
      [(equal? "*" string) 'mult]
      [(equal? "/" string) 'div]
      [(equal? ";" string) 'semicolon]
      [(ID? string) 'id]
      [(numbers? string) 'num]
      [else
       'unknown])))
(provide token)

;index numbers 
(define (index? word)
  (and (not (equal? (string-ref word 0) #\0))
       (numbers? word)))
(provide index?)

;Identifiers
(define (ID? word)
  (match word
    [(regexp #rx"^([a-zA-Z]+)$") #t]
    [else #f]))
(provide ID?)

;integer values
(define (numbers? word)
  (or (regexp-match? #rx"^[1-9][0-9]*$" word)
      (equal? word "0")))
(provide numbers?)

;mathematical operators
(define (operator? character)
  (regexp-match? #rx"[+-]" (string character))
  (regexp-match? #rx"[*/]" (string character)))

;nonzero_digits
(define (non-zero-digit? character)
  (regexp-match? #rx"^[1-9]$" (string character)))

;Digits
(define (digit? character)
  (or (char=?  character) #\0) (non-zero-digit? character))