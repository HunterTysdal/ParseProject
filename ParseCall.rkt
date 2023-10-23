#lang racket
(require "Parse.rkt")
(require "Definitions.rkt")

(define (parse Fname)
  (begin
    (define lineSplit
      (map(lambda (line) (string-split line))
       (file->lines Fname)))
    (program? lineSplit)))

(display "first input:")
(newline)
(parse "Input.txt")

(newline)
(newline)

(display "second input:")
(newline)

(parse "Input2.txt")

(newline)
(newline)

(display "third input:")
(newline)

(parse "Input3.txt")