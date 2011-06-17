#lang racket

(provide (all-defined-out))

(define compiler-error
  (lambda (msg . others)
    (display "*** ERROR -- ")
    (display msg)
    (for-each (lambda (x) (display " ") (write x)) others)
    (newline)
    (exit 1)))
