#lang racket

(provide (all-defined-out))

(require srfi/4)

(define (compiler-error msg . others)
  (parameterize ([current-output-port (current-error-port)])
    (printf "*** PICOBIT ERROR -- ~a" msg)
    (for ([x (in-list others)])
      (printf " ~a"
              (if (identifier? x)
                  (format "~a at ~a:~a"
                          (syntax->datum x)
                          (syntax-line   x)
                          (syntax-column x))
                  (format "~s" x))))
    (newline)
    (exit 1)))

(define (self-eval? expr)
  (or (number?   expr)
      (char?     expr)
      (boolean?  expr)
      (string?   expr)
      (u8vector? expr)))


;; to control output level
(define show-size?           (make-parameter #f))
(define show-asm?            (make-parameter #f))
(define show-parsed?         (make-parameter #f))
(define show-post-front-end? (make-parameter #f))
(define stats?               (make-parameter #f))
