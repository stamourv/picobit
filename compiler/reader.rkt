#lang racket

(require (only-in unstable/port read-all)
         racket/runtime-path
         srfi/4)

(provide read-program)

;; at this point, the #u or #u8(...) has already been seen
(define (read-u8vector port)
  (unless (and (equal? (read-char port) #\8)
               (equal? (read-char port) #\())
    (error "bad byte vector syntax"))
  (let ([s (open-output-string)])
    (let loop ([c (read-char port)])
      ;; parse until the closing paren
      (cond [(eof-object? c)
             (error "bad byte vector syntax")]
            [(not (equal? c #\)))
             (display c s)
             (loop (read-char port))]
            [else
             ;; we saw the closing paren, we're done
             (let ([contents (regexp-split #px"[[:space:]]+"
                                           (get-output-string s))])
               (list->u8vector
                (map string->number contents)))]))))

;; u8vector literals are not natively supported by Racket
(define u8vector-readtable
  (make-readtable
   (current-readtable)
   #\u
   'dispatch-macro
   (case-lambda
     [(char port) ; read
      (read-u8vector port)]
     [(char port src line col pos) ; read-syntax
      (read-u8vector port)])))

(define (expand-includes exprs)
  (map (lambda (e)
         (if (eq? (car e) 'include)
             (cons 'begin
                   (expand-includes
                    (with-input-from-file (cadr e) read-all)))
             e))
       exprs))

(define-runtime-path compiler-dir ".")

(define (read-program port)
  (parameterize ([current-readtable u8vector-readtable])
    (define (read-lib f)
      (with-input-from-file (build-path compiler-dir f)
        read-all))
    (define library
      (append (read-lib "library.scm")       ; architecture-independent
              (read-lib "gen.library.scm"))) ; architecture-dependent
    (expand-includes (append library (read-all read port)))))
