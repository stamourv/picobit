#lang racket

(require racket/runtime-path syntax/parse
         srfi/4)

(provide read-program)

;; at this point, the #u or #u8(...) has already been seen
(define (read-u8vector port)
  (unless (and (equal? (read-char port) #\8)
               (equal? (read-char port) #\())
    (error "bad byte vector syntax"))
  (let ([s (open-output-string)])
    (let loop ([c (read-char port)]
               [n 4]) ; how many characters we've seen (so far, "#u8(")
      ;; parse until the closing paren
      (cond [(eof-object? c)
             (error "bad byte vector syntax")]
            [(not (equal? c #\)))
             (display c s)
             (loop (read-char port) (add1 n))]
            [else
             ;; we saw the closing paren, we're done
             (let ([contents (regexp-split #px"[[:space:]]+"
                                           (get-output-string s))])
               (values (list->u8vector
                        (map string->number contents))
                       n))]))))

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
      (define-values (vec span) (read-u8vector port))
      (datum->syntax #'here
                     vec
                     (list src line col pos span))])))

(define (expand-includes exprs)
  #`(#,@(map
         (syntax-parser
           ;; This is a hack. Eventually, we should have the Racket expander
           ;; take care of includes.
           [(include file)
            #:when (eq? (syntax->datum #'include) 'include)
            #`(begin
                #,@(expand-includes
                    (let ([in (open-input-file (syntax->datum #'file))])
                      (port-count-lines! in)
                      (port->list (lambda (p) (read-syntax 'here p)) in))))]
           [e #'e])
         (if (syntax? exprs) (syntax->list exprs) exprs))))

(define-runtime-path compiler-dir ".")

(define (read-program port)
  (parameterize ([current-readtable u8vector-readtable])
    (define (read-lib f)
      (define in (open-input-file (build-path compiler-dir f)))
      (port-count-lines! in)
      (port->list (lambda (p) (read-syntax 'here p)) in))
    (define library
      #`(#,@(read-lib "library.scm")       ; architecture-independent
         #,@(read-lib "gen.library.scm"))) ; architecture-dependent
    (port-count-lines! port)
    (define prog
      (expand-includes
       #`(#,@library
          #,@(port->list (lambda (p) (read-syntax 'here p)) port))))
    (datum->syntax
     #'here ; to get the Racket bindings for define and co, for syntax-parse
     (syntax->datum prog)
     prog))) ; for source location
