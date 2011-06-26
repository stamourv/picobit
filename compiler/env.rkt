#lang racket

(provide (all-defined-out))
(require racket/mpair)
(require "utilities.rkt")

;; Environment representation.

(define-struct var
  (id
   global?
   (refs #:mutable)
   (sets #:mutable)
   (defs #:mutable)
   (needed? #:mutable)
   primitive)
  #:transparent)

(define-struct primitive (nargs inliner unspecified-result?) #:transparent)

(define-struct renaming (renamings) #:transparent)

(define allow-forward-references? (make-parameter #t))

(define/contract (env-lookup env id) (mlist? symbol? . -> . var?)
  (let loop ((lst env) (id id))
    (let ((b (mcar lst)))
      (cond ((and (renaming? b)
                  (assq id (renaming-renamings b)))
             =>
             (lambda (x)
               (loop (mcdr lst) (cadr x))))
            ((and (var? b)
                  (eq? (var-id b) id))
             b)
            ;; We didn't find it. If reasonable to do so, add it to the
            ;; env. This makes it possible to have forward references
            ;; at the top level.
            ((null? (mcdr lst))
             (unless (allow-forward-references?)
               (compiler-error "variable referenced before its definition:" id))
             (let ((x (make-var id #t '() '() '() #f #f)))
               (set-mcdr! lst (mlist x))
               x))
            (else
             (loop (mcdr lst) id))))))

(define/contract (env-extend env ids def)
  (mlist? (listof symbol?) any/c . -> . mlist?)
  (mappend (list->mlist (map (lambda (id)
                               (make-var id #f '() '() (list def) #f #f))
                             ids))
           env))

(define/contract (env-extend-renamings env renamings)
  (mlist? list? . -> . mlist?)
  (mcons (make-renaming renamings) env))
