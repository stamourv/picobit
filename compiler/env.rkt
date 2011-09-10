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

(define-struct primitive
  (nargs
   (constant-folder #:mutable) ; added post-creation
   eta-expansion ; for higher-order uses
   unspecified-result?)
  #:transparent)

(define allow-forward-references? (make-parameter #t))

(define/contract (env-lookup env id) ((mlistof var?) symbol? . -> . var?)
  (or (for/first ([b (in-mlist env)]
                  #:when (eq? (var-id b) id))
        b)
      ;; We didn't find it. If reasonable to do so, add it to the env.
      ;; This makes it possible to have forward references at the top level.
      (let ([x (make-var id #t '() '() '() #f #f)])
        (unless (allow-forward-references?)
          (compiler-error "variable referenced before its definition:" id))
        (mappend! env (mlist x))
        x)))

(define/contract (env-extend env ids def)
  ((mlistof var?) (listof symbol?) any/c . -> . (mlistof var?))
  (mappend (list->mlist
            (map (lambda (id)
                   (make-var id #f '() '() (list def) #f #f))
                 ids))
           env))
