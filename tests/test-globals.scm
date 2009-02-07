(define x 2) ;; TODO the vm tries to get the car of this
(define y 3)
;; (define foo (lambda (x) (+ x 8 213 32523))) ;; TODO creating a lambda fails
;; (display (foo x))
;; (display (+ x y)) ;; actually, gives the same error as if we had a lambda

(define (foo x y)
  (if (and x y)
      (#%+ x y) ;; TODO if we use this +, it doesn't show as global, probably because it doesn't use rest params
      -1))

(foo x y)
;; duplicates have no effect on the number of constants, and 5 didn't have an effect either, might be too small
