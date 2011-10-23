;; this is to make sure an optimization happens, so it needs to be checked
;; manually

(define (foo x y)
  (bar x y))
(define (bar x y)
  (baz x y))
(define (baz x y)
  (#%+ x y))

(displayln (foo 3 4)) ; should be calling #%+ directly
(displayln (foo (foo 1 2) 3)) ; same here
