;; this test is to make sure the inliner does not go in an infinite loop
(define (f x) (f x))

(define (g x) (h x))
(define (h x) (g x))
