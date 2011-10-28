;; optimization test, must be checked manually

(define x 3) ; should disappear, no non-propagated uses
(displayln x) ; should be inlined
(displayln (#%+ x 2)) ; inlined + constant propagated

(define y 2)
(displayln y) ; no can do, mutation
(set! y 1)

(define a 4) ; transitive chain
(define b a) ; all 3 globals should be gone
(define c b)
(displayln c) ; should be 4

(define aa 5)
(define (f x)
  (+ x aa)) ; make sure it inlines inside functions
(displayln (f 3)) ; f is single use, should be propagated

(define bb (#%+ aa 2)) ; can't prove plain + side-effect oblivious
(displayln (+ bb 4)) ; yes, bb is single-use
