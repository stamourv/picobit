;; optimization test, must be checked manually

(define x 3) ; should disappear, no non-propagated uses
(displayln x) ; should be inlined
(displayln (+ x 2)) ; inlined + constant propagated

(define y 2)
(displayln y) ; no can do, mutation
(set! y 1)
