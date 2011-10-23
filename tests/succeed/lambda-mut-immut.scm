(define (f x y) ; one is mutable, the other is not
  (set! x 4)
  (#%+ x y))
(displayln (f 1 6))
