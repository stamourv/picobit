(define x 2)
(define y 3)
(define foo (lambda (x) (+ x 8 213 32523)))
(display (foo x))
(display (+ x y))

(define (foo2 x y)
  (if (and x y)
      (+ x y)
      -1))

(display (foo2 x y))
(newline)
