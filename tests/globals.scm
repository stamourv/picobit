(define x 2)
(define y 3)
(define foo (lambda (x) (+ x 8 213 32523))) ;; TODO creating a lambda fails
(display (foo x))
(display (+ x y))

(define (foo x y)
  (if (and x y)
      (+ x y)
      -1))

(display (foo x y))
(newline)
