;; to test fixnum boundary conditions

(define x 256)
(define y 257)

(display (> x y))
(display (< x y))
(display (> x 255))
(newline)
(display (+ x y))
(newline)
(display (+ x 1))
(display (= (+ x 1) y))
(newline)
(display (- y 1))
(display (= x (- y 1)))
(newline)
(display (- x 1))
(newline)
