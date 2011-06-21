;; test for the new (0-255) fixnums

(define x 255)
(define (loop x)
  (if (> x 0)
      (begin (displayln x)
	     (loop (- x 1)))))
(loop x)
