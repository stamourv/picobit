(let loop ((x 0))
  (if (< x 10)
      (begin (displayln x)
	     (loop (+ x 1)))))
