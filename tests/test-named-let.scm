(let loop ((x 0))
  (if (< x 10)
      (begin (display x)
	     (loop (+ x 1)))))
