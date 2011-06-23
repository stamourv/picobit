(letrec ((odd?  (lambda (x) (and (not (= x 0)) (even? (- x 1)))))
	 (even? (lambda (x) (or  (= x 0) (odd? (- x 1))))))
  (display (even? 2)))
(newline)

(letrec ((foo (lambda (x) (if (> x 6) x (bar x))))
	 (bar (lambda (x) (foo (+ x 1)))))
  (display (foo 1)))
(newline)
