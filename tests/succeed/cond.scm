(define x 3)
(define y 2)
(set! x 5)

(display (cond (#f 2) ((= 1 y) 4) (#t 2)))
;; if all is false, returns an object, acceptable

(display (cond ((> y x) "foo")
	       ((= 1 y) "bar")
	       (else "baz")))
(newline)
