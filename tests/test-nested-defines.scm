(define foo
  (lambda (x)
    
    (bar x 3)))
(define bar
      (lambda (x y)
	(+ x y 2)))
;; no nested defines