(define l '(0 1 2 3 4 5 6))

(define l2 (cons 2 3))

(displayln (car l2))
(set-car! l2 1)
(displayln (car l2))

(define l3 (cons (list-ref l 4) 2))

(displayln (list-ref l 6))

(displayln l3)
