(define l '(0 1 2 3 4 5 6))

(define l2 (cons 2 3))

(set-car! l2 1)
(display (car l2))

;; ;; (define l2 (cons (list-ref l 4) 2)) ; (list-ref l (getchar))

;(led l) ;; invalid, but just to use l

(define x 9)
;(led x)

(display (list-ref l 6)) ;; TODO but (car l2) is ok, cdr too, but l and l2 fail, l2 with a segfault, since it's a pair and not a list, list bugs too but not as spectacular
;; TODO louche, si on fait list-ref de 1, ca marche, 4 affiche rien, 6 segfaulte
