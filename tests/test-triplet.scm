(define x (triplet 3 5 7))
(display x) ;; TODO doesn't display ok, but not really important
(display (fst x))
(display (snd x))
(display (trd x))
(display (triplet? x)) ;; TODO even bools don't diplay
(define iter
  (lambda (x)
    (if (= x 0)
	(begin (display (fst (fst (triplet (triplet x 3 4) 5 6))))
	       (display "\n"))
	(begin (display (fst (fst (triplet (triplet x 3 4) 5 6))))
	       (display "\n")
	       (iter (- x 1))))))
;; (iter 255)
;; TODO nested triplets work, but I don't really know why, I just copied the code from pairs

;; vectors from triplets
(define y (make-u8vector 7 3))
(display (u8vector-ref y 4))

(define z (u8vector 1 2 3 4 5))
(display (u8vector-ref z 0))
(display (u8vector-ref z 1))
(display (u8vector-ref z 2))
(display (u8vector-ref z 3))
(display (u8vector-ref z 4))

(display "foo\n")

(define w (triplet 1 2 3))
(set-fst! w 4)
(set-snd! w 5)
(set-trd! w 6)
(display (fst w))
(display (snd w))
(display (trd w))

(u8vector-set! z 0 10)
(u8vector-set! z 1 11)
(u8vector-set! z 2 12)
(u8vector-set! z 3 13)
(u8vector-set! z 4 14)
(display (u8vector-ref z 0))
(display (u8vector-ref z 1))
(display (u8vector-ref z 2))
(display (u8vector-ref z 3))
(display (u8vector-ref z 4))

(define q (triplet 1 2 '()))
(display (if (null? (trd q))  1 2))

(display "\n")

;; (define r (r-a-list 2 4 6 8 10 12 14 16))
;; (display (r-a-ref r 4))
;; (r-a-set! r 3 15)
;; (display (r-a-ref r 3))

;; (define r (b-list 2 4 6 8 10 12 14 16))
;; (display (b-list-ref r 0))
;; (display (b-list-ref r 1))
;; (display (b-list-ref r 2))
;; (display (b-list-ref r 3))
;; (b-list-set! r 3 15)
;; (display (b-list-ref r 3))

;; (display "\n")
;; (display (bitwise-ior (b-list-ref r 3) (b-list-ref r 7)))
;; (display (bitwise-xor (b-list-ref r 3) (b-list-ref r 4)))

(display "\n")
(display (if (equal? (u8vector 2 3 4) (u8vector 2 3 5)) 1 2))


(display "\n")
