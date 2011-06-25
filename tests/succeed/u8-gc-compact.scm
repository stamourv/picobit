;; test that should trigger vector heap compaction

;; this allocation pattern should cause compaction:
;; vector of size 1, size 2, size 3, etc. dropping each of these right away
;; by the time we fill up the vector space, say when allocating for size n,
;; the heap will be full of blocks of size at most n-1
;; gc will be triggered, all the smaller vectors will be freed, but the
;; heap will be too fragmented to accomodate a vector of size n, triggering
;; compaction
;; if gc triggers before vector space is full, that's fine too. none of the
;; freed blocks will be reusable, since they will all be too small for the
;; next vector to be allocated

;; for a 32k vector heap, 300 vectors should be enough
(define l (list (make-u8vector 30 3)))
(define x (make-u8vector 30 67))
(define (loop i v)
  (if (< i 500)
      (let ([v2 (make-u8vector i (modulo i 256))])
        (if (= (modulo i 50) 0) ; save some, to not always free everything
            (set! l (cons v2 l)))
        (if (not (= (u8vector-ref v2 (- i 1)) (modulo i 256)))
            (displayln "BAD"))
        (loop (+ i 1) v2))))
(loop 1 (make-u8vector 1 3))
(displayln "DONE")
(displayln (u8vector-ref x 17))
(displayln (map (lambda (x) (u8vector-ref x 0)) l))
