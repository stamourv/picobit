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
(define (loop i)
  (if (< i 300)
      (begin
        (if (not (= (u8vector-ref (make-u8vector i 3) (- i 1)) 3))
            (displayln "BAD"))
        (loop (+ i 1)))
      (displayln "DONE")))
(loop 1)
