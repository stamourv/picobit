;; let's see if this end up in rom, if so, vectors whose contents are known at compile time are likely to be optimized to rom for free
(define x 3)
(define y '(1 2 3)) ;; ok, if quote is used, goes to rom, if not, goes to ram, the compiler doesn't check to see if everything is known at compile-time, I probably should add this optimization
(set-car! y 9)
(display y)
