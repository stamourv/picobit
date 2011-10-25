;; this is to make sure an optimization happens, so it needs to be checked
;; manually

(define (foo x y) ; these should all be the same
  (bar x y))
(define (bar x y)
  (baz x y))
(define (baz x y)
  (#%+ x y))

(displayln (foo 3 4)) ; should be calling #%+ directly
(displayln (foo (foo 1 2) 3)) ; same here


(define (f x) (g 3)) ; no, f has 2 args, g has 2
(define (g x) (#%+ x 2))
(displayln (f 2)) ; should be (g 3)

(define (h)   (a 3)) ; no, increases arg count
(define (a x) (#%+ x 2))
(displayln (h))

(define (b x)   (c x 3)) ; yes
(define (c x y) (#%+ x y))
(displayln (b 2)) ; no we'd trade 1 arg for 2
(displayln (b 2)) ; to defeat copy propagation (not single-use)

(define (d x)   (e x 3)) ; yes
(define (e x y) (#%+ x 2))
(displayln (d 2)) ; no, same
(displayln (d 2)) ; same

(define (ff x)   (gg x 3)) ; yes, with constant prop, should just be 5
(define (gg x y) (#%+ y 2))
(displayln (ff 2)) ; no, same
(displayln (ff 2))

(define (hh x y) (#%+ x (aa y 34))) ; yes
(define (aa x y) (#%+ x 3)) ; hh's y is aa's x, tests capture-avoiding subst.
(displayln (hh 2 3)) ; no, one inner arg is non-trivial: (aa y)
(displayln (hh 2 3))
