;; tests yield

(define (infinity)
  (let loop ((a 1))
    (begin 
      (yield a)
      (loop (+ a 1)))))

(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))
(display (infinity))