(define x (time->seconds (current-time)))
(display x)
(sleep 700)
(display (- (time->seconds (current-time)) x))
