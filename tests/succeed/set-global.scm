(define *x* 0)

; Any one of the following crashes the picobit compiler:

(let ((x 1)) (set! *x* x))

((lambda () (set! *x* 1)))

(define (picobit-crashes) (set! *x* 1))
(picobit-crashes)

(display *x*)
