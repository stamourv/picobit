;; test processes

(start-first-process
 (lambda ()
   (let loop ((a 1))
     (begin
       (spawn
        (lambda ()
          (let loop2 ((a2 0))
            (yield)
            (displayln a2)
            (if (= a a2) (exit)
                (loop2 (+ a2 1))))))
       (if (= a 10)
           (exit)
           (loop (+ a 1)))))))
(displayln "DONE")
(start-first-process
 (lambda ()
   (let loop ((a 1))
     (begin
       (spawn
        (lambda ()
          (let loop2 ((a2 0))
            (yield)
            (displayln a2)
            (if (= a a2) #f
                (loop2 (+ a2 1))))))
       (if (= a 10)
           (exit)
           (loop (+ a 1)))))))
(displayln "DONE")
