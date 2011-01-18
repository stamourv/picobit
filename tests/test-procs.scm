;; test processes

(start-first-process 
 (lambda ()
   (let loop ((a 1)) 
     (begin
       (spawn 
        (lambda ()
          (let loop2 ((a2 0)) 
            (if (= a a2) (exit)
                (loop (+ a2 1))))))
       (if (= a 10) 
           '()
           (loop (+ a 1)))))))
 