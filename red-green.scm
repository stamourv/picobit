; File: "red-green.scm"

(define loop
  (lambda ()

    (led 'green)  ; set LED to green
    (putchar #\G) ; send a "G" to the console
    (led 'off)    ; turn off LED
    (sleep 100)   ; wait 1 second

    (led 'red)    ; set LED to red
    (putchar #\R) ; send an "R" to the console
    (led 'off)    ; turn off LED
    (sleep 100)   ; wait 1 second

    (loop)))      ; repeat

(loop)
