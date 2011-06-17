#lang racket

; File: "robot.scm", Time-stamp: <2006-03-01 15:57:44 feeley>

; Copyright (C) 2006 by Marc Feeley, All Rights Reserved.

; usage: usage: robot [[BASE_HOSTNAME|.] ID [HEX_FILE]]

(define debug? #f)

;------------------------------------------------------------------------------

(define default-base "localhost") ; internet address of base-station server
(define port-number 12345)

;------------------------------------------------------------------------------

(define version-addr 6)
(define program-filename "robot.hex")
(define program-start-addr #x2000)

(define serial-port-name "com1") ; default, works for Windows
(define serial-port-name "rs232") ; ADDED now to named pipe

(let loop ((lst '("/dev/cu.USA28X181P1.1"
                  "/dev/cu.USA28X181P2.2"
                  "/dev/cu.USA28X191P1.1"
                  "/dev/cu.USA28X191P2.2"
                  "/dev/ttyS0"
                  "/dev/ttyS1")))
  (if (not (null? lst))
      (let ((name (car lst)))
        (if (file-exists? name)
            (set! serial-port-name name)
            (loop (cdr lst))))))

;------------------------------------------------------------------------------

(define log-file
  (and debug?
       (with-exception-catcher
        (lambda (exc)
          #f)
        (lambda ()
          (open-output-file (list path: "robot.log" buffering: 'line))))))

;------------------------------------------------------------------------------

(current-user-interrupt-handler exit)

(define (main . args)

  (define (usage)
    (display "usage: robot [[BASE_HOSTNAME|.] ID [HEX_FILE]]\n"))

  (define (parse-arg1 args)
    (if (null? args)
        (parse-arg4 #f #f #f)
        (let ((arg (car args)))
          (if (exact-int? (string->number arg))
              (parse-arg2 default-base args)
              (parse-arg2 arg (cdr args))))))

  (define (parse-arg2 base args)
    (if (null? args)
        (usage)
        (let ((arg (string->number (car args))))
          (if (and (exact-int? arg)
                   (>= arg 0)
                   (< arg nb-ids))
              (parse-arg3 base arg (cdr args))
              (usage)))))

  (define (parse-arg3 base id args)
    (if (null? args)
        (parse-arg4 base id #f)
        (let ((arg (car args)))
          (if (null? (cdr args))
              (parse-arg4 base id arg)
              (usage)))))

  (define (parse-arg4 base id filename)
    (if id
        (start-client base id filename program-start-addr)
        (start-base)))

  (parse-arg1 args))

(define (exact-int? x)
  (and (integer? x) (exact? x)))

(define (start-base)
  (multiplex)
  (let ((connection-queue
         (open-tcp-server
          (list port-number: port-number
                reuse-address: #t
                eol-encoding: 'cr-lf))))
    (let loop ()
      (serve (read connection-queue))
      (loop))))

(define (start-client base id filename start-addr)
  (set! program-start-addr start-addr)
  (let ((connection
         (if (string=? base ".")
             (receive (client server) (open-string-pipe)
               (multiplex)
               (serve server)
               client)
             (open-tcp-client
              (list server-address: base
                    port-number: port-number
                    eol-encoding: 'cr-lf)))))
    (send (list id
                (with-exception-catcher
                 (lambda (exc)
                   "???")
                 (lambda ()
                   (host-info-name (host-info "")))))
          connection)
    (let ((ack (read connection)))
      (if (equal? ack '(ok))
          (begin
            (if filename
                (begin
                  (set! program-filename filename)
                  (start-client-upload connection id)))
            (start-client-console connection id))
          (display
           (list "Another client is already connected to robot " id "\n"))))
    (close-port connection)))

(define (start-client-upload connection id)
  (let ((mem (read-hex-file program-filename)))
    (if mem
        (upload connection id mem program-start-addr))))

(define (start-client-console connection id)

  (define (restart-robot)
    (restart connection id)
    (start-client-console connection id))

  (define (upload-again)
    (start-client-upload connection id)
    (start-client-console connection id))

  (define (stop-robot)
    (stop connection id)
    (start-client-console connection id))

  (display
   (list "###\n### Console:\n"))
  (let ((input (repl-input-port)))
    (if (tty? input)
        (tty-mode-set! input #t #t #t #f 0)))
  (let ((input (repl-input-port))
        (can-send-key #t)
        (tx-seq-num 0)
        (rx-seq-num 0))
    (let loop1 ((state 0))
      (input-port-timeout-set! connection 0.01)
      (let ((x (read connection)))
        (if (not (eof-object? x))
            (cond ((or (eq? x 'err)
                       (eq? x 'noerr))
                   (set! can-send-key #t))
                  (else
                   (if debug? (pp x))
                   (if (u8vector? x)
                       (if (and (>= (u8vector-length x) 3)
                                (= (quotient (u8vector-ref x 0) nb-ids)
                                   MSG_TYPE_STDIO))
                           (let ((seq-num (u8vector-ref x 1)))
                             (if (not (= seq-num rx-seq-num))
                                 (begin
                                   (set! rx-seq-num seq-num)
                                   (let loop2 ((i 2))
                                     (if (< i (u8vector-length x))
                                         (let ((n (u8vector-ref x i)))
                                           (cond ((= n 10)
                                                  (display "\n"))
                                                 ;((= n 13)
                                                  ;(display "\n"))
                                                 ((or (< n 32) (> n 126))
                                                  (display
                                                   (list "<" n ">")))
                                                 (else
                                                  (write-char (integer->char n))))
                                           (loop2 (+ i 1))))))))
                           (write (u8vector->list x))))))))
      (if can-send-key
          (begin
            (input-port-timeout-set! input 0.01)
            (let ((x (read-char input)))

              (define (got x)
                (send
                 (vector 'send-message
                         (+ id (* nb-ids MSG_TYPE_STDIO))
                         (u8vector tx-seq-num
                                   (char->integer x)))
                 connection)
                (set! can-send-key #f)
                (set! tx-seq-num (modulo (+ tx-seq-num 1) 256))
                (loop1 0))

              (if (char? x)
                  (cond ((char=? x #\tab)
                         (upload-again))
                        (else
                         (cond ((= state 0)
                                (cond ((char=? x #\u001b)
                                       (loop1 1))
                                      (else
                                       (got x))))
                               ((= state 1)
                                (cond ;((char=? x #\u001b))
                                      ((char=? x #\[)
                                       (loop1 3))
                                      ((char=? x #\O)
                                       (loop1 2))
                                      (else
                                       (got x))))
                               ((= state 2)
                                (cond ((char=? x #\P) ; F1
                                       (stop-robot))
                                      ((char=? x #\Q) ; F2
                                       (restart-robot))
                                      ((char=? x #\R) ; F3
                                       (upload-again))
                                      ((char=? x #\S) ; F4
                                       )
                                      (else
                                       #f)))
                               (else
                                (cond ((char=? x #\A)
                                       (got #\u008d))
                                      ((char=? x #\B)
                                       (got #\u008f))
                                      ((char=? x #\C)
                                       (got #\u008e))
                                      ((char=? x #\D)
                                       (got #\u008c))
                                      (else
                                       (got x)))))))
                  (loop1 state))))
          (loop1 state)))))

;------------------------------------------------------------------------------

(define (read-hex-file filename)

  (define addr-width 32)

  (define (syntax-error)
    (error "Improper HEX file"))

  (let ((f
         (with-exception-catcher
          (lambda (exc)
            #f)
          (lambda ()
            (open-input-file filename)))))

    (define mem (make-vector 16 #f))

    (define (mem-store! a b)
      (let loop ((m mem)
                 (a a)
                 (x (- addr-width 4)))
        (if (= x 0)
            (vector-set! m a b)
            (let ((i (arithmetic-shift a (- x))))
              (let ((v (vector-ref m i)))
                (loop (or v
                          (let ((v (make-vector 16 #f)))
                            (vector-set! m i v)
                            v))
                      (- a (arithmetic-shift i x))
                      (- x 4)))))))

    (define (mem->list)

      (define (f m a n tail)

        (define (g i a n tail)
          (if (>= i 0)
              (g (- i 1) (- a n) n (f (vector-ref m i) a n tail))
              tail))

        (if m
            (if (= n 1)
                (cons (cons (- a 1) m) tail)
                (g 15 a (quotient n 16) tail))
            tail))

      (f mem (expt 2 addr-width) (expt 2 addr-width) '()))

    (define hi16
      0)

    (define (read-hex-nibble)
      (let ((c (read-char f)))
        (cond ((and (char>=? c #\0) (char<=? c #\9))
               (- (char->integer c) (char->integer #\0)))
              ((and (char>=? c #\A) (char<=? c #\F))
               (+ 10 (- (char->integer c) (char->integer #\A))))
              ((and (char>=? c #\a) (char<=? c #\f))
               (+ 10 (- (char->integer c) (char->integer #\a))))
              (else
               (syntax-error)))))
             
    (define (read-hex-byte)
      (let* ((a (read-hex-nibble))
             (b (read-hex-nibble)))
        (+ b (* a 16))))

    (if f
        (begin
          (let loop1 ()
            (let ((c (read-char f)))
              (cond ((not (char? c)))
                    ((or (char=? c #\linefeed)
                         (char=? c #\return))
                     (loop1))
                    ((not (char=? c #\:))
                     (syntax-error))
                    (else
                     (let* ((len (read-hex-byte))
                            (a1 (read-hex-byte))
                            (a2 (read-hex-byte))
                            (type (read-hex-byte)))
                       (let* ((adr (+ a2 (* 256 a1)))
                              (sum (+ len a1 a2 type)))
                         (cond ((= type 0)
                                (let loop2 ((i 0))
                                  (if (< i len)
                                      (let ((a (+ adr (* hi16 65536)))
                                            (b (read-hex-byte)))
                                        (mem-store! a b)
                                        (set! adr (modulo (+ adr 1) 65536))
                                        (set! sum (+ sum b))
                                        (loop2 (+ i 1))))))
                               ((= type 1)
                                (if (not (= len 0))
                                    (syntax-error)))
                               ((= type 4)
                                (if (not (= len 2))
                                    (syntax-error))
                                (let* ((a1 (read-hex-byte))
                                       (a2 (read-hex-byte)))
                                  (set! sum (+ sum a1 a2))
                                  (set! hi16 (+ a2 (* 256 a1)))))
                               (else
                                (syntax-error)))
                         (let ((check (read-hex-byte)))
                           (if (not (= (modulo (- sum) 256) check))
                               (syntax-error)))
                         (let ((c (read-char f)))
                           (if (or (not (or (char=? c #\linefeed)
                                            (char=? c #\return)))
                                   (not (= type 1)))
                               (loop1)))))))))

          (close-input-port f)

          (mem->list))
        (begin
          (display
           (list "\n### The file " filename " does not exist\n"))
          #f))))

(define (upload connection id mem start-addr)

  (define max-programmable-address 65535)

  (define bp 8) ; program block size
  (define be 64) ; erase block size

  (if (start-programming connection id)
      (begin
        (let loop1 ((last-erased-be -1)
                    (lst mem))
          (if (pair? lst)
              (let* ((x (car lst))
                     (a (car x))
                     (a-bp (quotient a bp))
                     (a-be (quotient a be))
                     (bp-bytes (make-u8vector bp 255)))
                (if (<= a max-programmable-address)
                    (if (or (= a-be last-erased-be)
                            (let ((a (* a-be be)))
                              (or (< a start-addr)
                                  (erase-block connection id a))))
                        (begin
                          (u8vector-set! bp-bytes (modulo a bp) (cdr x))
                          (let loop2 ((lst2 (cdr lst)))
                            (if (and (pair? lst2)
                                     (let ((a (car (car lst2))))
                                       (and (<= a max-programmable-address)
                                            (= (quotient a bp) a-bp))))
                                (begin
                                  (u8vector-set! bp-bytes
                                                 (modulo (car (car lst2)) bp)
                                                 (cdr (car lst2)))
                                  (loop2 (cdr lst2)))
                                (if (let ((a (* a-bp bp)))
                                      (or (< a start-addr)
                                          (program-block connection id a bp-bytes)))
                                    (loop1 a-be
                                           lst2))))))
                    (reboot connection id)))
              (reboot connection id))))))

(define (request cmd connection)
  (let loop ((n 10))
    (if (> n 0)
        (begin
          (display ".")
          (let ((x (request-once cmd connection)))
            (if (eq? x 'err)
                (begin
                  (thread-sleep! 2)
                  (loop (- n 1)))
                (begin
                  (display "\n")
                  #t))))
        (begin
          (display " ERROR!\n")
          #f))))

(define (request-once cmd connection)
  (send cmd connection)
  (let loop ()
    (let ((x (read connection)))
      (cond ((or (eq? x 'err)
                 (eq? x 'noerr))
             x)
            (else
             (loop))))))

(define (request-version connection id)

  (define (version-msg? version)
    (and (u8vector? version)
         (= (u8vector-length version) 5)
         (= (u8vector-ref version 1)
            (quotient version-addr 256))
         (= (u8vector-ref version 2)
            (modulo version-addr 256))))

  (define (return x)
    (input-port-timeout-set! connection #f)
    x)

  (request-once (vector 'set-program-mode id) connection)
  (send
   (vector 'send-message
           (+ id (* nb-ids MSG_TYPE_PROGRAM))
           (u8vector (quotient version-addr 256)
                     (modulo version-addr 256)
                     1))
   connection)
  (input-port-timeout-set! connection 1)
  (let loop ((ack #f)
             (version #f))
    (let ((x (read connection)))
      (cond ((eof-object? x)
             (if ack
                 (return #f)
                 (loop ack version)))
            ((or (eq? x 'err)
                 (eq? x 'noerr))
             (if version
                 (return version)
                 (loop #t #f)))
            (else
             (if (version-msg? x)
                 (if ack
                     (return x)
                     (loop #f x))
                 (loop ack version)))))))

(define (send obj port)
  (write obj port)
  (newline port)
  (force-output port))

(define (start-programming connection id)
  (display
   (list "\n### Programming robot " id " with " program-filename))
  (enter-program-mode connection id))

(define (stop connection id)
  (display
   (list "\n### Stopping robot " id))
  (enter-program-mode connection id))

(define (restart connection id)
  (display
   (list "###\n### Connecting to robot " id))
  (enter-program-mode connection id)
  (reboot connection id))

(define (enter-program-mode connection id)
  (let loop ((n 5))
    (if (> n 0)
        (begin
          (display ".")
          (let ((version (request-version connection id)))
            (if version
                (let ((version-major (u8vector-ref version 3))
                      (version-minor (u8vector-ref version 4)))
                  (if (and (= version-major 1)
                           (= version-minor 0))
                      (begin
                        (display "\n")
                        #t)
                      (begin
                        (display " INCOMPATIBLE FIRMWARE!\n")
                        #f)))
                (loop (- n 1)))))
        (begin
          (display " THE ROBOT IS NOT RESPONDING!\n")
          #f))))

(define (erase-block connection id addr)
;  (set! addr (+ addr #x2000))
  (display
   (list "###\n### Erasing block 0x"
         (number->string addr 16)))
  (request
   (vector 'send-message
           (+ id (* nb-ids MSG_TYPE_PROGRAM))
           (u8vector (quotient addr 256)
                     (modulo addr 256)))
   connection))

(define (program-block connection id addr bytes)
;  (set! addr (+ addr #x2000))
  (display
   (list "###   Programming block 0x"
         (number->string addr 16)))
  (request
   (vector 'send-message
           (+ id (* nb-ids MSG_TYPE_PROGRAM))
           (u8vector-append
            (u8vector (quotient addr 256)
                      (modulo addr 256))
            bytes))
   connection))

(define (reboot connection id)
  (display
   (list "###\n### Restarting robot"))
  (request
   (vector 'send-message
           (+ id (* nb-ids MSG_TYPE_PROGRAM))
           (u8vector 0 0))
   connection))

;------------------------------------------------------------------------------

; Server side.

(define nb-ids 32)
(define mutex #f)
(define clients #f)
(define multiplexer #f)
(define rs232 #f)

(define (multiplex)

  (set! mutex (make-mutex))
  (set! clients (make-vector nb-ids #f))
  (set! multiplexer (open-vector))

  (set! rs232
        (open-file
         (list path: serial-port-name
               eol-encoding: 'cr-lf)))
  (if (tty? rs232)
      (tty-mode-set! rs232 #f #f #t #t 38400))

  (thread-sleep! 0.1)
  (rs232-flush-input)

  (thread-start!
   (make-thread
    (lambda ()
      (let loop1 ()
        (input-port-timeout-set! multiplexer 0.01)
        (let loop2 ()
          (let ((x (read multiplexer)))
            (if (vector? x)
                (let* ((id (vector-ref x 0))
                       (cmd (vector-ref x 1))
                       (cmd-type (vector-ref cmd 0)))
                  (cond ((eq? cmd-type 'send-message)
                         (let ((dest (vector-ref cmd 1))
                               (bytes (vector-ref cmd 2)))
                         (if (send-message dest bytes)
                             (let ((s (wait-until-end-of-tx)))
                               (cond ((not s)
                                      (send-to-id 'err id)
                                      (loop2))
                                     ((not (= (bitwise-and s NOERR_MASK) 0))
                                      (ir-tx-event-noerr-ack)
                                      (send-to-id 'noerr id)
                                      (loop2))
                                     ((not (= (bitwise-and s ERR_MASK) 0))
                                      (ir-tx-event-err-ack)
                                      (send-to-id 'err id)
                                      (loop2))
                                     (else
                                      (send-to-id 'err id)
                                      (loop2))))
                             (begin
                               (send-to-id 'err id)
                               (loop2)))))
                        ((eq? cmd-type 'set-program-mode)
                         (let ((dest (vector-ref cmd 1)))
                         (if (set-program-mode dest)
                             (let ((s (wait-until-end-of-tx)))
                               (cond ((not s)
                                      (send-to-id 'err id)
                                      (loop2))
                                     ((not (= (bitwise-and s NOERR_MASK) 0))
                                      (ir-tx-event-noerr-ack)
                                      (send-to-id 'noerr id)
                                      (loop2))
                                     ((not (= (bitwise-and s ERR_MASK) 0))
                                      (ir-tx-event-err-ack)
                                      (send-to-id 'noerr id)
                                      (loop2))
                                     (else
                                      (send-to-id 'err id)
                                      (loop2))))
                             (begin
                               (send-to-id 'err id)
                               (loop2)))))
                        (else
                         (loop2))))
                (begin
                  (poll-status-handling-rx)
                  (loop1))))))))))

(define (set-program-mode dest)
  (let ((s (prepare-to-tx)))
    (and s
         (let ((b (+ dest (* nb-ids MSG_TYPE_SET_PROG_MODE))))
           (ir-tx-special (- #xff b) b)))))

(define (send-message dest bytes)
  (let ((s (prepare-to-tx)))
    (and s
         (begin
          (display
           (list "sending to " (modulo dest nb-ids) ": "))
          (write (u8vector->list bytes))
          (display "\n")
          (ir-tx dest bytes)))))

(define (prepare-to-tx)
  (let loop ()
    (let ((s (wait-until-end-of-tx)))
      (cond ((not s)
             #f)
            ((not (= (bitwise-and s NOERR_MASK) 0))
             (ir-tx-event-noerr-ack)
             (loop))
            ((not (= (bitwise-and s ERR_MASK) 0))
             (ir-tx-event-err-ack)
             (loop))
            (else
             s)))))

(define (wait-until-end-of-tx)
  (let loop ()
    (let ((s (poll-status-handling-rx)))
      (cond ((not s)
             #f)
            ((not (= (bitwise-and s TX_MASK) 0))
             (loop))
            (else
             s)))))
            
(define (poll-status-handling-rx)
  (let loop ()
    (let ((s (poll-status)))
      (cond ((not s)
             #f)
            ((not (= (bitwise-and s RX_MASK) 0))
             (handle-rx-message)
             (loop))
            (else
             s)))))
            
(define (handle-rx-message)
  (let ((msg (ir-rx)))
    (if msg
        (let ((id (modulo (u8vector-ref msg 0) nb-ids)))
          (display
           (list "                                          received from " id ": "))
          (write (u8vector->list msg))
          (display "\n")
          (send-to-id msg id)))))

(define (send-to-id msg id)
  (mutex-lock! mutex)
  (let ((client (vector-ref clients id)))
    (if client
        (with-exception-catcher
         (lambda (exc)
           (vector-set! clients id #f))
         (lambda ()
           (send msg client)))))
  (mutex-unlock! mutex))

(define (ir-tx-event-noerr-ack) (send-command-no-cr "n" #t))
(define (ir-tx-event-err-ack)   (send-command-no-cr "e" #t))

(define (send-command-no-cr cmd trace?)
  (and (rs232-send-no-cr cmd trace?)
       (check-ok trace?)))

(define (send-command cmd trace?)
  (and (rs232-send cmd trace?)
       (check-ok trace?)))

(define (check-ok trace?)
  (let ((answer (rs232-read-line trace?)))
    (and (string? answer)
         (= (string-length answer) 1)
         (char=? (string-ref answer 0) #\!))))

(define (byte->string n)
  (define (hex n) (string-ref "0123456789ABCDEF" (modulo n 16)))
  (string (hex (quotient n 16))
          (hex n)))

(define (ir-tx-special byte1 byte2)
  (let ((cmd
         (apply string-append
                "s"
                (map byte->string
                     (list byte1 byte2)))))
    (send-command-no-cr cmd #t)))

(define (ir-tx dest bytes)
  (let ((cmd
         (apply string-append
                "t"
                (map byte->string
                     (cons dest (u8vector->list bytes))))))
    (send-command cmd #t)))

(define (poll-status)
  (and (rs232-send-no-cr "p" #t)
       (let ((answer (rs232-read-line #t)))
         (and (string? answer)
              (= (string-length answer) 3)
              (char=? (string-ref answer 0) #\=)
              (string->number (substring answer 1 3) 16)))))

(define (ir-rx)
  (and (rs232-send-no-cr "r" #t)
       (let ((answer (rs232-read-line #t)))
         (and (string? answer)
              (>= (string-length answer) 3)
              (odd? (string-length answer))
              (char=? (string-ref answer 0) #\=)
              (let ((n (quotient (string-length answer) 2)))
                (let ((v (make-u8vector n 0)))
                  (let loop ((i (- n 1)))
                    (if (>= i 0)
                        (let* ((j (+ (* i 2) 1))
                               (x (string->number
                                   (substring answer j (+ j 2))
                                   16)))
                          (and x
                               (begin
                                 (u8vector-set! v i x)
                                 (loop (- i 1)))))
                        v))))))))

(define MSG_TYPE_ACK           0)
(define MSG_TYPE_SET_PROG_MODE 1)
(define MSG_TYPE_NORMAL        0)
(define MSG_TYPE_PROGRAM       1)
(define MSG_TYPE_STDIO         7)

(define NOERR_MASK 1)
(define ERR_MASK   2)
(define RX_MASK    4)
(define CLOCK_MASK 8)
(define TX_MASK    128)

(define (rs232-flush-input)
  (input-port-timeout-set! rs232 0)
  (read-line rs232 #f))

(define no-response-count 0)

(define (rs232-read-line trace?)
  (input-port-timeout-set! rs232 0.5)
  (let ((x (read-line rs232)))
'
    (if (and debug? trace?)
        (pp (list '(rs232-read-line) '-> x)))
    (if (eof-object? x)
        (begin
          (set! no-response-count (+ no-response-count 1))
          (if (> no-response-count 100)
              (begin
                (pp 'base-station-not-responding)
                (set! no-response-count 50))))
        (begin
          (if (and debug? trace?)
              (begin
                (display "<- ")
                (display x)
                (display "\n")))
          (if (>= no-response-count 50)
              (pp 'base-station-now-ok))
          (set! no-response-count 0)))
    x))

(define (rs232-send-no-check str trace?)
'
  (if (and debug? trace?)
      (pp (list 'rs232-send-no-check str)))
  (display str rs232)
  (display "\r" rs232)
  (force-output rs232)
  (if (and debug? trace?)
      (begin
        (display "-> ")
        (display str)
        (display "\n"))))

(define (rs232-send-no-cr-no-check str trace?)
'
  (if (and debug? trace?)
      (pp (list 'rs232-send-no-cr-no-check str)))
  (display str rs232)
  (force-output rs232)
  (if (and debug? trace?)
      (begin
        (display "-> ")
        (display str)
        (display "\n"))))

(define (rs232-send str trace?)
  (rs232-send-no-check str trace?)
  (let ((echo (rs232-read-line #f)))
    (if (and debug? trace? (string? echo))
        (begin
          (display "<- ")
          (display echo)
          (display "\n")))
    (and (string? echo)
         (string=? echo str))))

(define (rs232-send-no-cr str trace?)
  (rs232-send-no-cr-no-check str trace?)
  (let ((echo (rs232-read-line trace?)))
    (and (string? echo)
         (string=? echo str))))

(define (serve connection)
  (thread-start!
   (make-thread
    (lambda ()
      (let ((id-and-hostname (read connection)))
        (if (and (pair? id-and-hostname)
                 (pair? (cdr id-and-hostname))
                 (null? (cddr id-and-hostname))
                 (exact-int? (car id-and-hostname))
                 (>= (car id-and-hostname) 0)
                 (< (car id-and-hostname) nb-ids))
            (let ((id (car id-and-hostname))
                  (hostname (cadr id-and-hostname)))
              (mutex-lock! mutex)
              (let ((client (vector-ref clients id)))
                (if client
                    (begin
                      (mutex-unlock! mutex)
                      (display
                       (list "============================================= connection to robot " id " from " hostname " **REFUSED**\n"))
                      (if log-file
                          (begin
                            (display
                             (list "============================================= connection to robot " id " from " hostname " **REFUSED**\n")
                             log-file)
                            (force-output log-file)))
                      (close-port connection))
                    (begin
                      (vector-set! clients id connection)
                      (mutex-unlock! mutex)
                      (display
                       (list "============================================= connection to robot " id " from " hostname "\n"))
                      (if log-file
                          (begin
                            (display
                             (list "============================================= connection to robot " id " from " hostname "\n")
                             log-file)
                            (force-output log-file)))
                      (send '(ok) connection)
                      (process-client-commands connection id)
                      (mutex-lock! mutex)
                      (vector-set! clients id #f)
                      (mutex-unlock! mutex)
                      (close-port connection)))))))))))

(define (process-client-commands connection id)
  (with-exception-catcher
   (lambda (exc)
     #f)
   (lambda ()
     (let loop ()
       (let ((cmd (read connection)))
         (if (vector? cmd)
             (begin
               (send (vector id cmd) multiplexer)
               (loop))))))))

;------------------------------------------------------------------------------
