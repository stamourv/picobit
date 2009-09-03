;;; File: "asm.scm"
;;;
;;; This module implements the generic assembler.

;;(##declare (standard-bindings) (fixnum) (block))

(define compiler-internal-error error)

;; (asm-begin! start-pos big-endian?) initializes the assembler and
;; starts a new empty code stream at address "start-pos".  It must be
;; called every time a new code stream is to be built.  The argument
;; "big-endian?" indicates the byte ordering to use for 16, 32 and 64
;; bit values.  After a call to "asm-begin!" the code stream is built
;; by calling the following procedures:
;;
;;  asm-8            to add an 8 bit integer to the code stream
;;  asm-16           to add a 16 bit integer to the code stream
;;  asm-32           to add a 32 bit integer to the code stream
;;  asm-64           to add a 64 bit integer to the code stream
;;  asm-float64      to add a 64 bit IEEE float to the code stream
;;  asm-string       to add a null terminated string to the code stream
;;  asm-label        to set a label to the current position in the code stream
;;  asm-align        to add enough zero bytes to force alignment
;;  asm-origin       to add enough zero bytes to move to a particular address
;;  asm-at-assembly  to defer code production to assembly time
;;  asm-listing      to add textual information to the listing

(define (asm-begin! start-pos big-endian?)
  (set! asm-start-pos start-pos)
  (set! asm-big-endian? big-endian?)
  (set! asm-code-stream (asm-make-stream))
  #f)

;; (asm-end!) must be called to finalize the assembler.

(define (asm-end!)
  (set! asm-code-stream #f)
  #f)

;; (asm-8 n) adds an 8 bit signed or unsigned integer to the code stream.

(define (asm-8 n)
  (asm-code-extend (asm-bits-0-to-7 n)))

;; (asm-16 n) adds a 16 bit signed or unsigned integer to the code stream.

(define (asm-16 n)
  (if asm-big-endian?
    (begin (asm-8 (asm-bits-8-and-up n)) (asm-8 n))
    (begin (asm-8 n) (asm-8 (asm-bits-8-and-up n)))))

;; (asm-32 n) adds a 32 bit signed or unsigned integer to the code stream.

(define (asm-32 n)
  (if asm-big-endian?
    (begin (asm-16 (asm-bits-16-and-up n)) (asm-16 n))
    (begin (asm-16 n) (asm-16 (asm-bits-16-and-up n)))))

;; (asm-64 n) adds a 64 bit signed or unsigned integer to the code stream.

(define (asm-64 n)
  (if asm-big-endian?
    (begin (asm-32 (asm-bits-32-and-up n)) (asm-32 n))
    (begin (asm-32 n) (asm-32 (asm-bits-32-and-up n)))))

;; (asm-float64 n) adds a 64 bit IEEE floating point number to the code stream.

(define (asm-float64 n)
  (asm-64 (asm-float->bits n)))

;; (asm-string str) adds a null terminated string to the code stream.

(define (asm-string str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
        (begin
          (asm-8 (char->integer (string-ref str i)))
          (loop (+ i 1)))
        (asm-8 0)))))

;; (asm-make-label id) creates a new label object.  A label can
;; be queried with "asm-label-pos" to obtain the label's position
;; relative to the start of the code stream (i.e. "start-pos").
;; The argument "id" gives a name to the label (not necessarily
;; unique) and is only needed for debugging purposes.

(define (asm-make-label id)
  (vector 'LABEL #f id))

;; (asm-label label-obj) sets the label to the current position in the
;; code stream.

(define (asm-label label-obj)
  (if (vector-ref label-obj 1)
    (compiler-internal-error
      "asm-label, label multiply defined" (asm-label-id label-obj))
    (begin
      (vector-set! label-obj 1 0)
      (asm-code-extend label-obj))))

;; (asm-label-id label-obj) returns the identifier of the label object.

(define (asm-label-id label-obj)
  (vector-ref label-obj 2))

;; (asm-label-pos label-obj) returns the position of the label
;; relative to the start of the code stream (i.e. "start-pos").
;; This procedure can only be called at assembly time (i.e.
;; within the call to "asm-assemble") or after assembly time
;; for labels declared prior to assembly time with "asm-label".
;; A label declared at assembly time can only be queried after
;; assembly time.  Moreover, at assembly time the position of a
;; label may vary from one call to the next due to the actions
;; of the assembler.

(define (asm-label-pos label-obj)
  (let ((pos (vector-ref label-obj 1)))
    (if pos
      pos
      (compiler-internal-error
        "asm-label-pos, undefined label" (asm-label-id label-obj)))))

;; (asm-align multiple offset) adds enough zero bytes to the code
;; stream to force alignment to the next address congruent to
;; "offset" modulo "multiple".

(define (asm-align multiple offset)
  (asm-at-assembly
    (lambda (self)
      (modulo (- multiple (- self offset)) multiple))
    (lambda (self)
      (let loop ((n (modulo (- multiple (- self offset)) multiple)))
        (if (> n 0)
          (begin
            (asm-8 0)
            (loop (- n 1))))))))

;; (asm-origin address) adds enough zero bytes to the code stream to move
;; to the address "address".

(define (asm-origin address)
  (asm-at-assembly
    (lambda (self)
      (- address self))
    (lambda (self)
      (let ((len (- address self)))
        (if (< len 0)
          (compiler-internal-error "asm-origin, can't move back")
          (let loop ((n len))
            (if (> n 0)
              (begin
                (asm-8 0)
                (loop (- n 1))))))))))

;; (asm-at-assembly . procs) makes it possible to defer code
;; production to assembly time.  A useful application is to generate
;; position dependent and span dependent code sequences.  This
;; procedure must be passed an even number of procedures.  All odd
;; indexed procedures (including the first procedure) are called "check"
;; procedures.  The even indexed procedures are the "production"
;; procedures which, when called, produce a particular code sequence.
;; A check procedure decides if, given the current state of assembly
;; (in particular the current positioning of the labels), the code
;; produced by the corresponding production procedure is valid.
;; If the code is not valid, the check procedure must return #f.
;; If the code is valid, the check procedure must return the length
;; of the code sequence in bytes.  The assembler will try each check
;; procedure in order until it finds one that does not return #f
;; (the last check procedure must never return #f).  For convenience,
;; the current position in the code sequence is passed as the single
;; argument of check and production procedures.
;;
;; Here is a sample call of "asm-at-assembly" to produce the
;; shortest branch instruction to branch to label "x" for a
;; hypothetical processor:
;;
;;  (asm-at-assembly
;;
;;    (lambda (self) ; first check procedure
;;      (let ((dist (- (asm-label-pos x) self)))
;;        (if (and (>= dist -128) (<= dist 127)) ; short branch possible?
;;          2
;;          #f)))
;;
;;    (lambda (self) ; first production procedure
;;      (asm-8 #x34) ; branch opcode for 8 bit displacement
;;      (asm-8 (- (asm-label-pos x) self)))
;;
;;    (lambda (self) 5) ; second check procedure
;;
;;    (lambda (self) ; second production procedure
;;      (asm-8 #x35) ; branch opcode for 32 bit displacement
;;      (asm-32 (- (asm-label-pos x) self))))

(define (asm-at-assembly . procs)
  (asm-code-extend (vector 'DEFERRED procs 0)))

;; (asm-listing text) adds text to the right side of the listing.
;; The atoms in "text" will be output using "display" (lists are
;; traversed recursively).  The listing is generated by calling
;; "asm-display-listing".

(define (asm-listing text)
  (asm-code-extend (vector 'LISTING text)))

;; (asm-assemble) assembles the code stream.  After assembly, the
;; label objects will be set to their final position and the
;; alignment bytes and the deferred code will have been produced.  It
;; is possible to extend the code stream after assembly.  However, if
;; any of the procedures "asm-label", "asm-align", and
;; "asm-at-assembly" are called, the code stream will have to be
;; assembled once more.

(define (asm-assemble)
  (let ((fixup-lst (asm-pass1)))

    (let loop1 ()
      (let loop2 ((lst fixup-lst)
                  (pos asm-start-pos))
        (if (pair? lst)
            (let* ((fixup (car lst))
                   (pos (+ pos (car fixup)))
                   (curr (cdr fixup))
                   (x (car curr)))
              (if (eq? (vector-ref x 0) 'LABEL)
                  ;; LABEL
                  (loop2 (cdr lst) pos)
                  ;; DEFERRED
                  (let ((old-size (vector-ref x 2)))
                    (let loop3 ()
                      (let ((new-size ((car (vector-ref x 1)) pos)))
                        (if new-size
                            (begin
                              (vector-set! x 2 new-size)
                              (loop2 (cdr lst) (+ pos old-size)))
                            (begin
                              (vector-set! x 1 (cddr (vector-ref x 1)))
                              (loop3))))))))
            (let loop4 ((lst fixup-lst)
                        (pos asm-start-pos)
                        (changed? #f))
              (if (pair? lst)
                  (let* ((fixup (car lst))
                         (pos (+ pos (car fixup)))
                         (curr (cdr fixup))
                         (x (car curr)))
                    (if (eq? (vector-ref x 0) 'LABEL)
                        ;; LABEL
                        (if (= (vector-ref x 1) pos)
                            (loop4 (cdr lst) pos changed?)
                            (begin
                              (vector-set! x 1 pos)
                              (loop4 (cdr lst) pos #t)))
                        ;; DEFERRED
                        (let ((new-size (vector-ref x 2)))
                          (loop4 (cdr lst) (+ pos new-size) changed?))))
                  (if changed?
                      (loop1)))))))

    (let loop5 ((prev asm-code-stream)
                (curr (cdr asm-code-stream))
                (pos asm-start-pos))
      (if (null? curr)
          (set-car! asm-code-stream prev)
          (let ((x (car curr))
                (next (cdr curr)))
            (if (vector? x)
                (let ((kind (vector-ref x 0)))
                  (cond ((eq? kind 'LABEL)
                         (let ((final-pos (vector-ref x 1)))
                           (if final-pos
                               (if (not (= pos final-pos))
                                   (compiler-internal-error
                                    "asm-assemble, inconsistency detected"))
                               (vector-set! x 1 pos))
                           (set-cdr! prev next)
                           (loop5 prev next pos)))
                        ((eq? kind 'DEFERRED)
                         (let ((temp asm-code-stream))
                           (set! asm-code-stream (asm-make-stream))
                           ((cadr (vector-ref x 1)) pos)
                           (let ((tail (car asm-code-stream)))
                             (set-cdr! tail next)
                             (let ((head (cdr asm-code-stream)))
                               (set-cdr! prev head)
                               (set! asm-code-stream temp)
                               (loop5 prev head pos)))))
                        (else
                         (loop5 curr next pos))))
                (loop5 curr next (+ pos 1))))))))

;; (asm-display-listing port) produces a listing of the code stream
;; on the given output port.  The bytes generated are shown in
;; hexadecimal on the left side of the listing and the right side
;; of the listing contains the text inserted by "asm-listing".

(define (asm-display-listing port)

  (define text-col 24)
  (define pos-width 6)
  (define byte-width 2)

  (define (output text)
    (cond ((null? text))
          ((pair? text)
           (output (car text))
           (output (cdr text)))
          (else
           (display text port))))

  (define (print-hex n)
    (display (string-ref "0123456789ABCDEF" n) port))

  (define (print-byte n)
    (print-hex (quotient n 16))
    (print-hex (modulo n 16)))

  (define (print-pos n)
    (if (< n 0)
      (display "      " port)
      (begin
        (print-byte (quotient n #x10000))
        (print-byte (modulo (quotient n #x100) #x100))
        (print-byte (modulo n #x100)))))

  (let loop1 ((lst (cdr asm-code-stream)) (pos asm-start-pos) (col 0))
    (if (null? lst)
      (if (> col 0)
        (newline port))
      (let ((x (car lst)))
        (if (vector? x)
          (let ((kind (vector-ref x 0)))
            (cond ((eq? kind 'LISTING)
                   (let loop2 ((col col))
                     (if (< col text-col)
                       (begin
                         (display (integer->char 9) port)
                         (loop2 (* 8 (+ (quotient col 8) 1))))))
                   (output (vector-ref x 1))
                   (newline port)
                   (loop1 (cdr lst) pos 0))
                  (else
                   (compiler-internal-error
                     "asm-display-listing, code stream not assembled"))))
          (if (or (= col 0) (>= col (- text-col byte-width)))
            (begin
              (if (not (= col 0)) (newline port))
              (print-pos pos)
              (display " " port)
              (print-byte x)
              (loop1 (cdr lst) (+ pos 1) (+ (+ pos-width 1) byte-width)))
            (begin
              (print-byte x)
              (loop1 (cdr lst) (+ pos 1) (+ col byte-width)))))))))

;; (asm-write-code filename) outputs the code stream (i.e. the sequence
;; of bytes produced) on the named file.

(define (asm-write-code filename)
  (with-output-to-file filename
    (lambda ()
      (let loop ((lst (cdr asm-code-stream)))
        (if (not (null? lst))
          (let ((x (car lst)))
            (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (if (not (eq? kind 'LISTING))
                  (compiler-internal-error
                    "asm-write-code, code stream not assembled"))
                (loop (cdr lst)))
              (begin
                (write-char (integer->char x))
                (loop (cdr lst))))))))))

(define (asm-write-hex-file filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-hex n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (print-hex (quotient n 16))
        (print-hex (modulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (quotient addr 256))
              (addr-lo (modulo addr 256)))
          (display ":")
          (print-byte n)
          (print-byte addr-hi)
          (print-byte addr-lo)
          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (modulo (- (apply + n addr-hi addr-lo type bytes)) 256)))
            (print-byte sum)
            (newline))))

      (let loop ((lst (cdr asm-code-stream))
                 (pos asm-start-pos)
                 (rev-bytes '()))
        (if (not (null? lst))
          (let ((x (car lst)))
            (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (if (not (eq? kind 'LISTING))
                  (compiler-internal-error
                    "asm-write-hex-file, code stream not assembled"))
                (loop (cdr lst)
                      pos
                      rev-bytes))
              (let ((new-pos
                     (+ pos 1))
                    (new-rev-bytes
                     (cons x
                           (if (= (modulo pos 16) 0)
                               (begin
                                 (print-line 0
                                             (- pos (length rev-bytes))
                                             (reverse rev-bytes))
                                 '())
                               rev-bytes))))
                (loop (cdr lst)
                      new-pos
                      new-rev-bytes))))
          (begin
            (if (not (null? rev-bytes))
                (print-line 0
                            (- pos (length rev-bytes))
                            (reverse rev-bytes)))
            (print-line 1 0 '())
            (if #t
                (begin
                  ;;;(pp (- 3447 (- pos asm-start-pos)));;;;;;;;;;;;

                  (display (- pos asm-start-pos) ##stderr-port)
                  (display " bytes\n" ##stderr-port)))))))))

;; Utilities.

(define asm-start-pos #f)   ; start position of the code stream
(define asm-big-endian? #f) ; endianness to use
(define asm-code-stream #f) ; current code stream

(define (asm-make-stream) ; create an empty stream
  (let ((x (cons '() '())))
    (set-car! x x)
    x))
     
(define (asm-code-extend item) ; add an item at the end of current code stream
  (let* ((stream asm-code-stream)
         (tail (car stream))
         (cell (cons item '())))
    (set-cdr! tail cell)
    (set-car! stream cell)))

(define (asm-pass1) ; construct fixup list and make first label assignment
  (let loop ((curr (cdr asm-code-stream))
             (fixup-lst '())
             (span 0)
             (pos asm-start-pos))
    (if (null? curr)
      (reverse fixup-lst)
      (let ((x (car curr)))
        (if (vector? x)
          (let ((kind (vector-ref x 0)))
            (cond ((eq? kind 'LABEL)
                   (vector-set! x 1 pos) ; first approximation of position
                   (loop (cdr curr) (cons (cons span curr) fixup-lst) 0 pos))
                  ((eq? kind 'DEFERRED)
                   (loop (cdr curr) (cons (cons span curr) fixup-lst) 0 pos))
                  (else
                   (loop (cdr curr) fixup-lst span pos))))
          (loop (cdr curr) fixup-lst (+ span 1) (+ pos 1)))))))

;(##declare (generic))

(define (asm-bits-0-to-7 n) ; return bits 0 to 7 of a signed integer
  (modulo n #x100))

(define (asm-bits-8-and-up n) ; return bits 8 and up of a signed integer
  (if (>= n 0)
    (quotient n #x100)
    (- (quotient (+ n 1) #x100) 1)))

(define (asm-bits-16-and-up n) ; return bits 16 and up of a signed integer
  (if (>= n 0)
    (quotient n #x10000)
    (- (quotient (+ n 1) #x10000) 1)))

(define (asm-bits-32-and-up n) ; return bits 32 and up of a signed integer
  (if (>= n 0)
    (quotient n #x100000000)
    (- (quotient (+ n 1) #x100000000) 1)))

; The following procedures convert floating point numbers into their
; machine representation.  They perform bignum and flonum arithmetic.

(define (asm-float->inexact-exponential-format x)

  (define (exp-form-pos x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (not (< asm-ieee-e-bias i*2))
                        (not (< x y)))
                 (exp-form-pos x (* y y) i*2)
                 (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (not (< asm-ieee-e-bias i+b))
                     (not (< a y)))
              (begin
                (set-car! z (/ a y))
                (set-cdr! z i+b)))
            z)))))

  (define (exp-form-neg x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (< i*2 asm-ieee-e-bias-minus-1)
                        (< x y))
                 (exp-form-neg x (* y y) i*2)
                 (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (< i+b asm-ieee-e-bias-minus-1)
                     (< a y))
              (begin
                (set-car! z (/ a y))
                (set-cdr! z i+b)))
            z)))))

  (define (exp-form x)
    (if (< x asm-inexact-+1)
      (let ((z (exp-form-neg x asm-inexact-+1/2 1)))
        (set-car! z (* asm-inexact-+2 (car z)))
        (set-cdr! z (- -1 (cdr z)))
        z)
      (exp-form-pos x asm-inexact-+2 1)))

  (if (negative? x)
    (let ((z (exp-form (- asm-inexact-0 x))))
      (set-car! z (- asm-inexact-0 (car z)))
      z)
    (exp-form x)))

(define (asm-float->exact-exponential-format x)
  (let ((z (asm-float->inexact-exponential-format x)))
    (let ((y (car z)))
      (cond ((not (< y asm-inexact-+2))
             (set-car! z asm-ieee-+m-min)
             (set-cdr! z asm-ieee-e-bias-plus-1))
            ((not (< asm-inexact--2 y))
             (set-car! z asm-ieee--m-min)
             (set-cdr! z asm-ieee-e-bias-plus-1))
            (else
             (set-car! z
               (truncate (inexact->exact (* (car z) asm-inexact-m-min))))))
      (set-cdr! z (- (cdr z) asm-ieee-m-bits))
      z)))

(define (asm-float->bits x) ; returns the 64 bit integer encoding the float "x"

  (define (bits a b)
    (if (< a asm-ieee-+m-min)
      a
      (+ (- a asm-ieee-+m-min)
         (* (+ (+ b asm-ieee-m-bits) asm-ieee-e-bias)
            asm-ieee-+m-min))))

  (let ((z (asm-float->exact-exponential-format x)))
    (let ((a (car z)) (b (cdr z)))
      (if (negative? a)
        (+ asm-ieee-sign-bit (bits (- 0 a) b))
        (bits a b)))))

; Parameters for ANSI-IEEE Std 754-1985 representation of
; doubles (i.e. 64 bit floating point numbers):

(define asm-ieee-m-bits 52)
(define asm-ieee-e-bits 11)
(define asm-ieee-+m-min 4503599627370496)    ; (expt 2 asm-ieee-m-bits)
(define asm-ieee--m-min -4503599627370496)   ; (- asm-ieee-+m-min)
(define asm-ieee-sign-bit #x8000000000000000); (expt 2 (+ asm-ieee-e-bits asm-ieee-m-bits))

(define asm-ieee-e-bias         1023) ; (- (expt 2 (- asm-ieee-e-bits 1)) 1)
(define asm-ieee-e-bias-plus-1  1024) ; (+ asm-ieee-e-bias 1)
(define asm-ieee-e-bias-minus-1 1022) ; (- asm-ieee-e-bias 1)

(define asm-inexact-m-min (exact->inexact asm-ieee-+m-min))
(define asm-inexact-+2    (exact->inexact 2))
(define asm-inexact--2    (exact->inexact -2))
(define asm-inexact-+1    (exact->inexact 1))
(define asm-inexact-+1/2  (exact->inexact (/ 1 2)))
(define asm-inexact-0     (exact->inexact 0))
