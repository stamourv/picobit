; File: "picobit.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

; Copyright (C) 2006 by Marc Feeley, All Rights Reserved.

(define-macro (dummy)
  (proper-tail-calls-set! #f)
  #f)
;(dummy)

;-----------------------------------------------------------------------------

(define compiler-error
  (lambda (msg . others)
    (display "*** ERROR -- ")
    (display msg)
    (for-each (lambda (x) (display " ") (write x)) others)
    (newline)
    (exit 1)))

;-----------------------------------------------------------------------------

(define keep
  (lambda (keep? lst)
    (cond ((null? lst)       '())
          ((keep? (car lst)) (cons (car lst) (keep keep? (cdr lst))))
          (else              (keep keep? (cdr lst))))))

(define take
  (lambda (n lst)
    (if (> n 0)
        (cons (car lst) (take (- n 1) (cdr lst)))
        '())))

(define drop
  (lambda (n lst)
    (if (> n 0)
        (drop (- n 1) (cdr lst))
        lst)))

(define repeat
  (lambda (n x)
    (if (> n 0)
        (cons x (repeat (- n 1) x))
        '())))

(define pos-in-list
  (lambda (x lst)
    (let loop ((lst lst) (i 0))
      (cond ((not (pair? lst)) #f)
            ((eq? (car lst) x) i)
            (else              (loop (cdr lst) (+ i 1)))))))

(define every
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (every pred? (cdr lst))))))

;-----------------------------------------------------------------------------

(load "node.scm")

;-----------------------------------------------------------------------------

(load "env.scm")

;-----------------------------------------------------------------------------

(load "parser.scm")

;-----------------------------------------------------------------------------

(load "context.scm")

;-----------------------------------------------------------------------------

(load "gen.scm")

;-----------------------------------------------------------------------------

(load "comp.scm")

;-----------------------------------------------------------------------------

(load "mutable.scm")

;-----------------------------------------------------------------------------

(load "varset.scm")

;------------------------------------------------------------------------------

(load "optim.scm")

(define expand-loads ;; ADDED
  (lambda (exprs)
    (map (lambda (e)
	   (if (eq? (car e) 'load)
	       (cons 'begin
		     (expand-loads (with-input-from-file (cadr e) read-all)))
	       e))
	 exprs)))

(define parse-file
  (lambda (filename)
    (let* ((library
            (with-input-from-file "library.scm" read-all))
           (toplevel-exprs
            (expand-loads (append library ;; ADDED (didn't have expand-loads)
				  (with-input-from-file filename read-all))))
           (global-env
            (make-global-env))
           (parsed-prog
            (parse-top (cons 'begin toplevel-exprs) global-env)))

      (for-each
       (lambda (node)
         (mark-needed-global-vars! global-env node))
       parsed-prog)

      (extract-parts
       parsed-prog
       (lambda (defs after-defs)

         (define make-seq-preparsed
           (lambda (exprs)
             (let ((r (make-seq #f exprs)))
               (for-each (lambda (x) (node-parent-set! x r)) exprs)
               r)))

         (define make-call-preparsed
           (lambda (exprs)
             (let ((r (make-call #f exprs)))
               (for-each (lambda (x) (node-parent-set! x r)) exprs)
               r)))

         (if (var-needed?
              (env-lookup global-env '#%readyq))
             (make-seq-preparsed
              (list (make-seq-preparsed defs)
                    (make-call-preparsed
                     (list (parse 'value '#%start-first-process global-env)
                           (let* ((pattern
                                   '())
                                  (ids
                                   (extract-ids pattern))
                                  (r
                                   (make-prc #f '() #f (has-rest-param? pattern) #f))
                                  (new-env
                                   (env-extend global-env ids r))
                                  (body
                                   (make-seq-preparsed after-defs)))
                             (prc-params-set!
                              r
                              (map (lambda (id) (env-lookup new-env id))
                                   ids))
                             (node-children-set! r (list body))
                             (node-parent-set! body r)
                             r)))
                    (parse 'value
                           '(#%exit)
                           global-env)))
             (make-seq-preparsed
              (append defs
                      after-defs
                      (list (parse 'value
                                   '(#%halt)
                                   global-env))))))))))

(define extract-parts
  (lambda (lst cont)
    (if (or (null? lst)
            (not (def? (car lst))))
        (cont '() lst)
        (extract-parts
         (cdr lst)
         (lambda (d ad)
           (cont (cons (car lst) d) ad))))))

;------------------------------------------------------------------------------

(load "asm.scm")

;------------------------------------------------------------------------------

(load "encode.scm")

(define assemble
  (lambda (code hex-filename)
    (let loop1 ((lst code)
                (constants (predef-constants))
                (globals (predef-globals))
                (labels (list)))
      (if (pair? lst)

          (let ((instr (car lst)))
            (cond ((number? instr)
                   (loop1 (cdr lst)
                          constants
                          globals
                          (cons (cons instr (asm-make-label 'label))
                                labels)))
                  ((eq? (car instr) 'push-constant)
                   (add-constant (cadr instr)
                                 constants
                                 #t
                                 (lambda (new-constants)
                                   (loop1 (cdr lst)
                                          new-constants
                                          globals
                                          labels))))
                  ((memq (car instr) '(push-global set-global))
                   (add-global (cadr instr)
                               globals
                               (lambda (new-globals)
                                 (loop1 (cdr lst)
                                        constants
                                        new-globals
                                        labels))))
                  (else
                   (loop1 (cdr lst)
                          constants
                          globals
                          labels))))

          (let ((constants (sort-constants constants)))

            (define (label-instr label opcode)
              (asm-at-assembly
               (lambda (self)
                 3) ;; TODO BARF was 2, maybe was length ? seems to be fixed
               (lambda (self)
                 (let ((pos (- (asm-label-pos label) code-start)))
                   ;; (asm-8 (+ (quotient pos 256) opcode))
		   ;; TODO do we mess up any offsets ? FOOBAR
		   (asm-8 opcode)
		   (asm-8 (quotient pos 256))
                   (asm-8 (modulo pos 256))))))

            (define (push-constant n)
              (if (<= n 31)
                  (asm-8 (+ #x00 n))
                  (begin
                    (asm-8 #xfc)
                    (asm-8 (quotient n 256))
		    (asm-8 (modulo n 256))))) ;; TODO with 13-bit objects, we need 2 bytes, maybe limit to 12, so we could use a byte and a half, but we'd need to use an opcode with only 4 bits, maybe the call/jump stuff can be combined ? FOOBAR

            (define (push-stack n)
              (if (> n 31)
                  (compiler-error "stack is too deep")
                  (asm-8 (+ #x20 n))))

            (define (push-global n)
	      (asm-8 (+ #x40 n)) ;; TODO maybe do the same as for csts, have a push-long-global to have more ?
              ;; (if (> n 15)
	      ;;     (compiler-error "too many global variables")
	      ;;     (asm-8 (+ #x40 n)))
	      ) ;; TODO actually inline most, or put as csts

            (define (set-global n)
	      (asm-8 (+ #x50 n))
              ;; (if (> n 15) ;; ADDED prevented the stack from compiling
	      ;;     (compiler-error "too many global variables")
	      ;;     (asm-8 (+ #x50 n)))
	      )

            (define (call n)
              (if (> n 15)
	          (compiler-error "call has too many arguments")
	          (asm-8 (+ #x60 n))))

            (define (jump n)
              (if (> n 15)
                  (compiler-error "call has too many arguments")
                  (asm-8 (+ #x70 n))))

            (define (call-toplevel label) ;; TODO use 8-bit opcodes for these
              (label-instr label #x80))

            (define (jump-toplevel label)
              (label-instr label #x90))

            (define (goto label)
              (label-instr label #xa0))

            (define (goto-if-false label)
              (label-instr label #xb0))

            (define (closure label)
              (label-instr label #xc0)) ;; FOOBAR change here ?

            (define (prim n)
              (asm-8 (+ #xd0 n)))

            (define (prim.number?)        (prim 0))
            (define (prim.+)              (prim 1))
            (define (prim.-)              (prim 2))
            (define (prim.*)              (prim 3))
            (define (prim.quotient)       (prim 4))
            (define (prim.remainder)      (prim 5))
            (define (prim.neg)            (prim 6))
            (define (prim.=)              (prim 7))
            (define (prim.<)              (prim 8))
	    (define (prim.ior)            (prim 9))
            (define (prim.>)              (prim 10))
	    (define (prim.xor)            (prim 11))
            (define (prim.pair?)          (prim 12))
            (define (prim.cons)           (prim 13))
            (define (prim.car)            (prim 14))
            (define (prim.cdr)            (prim 15))
            (define (prim.set-car!)       (prim 16))
            (define (prim.set-cdr!)       (prim 17))
            (define (prim.null?)          (prim 18))
            (define (prim.eq?)            (prim 19))
            (define (prim.not)            (prim 20))
            (define (prim.get-cont)       (prim 21))
            (define (prim.graft-to-cont)  (prim 22))
            (define (prim.return-to-cont) (prim 23))
            (define (prim.halt)           (prim 24))
            (define (prim.symbol?)        (prim 25))
            (define (prim.string?)        (prim 26))
            (define (prim.string->list)   (prim 27))
            (define (prim.list->string)   (prim 28))

            (define (prim.print)          (prim 32))
            (define (prim.clock)          (prim 33))
            (define (prim.motor)          (prim 34))
            (define (prim.led)            (prim 35))
	    (define (prim.led2-color)     (prim 36))
	    (define (prim.getchar-wait)   (prim 37))
	    (define (prim.putchar)        (prim 38))
	    (define (prim.beep)           (prim 39))
	    (define (prim.adc)            (prim 40))
	    (define (prim.dac)            (prim 41))
	    (define (prim.sernum)         (prim 42)) ;; TODO necessary ?

            (define (prim.shift)          (prim 45))
            (define (prim.pop)            (prim 46))
            (define (prim.return)         (prim 47))

            (define big-endian? #f)

            (asm-begin! code-start #f)

            (asm-8 #xfb)
            (asm-8 #xd7)
            (asm-8 (length constants))
            (asm-8 0)

            (pp (list constants: constants globals: globals)) ;; TODO debug

            (for-each
             (lambda (x)
               (let* ((descr (cdr x))
                      (label (vector-ref descr 1))
                      (obj (car x)))
                 (asm-label label)
		 ;; see the vm source for a description of encodings
                 (cond ((and (integer? obj) (exact? obj))
                        (asm-8 0)
                        (asm-8 (bitwise-and (arithmetic-shift obj -16) 255))
                        (asm-8 (bitwise-and (arithmetic-shift obj -8) 255))
                        (asm-8 (bitwise-and obj 255)))
                       ((pair? obj)
			(let ((obj-car (encode-constant (car obj) constants))
			      (obj-cdr (encode-constant (cdr obj) constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-car -8)))
			  (asm-8 (bitwise-and obj-car #xff))
			  (asm-8 (+ 0 (arithmetic-shift obj-cdr -8)))
			  (asm-8 (bitwise-and obj-cdr #xff))))
                       ((symbol? obj)
                        (asm-8 #x80)
                        (asm-8 0)
                        (asm-8 #x20)
                        (asm-8 0))
                       ((string? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-enc -8)))
			  (asm-8 (bitwise-and obj-enc #xff))
			  (asm-8 #x40)
			  (asm-8 0)))
                       ((vector? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-enc -8)))
			  (asm-8 (bitwise-and obj-enc #xff))
			  (asm-8 #x60)
			  (asm-8 0)))
                       (else
                        (compiler-error "unknown object type" obj)))))
             constants)

            (let loop2 ((lst code))
              (if (pair? lst)
                  (let ((instr (car lst)))

                    (cond ((number? instr)
                           (let ((label (cdr (assq instr labels))))
                             (asm-label label)))

                          ((eq? (car instr) 'entry)
                           (let ((np (cadr instr))
                                 (rest? (caddr instr)))
                             (asm-8 (if rest? (- np) np))))

                          ((eq? (car instr) 'push-constant) ;; TODO FOOBAR 12 bits for constants now (actually, I don't think it matters here)
                           (let ((n (encode-constant (cadr instr) constants)))
                             (push-constant n)))

                          ((eq? (car instr) 'push-stack)
                           (push-stack (cadr instr)))

                          ((eq? (car instr) 'push-global)
                           (push-global (cdr (assq (cadr instr) globals))))

                          ((eq? (car instr) 'set-global)
                           (set-global (cdr (assq (cadr instr) globals))))

                          ((eq? (car instr) 'call)
                           (call (cadr instr)))

                          ((eq? (car instr) 'jump)
                           (jump (cadr instr)))

                          ((eq? (car instr) 'call-toplevel)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (call-toplevel label)))

                          ((eq? (car instr) 'jump-toplevel)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (jump-toplevel label)))

                          ((eq? (car instr) 'goto)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (goto label)))

                          ((eq? (car instr) 'goto-if-false)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (goto-if-false label)))

                          ((eq? (car instr) 'closure)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (closure label)))

                          ((eq? (car instr) 'prim)
                           (case (cadr instr)
                             ((#%number?)        (prim.number?))
                             ((#%+)              (prim.+))
                             ((#%-)              (prim.-))
                             ((#%*)              (prim.*))
                             ((#%quotient)       (prim.quotient))
                             ((#%remainder)      (prim.remainder))
                             ((#%neg)            (prim.neg))
                             ((#%=)              (prim.=))
                             ((#%<)              (prim.<))
			     ((#%ior)            (prim.ior))
                             ((#%>)              (prim.>))
			     ((#%xor)            (prim.xor))
                             ((#%pair?)          (prim.pair?))
                             ((#%cons)           (prim.cons))
                             ((#%car)            (prim.car))
                             ((#%cdr)            (prim.cdr))
                             ((#%set-car!)       (prim.set-car!))
                             ((#%set-cdr!)       (prim.set-cdr!))
                             ((#%null?)          (prim.null?))
                             ((#%eq?)            (prim.eq?))
                             ((#%not)            (prim.not))
                             ((#%get-cont)       (prim.get-cont))
                             ((#%graft-to-cont)  (prim.graft-to-cont))
                             ((#%return-to-cont) (prim.return-to-cont))
                             ((#%halt)           (prim.halt))
                             ((#%symbol?)        (prim.symbol?))
                             ((#%string?)        (prim.string?))
                             ((#%string->list)   (prim.string->list))
                             ((#%list->string)   (prim.list->string))

                             ((#%print)          (prim.print))
                             ((#%clock)          (prim.clock))
                             ((#%motor)          (prim.motor))
                             ((#%led)            (prim.led))
			     ((#%led2-color)     (prim.led2-color))
                             ((#%getchar-wait)   (prim.getchar-wait))
                             ((#%putchar)        (prim.putchar))
			     ((#%beep)           (prim.beep))
                             ((#%adc)            (prim.adc))
                             ((#%dac)            (prim.dac))
                             ((#%sernum)         (prim.sernum))
                             (else
                              (compiler-error "unknown primitive" (cadr instr)))))

                          ((eq? (car instr) 'return)
                           (prim.return))

                          ((eq? (car instr) 'pop)
                           (prim.pop))

                          ((eq? (car instr) 'shift)
                           (prim.shift))

                          (else
                           (compiler-error "unknown instruction" instr)))

                    (loop2 (cdr lst)))))

            (asm-assemble)

            (asm-write-hex-file hex-filename)

            (asm-end!))))))

(define execute
  (lambda (hex-filename)
'
    (if #f
        (begin
          (shell-command "gcc -o picobit-vm picobit-vm.c")
          (shell-command (string-append "./picobit-vm " hex-filename)))
        (shell-command (string-append "./robot . 1 " hex-filename)))))

(define (sort-list l <?)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))

;------------------------------------------------------------------------------

(define compile
  (lambda (filename)
    (let* ((node (parse-file filename))
           (hex-filename
            (string-append
             (path-strip-extension filename)
             ".hex")))

;      (pp (node->expr node))

      (let ((ctx (comp-none node (make-init-context))))
        (let ((prog (linearize (optimize-code (context-code ctx)))))
;         (pp (list code: prog env: (context-env ctx)))
	  (assemble prog hex-filename)
	  (execute hex-filename))))))


(define main
  (lambda (filename)
    (compile filename)))

;------------------------------------------------------------------------------

'
(define (asm-write-hex-file filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-hex n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (display ", 0x")
        (print-hex (quotient n 16))
        (print-hex (modulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (quotient addr 256))
              (addr-lo (modulo addr 256)))
;          (display ":")
;          (print-byte n)
;          (print-byte addr-hi)
;          (print-byte addr-lo)
;          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (modulo (- (apply + n addr-hi addr-lo type bytes)) 256)))
;            (print-byte sum)
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
                           (if (= (modulo pos 8) 0)
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
            (print-line 1 0 '())))))))
