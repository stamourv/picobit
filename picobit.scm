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

(define code->vector
  (lambda (code)
    (let ((v (make-vector (+ (code-last-label code) 1))))
      (for-each
       (lambda (bb)
         (vector-set! v (bb-label bb) bb))
       (code-rev-bbs code))
      v)))

(define bbs->ref-counts
  (lambda (bbs)
    (let ((ref-counts (make-vector (vector-length bbs) 0)))

      (define visit
        (lambda (label)
          (let ((ref-count (vector-ref ref-counts label)))
            (vector-set! ref-counts label (+ ref-count 1))
            (if (= ref-count 0)
                (let* ((bb (vector-ref bbs label))
                       (rev-instrs (bb-rev-instrs bb)))
                  (for-each
                   (lambda (instr)
                     (let ((opcode (car instr)))
                       (cond ((eq? opcode 'goto)
                              (visit (cadr instr)))
                             ((eq? opcode 'goto-if-false)
                              (visit (cadr instr))
                              (visit (caddr instr)))
                             ((or (eq? opcode 'closure)
                                  (eq? opcode 'call-toplevel)
                                  (eq? opcode 'jump-toplevel))
                              (visit (cadr instr))))))
                   rev-instrs))))))

      (visit 0)

      ref-counts)))

(define resolve-toplevel-labels!
  (lambda (bbs)
    (let loop ((i 0))
      (if (< i (vector-length bbs))
          (let* ((bb (vector-ref bbs i))
                 (rev-instrs (bb-rev-instrs bb)))
            (bb-rev-instrs-set!
             bb
             (map (lambda (instr)
                    (let ((opcode (car instr)))
                      (cond ((eq? opcode 'call-toplevel)
                             (list opcode
                                   (prc-entry-label (cadr instr))))
                            ((eq? opcode 'jump-toplevel)
                             (list opcode
                                   (prc-entry-label (cadr instr))))
                            (else
                             instr))))
                  rev-instrs))
            (loop (+ i 1)))))))

(define tighten-jump-cascades!
  (lambda (bbs)
    (let ((ref-counts (bbs->ref-counts bbs)))

      (define resolve
        (lambda (label)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb)))
            (and (or (null? (cdr rev-instrs))
                     (= (vector-ref ref-counts label) 1))
                 rev-instrs))))

      (let loop1 ()
        (let loop2 ((i 0)
                    (changed? #f))
          (if (< i (vector-length bbs))
              (if (> (vector-ref ref-counts i) 0)
                  (let* ((bb (vector-ref bbs i))
                         (rev-instrs (bb-rev-instrs bb))
                         (jump (car rev-instrs))
                         (opcode (car jump)))
                    (cond ((eq? opcode 'goto)
                           (let* ((label (cadr jump))
                                  (jump-replacement (resolve label)))
                             (if jump-replacement
                                 (begin
                                   (vector-set!
                                    bbs
                                    i
                                    (make-bb (bb-label bb)
                                             (append jump-replacement
                                                     (cdr rev-instrs))))
                                   (loop2 (+ i 1)
                                          #t))
                                 (loop2 (+ i 1)
                                        changed?))))
                          ((eq? opcode 'goto-if-false)
                           (let* ((label-then (cadr jump))
                                  (label-else (caddr jump))
                                  (jump-then-replacement (resolve label-then))
                                  (jump-else-replacement (resolve label-else)))
                             (if (and jump-then-replacement
                                      (null? (cdr jump-then-replacement))
                                      jump-else-replacement
                                      (null? (cdr jump-else-replacement))
                                      (or (eq? (caar jump-then-replacement) 'goto)
                                          (eq? (caar jump-else-replacement) 'goto)))
                                 (begin
                                   (vector-set!
                                    bbs
                                    i
                                    (make-bb (bb-label bb)
                                             (cons (list 'goto-if-false
                                                         (if (eq? (caar jump-then-replacement) 'goto)
                                                             (cadar jump-then-replacement)
                                                             label-then)
                                                         (if (eq? (caar jump-else-replacement) 'goto)
                                                             (cadar jump-else-replacement)
                                                             label-else))
                                                   (cdr rev-instrs))))
                                   (loop2 (+ i 1)
                                          #t))
                                 (loop2 (+ i 1)
                                        changed?))))
                          (else
                           (loop2 (+ i 1)
                                  changed?))))
                  (loop2 (+ i 1)
                         changed?))
              (if changed?
                  (loop1))))))))

(define remove-useless-bbs!
  (lambda (bbs)
    (let ((ref-counts (bbs->ref-counts bbs)))
      (let loop1 ((label 0) (new-label 0))
        (if (< label (vector-length bbs))
            (if (> (vector-ref ref-counts label) 0)
                (let ((bb (vector-ref bbs label)))
                  (vector-set!
                   bbs
                   label
                   (make-bb new-label (bb-rev-instrs bb)))
                  (loop1 (+ label 1) (+ new-label 1)))
                (loop1 (+ label 1) new-label))
            (renumber-labels bbs ref-counts new-label))))))

(define renumber-labels
  (lambda (bbs ref-counts n)
    (let ((new-bbs (make-vector n)))
      (let loop2 ((label 0))
        (if (< label (vector-length bbs))
            (if (> (vector-ref ref-counts label) 0)
                (let* ((bb (vector-ref bbs label))
                       (new-label (bb-label bb))
                       (rev-instrs (bb-rev-instrs bb)))

                  (define fix
                    (lambda (instr)

                      (define new-label
                        (lambda (label)
                          (bb-label (vector-ref bbs label))))

                      (let ((opcode (car instr)))
                        (cond ((eq? opcode 'closure)
                               (list 'closure
                                     (new-label (cadr instr))))
                              ((eq? opcode 'call-toplevel)
                               (list 'call-toplevel
                                     (new-label (cadr instr))))
                              ((eq? opcode 'jump-toplevel)
                               (list 'jump-toplevel
                                     (new-label (cadr instr))))
                              ((eq? opcode 'goto)
                               (list 'goto
                                     (new-label (cadr instr))))
                              ((eq? opcode 'goto-if-false)
                               (list 'goto-if-false
                                     (new-label (cadr instr))
                                     (new-label (caddr instr))))
                              (else
                               instr)))))

                  (vector-set!
                   new-bbs
                   new-label
                   (make-bb new-label (map fix rev-instrs)))
                  (loop2 (+ label 1)))
                (loop2 (+ label 1)))
            new-bbs)))))

(define reorder!
  (lambda (bbs)
    (let* ((done (make-vector (vector-length bbs) #f)))

      (define unscheduled?
        (lambda (label)
          (not (vector-ref done label))))

      (define label-refs
        (lambda (instrs todo)
          (if (pair? instrs)
              (let* ((instr (car instrs))
                     (opcode (car instr)))
                (cond ((or (eq? opcode 'closure)
                           (eq? opcode 'call-toplevel)
                           (eq? opcode 'jump-toplevel))
                       (label-refs (cdr instrs) (cons (cadr instr) todo)))
                      (else
                       (label-refs (cdr instrs) todo))))
              todo)))

      (define schedule-here
        (lambda (label new-label todo cont)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb))
                 (jump (car rev-instrs))
                 (opcode (car jump))
                 (new-todo (label-refs rev-instrs todo)))
            (vector-set! bbs label (make-bb new-label rev-instrs))
            (vector-set! done label #t)
            (cond ((eq? opcode 'goto)
                   (let ((label (cadr jump)))
                     (if (unscheduled? label)
                         (schedule-here label
                                        (+ new-label 1)
                                        new-todo
                                        cont)
                         (cont (+ new-label 1)
                               new-todo))))
                  ((eq? opcode 'goto-if-false)
                   (let ((label-then (cadr jump))
                         (label-else (caddr jump)))
                     (cond ((unscheduled? label-else)
                            (schedule-here label-else
                                           (+ new-label 1)
                                           (cons label-then new-todo)
                                           cont))
                           ((unscheduled? label-then)
                            (schedule-here label-then
                                           (+ new-label 1)
                                           new-todo
                                           cont))
                           (else
                            (cont (+ new-label 1)
                                  new-todo)))))
                  (else
                   (cont (+ new-label 1)
                         new-todo))))))

      (define schedule-somewhere
        (lambda (label new-label todo cont)
          (schedule-here label new-label todo cont)))

      (define schedule-todo
        (lambda (new-label todo)
          (if (pair? todo)
              (let ((label (car todo)))
                (if (unscheduled? label)
                    (schedule-somewhere label
                                        new-label
                                        (cdr todo)
                                        schedule-todo)
                    (schedule-todo new-label
                                   (cdr todo)))))))


      (schedule-here 0 0 '() schedule-todo)

      (renumber-labels bbs
                       (make-vector (vector-length bbs) 1)
                       (vector-length bbs)))))

(define linearize
  (lambda (bbs)
    (let loop ((label (- (vector-length bbs) 1))
               (lst '()))
      (if (>= label 0)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb))
                 (jump (car rev-instrs))
                 (opcode (car jump)))
            (loop (- label 1)
                  (append
                   (list label)
                   (reverse
                    (cond ((eq? opcode 'goto)
                           (if (= (cadr jump) (+ label 1))
                               (cdr rev-instrs)
                               rev-instrs))
                          ((eq? opcode 'goto-if-false)
                           (cond ((= (caddr jump) (+ label 1))
                                  (cons (list 'goto-if-false (cadr jump))
                                        (cdr rev-instrs)))
                                 ((= (cadr jump) (+ label 1))
                                  (cons (list 'goto-if-not-false (caddr jump))
                                        (cdr rev-instrs)))
                                 (else
                                  (cons (list 'goto (caddr jump))
                                        (cons (list 'goto-if-false (cadr jump))
                                              (cdr rev-instrs))))))
                          (else
                           rev-instrs)))
                   lst)))
          lst))))

(define optimize-code
  (lambda (code)
    (let ((bbs (code->vector code)))
      (resolve-toplevel-labels! bbs)
      (tighten-jump-cascades! bbs)
      (let ((bbs (remove-useless-bbs! bbs)))
        (reorder! bbs)))))

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
                 2)
               (lambda (self)
                 (let ((pos (- (asm-label-pos label) code-start)))
                   (asm-8 (+ (quotient pos 256) opcode))
                   (asm-8 (modulo pos 256))))))

            (define (push-constant n)
              (if (<= n 31)
                  (asm-8 (+ #x00 n))
                  (begin
                    (asm-8 #xfc)
                    (asm-8 n))))

            (define (push-stack n)
              (if (> n 31)
                  (compiler-error "stack is too deep")
                  (asm-8 (+ #x20 n))))

            (define (push-global n)
	      (asm-8 (+ #x40 n)) ;; TODO we are actually limited to 16 constants, since we only have 4 bits to represent them
              ;; (if (> n 15) ;; ADDED prevented the stack from compiling
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

            (define (call-toplevel label)
              (label-instr label #x80))

            (define (jump-toplevel label)
              (label-instr label #x90))

            (define (goto label)
              (label-instr label #xa0))

            (define (goto-if-false label)
              (label-instr label #xb0))

            (define (closure label)
              (label-instr label #xc0))

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
	    (define (prim.ior)            (prim 9)) ;; ADDED
            (define (prim.>)              (prim 10))
	    (define (prim.xor)            (prim 11)) ;; ADDED
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
	    (define (prim.set-fst!)       (prim 29)) ;; ADDED
	    (define (prim.set-snd!)       (prim 30)) ;; ADDED
	    (define (prim.set-trd!)       (prim 31)) ;; ADDED

            (define (prim.print)          (prim 32))
            (define (prim.clock)          (prim 33))
            (define (prim.motor)          (prim 34))
            (define (prim.led)            (prim 35))
            (define (prim.getchar-wait)   (prim 36))
            (define (prim.putchar)        (prim 37))
            (define (prim.light)          (prim 38))

	    (define (prim.triplet?)       (prim 39)) ;; ADDED
	    (define (prim.triplet)        (prim 40)) ;; ADDED
	    (define (prim.fst)            (prim 41)) ;; ADDED
	    (define (prim.snd)            (prim 42)) ;; ADDED
	    (define (prim.trd)            (prim 43)) ;; ADDED

            (define (prim.shift)          (prim 45))
            (define (prim.pop)            (prim 46))
            (define (prim.return)         (prim 47))

            (define big-endian? #f)

            (asm-begin! code-start #f)

            (asm-8 #xfb)
            (asm-8 #xd7)
            (asm-8 (length constants)) ;; TODO maybe more constants ? that would mean more rom adress space, and less for ram, for now we are ok
            (asm-8 0)

            (pp (list constants: constants globals: globals)) ;; TODO debug

            (for-each
             (lambda (x)
               (let* ((descr (cdr x))
                      (label (vector-ref descr 1))
                      (obj (car x)))
                 (asm-label label)
                 (cond ((and (integer? obj) (exact? obj))
                        (asm-8 0)
                        (asm-8 (bitwise-and (arithmetic-shift obj -16) 255))
                        (asm-8 (bitwise-and (arithmetic-shift obj -8) 255))
                        (asm-8 (bitwise-and obj 255)))
                       ((pair? obj) ;; TODO this is ok no matter how many csts we have
			(let ((obj-car (encode-constant (car obj) constants))
			      (obj-cdr (encode-constant (cdr obj) constants)))
			  ;; car and cdr are both represented in 12 bits, the
			  ;; center byte being shared between the 2
			  ;; TODO changed
			  (asm-8 2)
			  (asm-8
			   (arithmetic-shift (bitwise-and obj-car #xff0) -4))
			  (asm-8
			   (bitwise-ior (arithmetic-shift
					 (bitwise-and obj-car #xf)
					 4)
					(arithmetic-shift
					 (bitwise-and obj-cdr #xf00)
					 -8)))
			  (asm-8 (bitwise-and obj-cdr #xff))))
                       ((symbol? obj)
                        (asm-8 3)
                        (asm-8 0)
                        (asm-8 0)
                        (asm-8 0))
                       ((string? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants)))
			  (asm-8 4) ;; TODO changed
			  (asm-8 (arithmetic-shift (bitwise-and obj-enc #xff0)
						   -4))
			  (asm-8 (arithmetic-shift (bitwise-and obj-enc #xf)
						   4))
			  (asm-8 0)))
                       ((vector? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants)))
			  (asm-8 5) ;; TODO changed, and factor code
			  (asm-8 (arithmetic-shift (bitwise-and obj-enc #xff0)
						   -4))
			  (asm-8 (arithmetic-shift (bitwise-and obj-enc #xf)
						   4))
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

                          ((eq? (car instr) 'push-constant) ;; TODO FOOBAR 12 bits for constants now
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
			     ((#%ior)            (prim.ior)) ;; ADDED
                             ((#%>)              (prim.>))
			     ((#%xor)            (prim.xor)) ;; ADDED
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
			     ((#%set-fst!)       (prim.set-fst!)) ;; ADDED
			     ((#%set-snd!)       (prim.set-snd!)) ;; ADDED
			     ((#%set-trd!)       (prim.set-trd!)) ;; ADDED

                             ((#%print)          (prim.print))
                             ((#%clock)          (prim.clock))
                             ((#%motor)          (prim.motor))
                             ((#%led)            (prim.led))
                             ((#%getchar-wait)   (prim.getchar-wait))
                             ((#%putchar)        (prim.putchar))
                             ((#%light)          (prim.light))

			     ((#%triplet?)       (prim.triplet?)) ;; ADDED
			     ((#%triplet)        (prim.triplet)) ;; ADDED
			     ((#%fst)            (prim.fst)) ;; ADDED
			     ((#%snd)            (prim.snd)) ;; ADDED
			     ((#%trd)            (prim.trd)) ;; ADDED
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
