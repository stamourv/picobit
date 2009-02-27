;;;; File: "picobit.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

;;;; Copyright (C) 2004-2009 by Marc Feeley and Vincent St-Amour
;;;; All Rights Reserved.

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

(include "utilities.scm")
(include "node.scm")
(include "env.scm")
(include "parser.scm")
(include "context.scm")
(include "comp.scm")
(include "asm.scm")
(include "encoding.scm")

;-----------------------------------------------------------------------------

(define expand-includes
  (lambda (exprs)
    (map (lambda (e)
	   (if (eq? (car e) 'include)
	       (cons 'begin
		     (expand-includes
		      (with-input-from-file (cadr e) read-all)))
	       e))
	 exprs)))

(define parse-file
  (lambda (filename)
    (let* ((library
            (with-input-from-file "library.scm" read-all))
           (toplevel-exprs
            (expand-includes
	     (append library
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
                                   (make-prc #f
					     '()
					     #f
					     (has-rest-param? pattern)
					     #f))
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

(define execute
  (lambda (hex-filename)
'
    (if #f
        (begin
          (shell-command "gcc -o picobit-vm picobit-vm.c")
          (shell-command (string-append "./picobit-vm " hex-filename)))
        (shell-command (string-append "./robot . 1 " hex-filename)))))

;------------------------------------------------------------------------------

(define compile
  (lambda (filename)
    (let* ((node (parse-file filename))
           (hex-filename
            (string-append
             (path-strip-extension filename)
             ".hex")))
      
      (adjust-unmutable-references! node)

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
