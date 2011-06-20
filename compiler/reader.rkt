#lang racket

(require (only-in unstable/port read-all)
         racket/runtime-path)

(provide read-file)

(define (expand-includes exprs)
  (map (lambda (e)
         (if (eq? (car e) 'include)
             (cons 'begin
                   (expand-includes
                    (with-input-from-file (cadr e) read-all)))
             e))
       exprs))

(define-runtime-path compiler-dir ".")

(define (read-file filename)
  (let ([library
         (with-input-from-file (build-path compiler-dir "library.scm")
           read-all)])
    (expand-includes
     (append library
             (with-input-from-file filename read-all)))))
