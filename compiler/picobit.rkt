#lang racket

(require "utilities.rkt"
         "ast.rkt"
         "env.rkt"
         "parser.rkt"
         "context.rkt"
         "comp.rkt"
         "encoding.rkt"
         "analysis.rkt"
         "scheduling.rkt"
         "tree-shaker.rkt")

;-----------------------------------------------------------------------------

(define (optimize-code code)
  (let ((bbs (code->vector code)))
    (resolve-toplevel-labels! bbs)
    (tree-shake! bbs)))

(define (compile filename)
  (let* ((node (parse-file filename))
         (hex-filename
          (path-replace-suffix filename ".hex")))
    
    (adjust-unmutable-references! node)

    (let ((ctx (comp-none node (make-init-context))))
      (let ((prog (linearize (optimize-code (context-code ctx)))))
        ;; r5rs's with-output-to-file (in asm.rkt) can't overwrite. bleh
        (when (file-exists? hex-filename)
          (delete-file hex-filename))
        (assemble prog hex-filename)))))


(void (compile (vector-ref (current-command-line-arguments) 0)))
