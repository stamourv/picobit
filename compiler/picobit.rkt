#lang racket

(require "utilities.rkt"
         "ast.rkt"
         "env.rkt"
         "reader.rkt"
         "parser.rkt"
         "front-end.rkt"
         "ir.rkt"
         "comp.rkt"
         "encoding.rkt"
         "analysis.rkt"
         "scheduling.rkt"
         "tree-shaker.rkt")

;-----------------------------------------------------------------------------

(define (compile filename)
  (let* ([hex-filename (path-replace-suffix filename ".hex")]
         [forms        (read-file  filename)]
         [global-env   (make-global-env)]
         [node         (parse-top-list forms global-env)])

    (mark-needed-global-vars! global-env node)

    (let ([node (extract-parts-top node global-env parse)])
      (adjust-unmutable-references! node)
      (let* ([ctx  (comp-none node (make-init-context))]
             [code (context-code ctx)]
             [bbs  (code->vector code)])
        (resolve-toplevel-labels! bbs)
        (let* ([bbs  (reorder! (tree-shake! bbs))]
               [prog (linearize bbs)])
          ;; r5rs's with-output-to-file (in asm.rkt) can't overwrite. bleh
          (when (file-exists? hex-filename)
            (delete-file hex-filename))
          (assemble prog hex-filename))))))

(command-line
 #:once-each
 [("--stats") "Display statistics about generated instructions." (stats? #t)]
 #:args (filename)
 (void (compile filename)))
