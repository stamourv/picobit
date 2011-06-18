#lang racket

(require rackunit)

(for ([file (in-directory "./tests/")]
      #:when (and (regexp-match? #rx"[.]scm$" file)
                  ;; skip emacs temp unsaved file backups
                  (not (regexp-match "^\\.#" file))))
  (let* [(file-str (path->string file))
         (hex (path-replace-suffix file ".hex"))
         (expected (path-replace-suffix file ".expected"))]
    (if (file-exists? expected)
        (dynamic-wind
          (λ () (system* "./picobit" file-str))
          (λ () (if (file-exists? hex)
                    (check-equal? (file->string (string->path expected))
                                  (file->string (string->path hex))
                                  file-str)
                    (printf "~a did not compile!\n" file-str)))
          (λ () (if (file-exists? hex)
                    (delete-file hex)
                    '())))
        (printf "File ~a has no expected file!\n" file-str))))
