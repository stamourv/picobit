#lang racket

(require rackunit)

(for ([file (in-directory "./tests/")]
      #:when (and (regexp-match? #rx"[.]scm$" file)
                  ;; skip emacs temp unsaved file backups
                  (not (regexp-match "^\\.#" file))))
  (let* [(file-str (path->string file))
         (base (substring file-str 0 (- (string-length file-str) 4)))
         (hex (string-append base ".hex"))
         (expected (string-append base ".expected"))]
    (if (file-exists? expected)
        (dynamic-wind
          (λ () (system* "./picobit" file-str))
          (λ () (if (file-exists? hex)
                    (check-equal? (file->string (string->path expected))
                                  (file->string (string->path hex))
                                  base)
                    (printf "~a did not compile!\n" file-str)))
          (λ () (if (file-exists? hex)
                    (delete-file hex)
                    '())))
        (printf "File ~a has no expected file!\n" file-str))))
