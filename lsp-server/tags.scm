(define-library (lsp-server tags)

(export parse-definition-line
        generate-tags)

(import r7rs
        (scheme base)
        (scheme file)
        (scheme write)
        (srfi 1)
        (srfi 28)
        (chicken file)
        (chicken irregex)
        (only (chicken file posix) directory?)

        (lsp-server private))

(begin
  (define (parse-definition-line line)
    (define regex
      (irregex '(: (* whitespace)
                   (* #\()
                   (: (or "define" "define-syntax" "set!")
                      (+ whitespace)
                      (? #\()
                      (submatch (+ (~ (or whitespace
                                          #\))))))
                   (* any))))
      (let ((submatches (irregex-match regex line)))
      (if submatches
          (irregex-match-substring submatches 1)
          #f)))

  (define (generate-tags-for-file source-file)
    (define (display-tags tags)
      (display "\f\n")
      ;; TODO figure out what is the number past the file name
      (display (format "~a,0~%" source-file))
      (for-each
       (lambda (tag-entry)
         (display (format "~a~a,0~%"
                          (car tag-entry)
                          (cdr tag-entry))))
       tags))
    (with-input-from-file source-file
      (lambda ()
        (let loop ((line (read-line))
                   (line-number 1)
                   (results '()))
          (cond ((eof-object? line)
                 (display-tags (reverse results)))
                (else
                 (let ((parsed-symbol (parse-definition-line line)))
                   (if parsed-symbol
                       (loop (read-line)
                             (+ line-number 1)
                             (cons (cons parsed-symbol line-number)
                                   results))
                       (loop (read-line)
                             (+ line-number 1)
                             results)))))))))

  (define (generate-tags tags-file . files)
    (write-log 'debug
               (format "GENERATE-TAGS: tags-file: ~s, dir: ~s" tags-file files))
    (with-output-to-file tags-file
      (lambda ()
        (for-each
         (lambda (f)
           (guard
            (condition
             (#t (write-log 'warning
                            (format "generate-tags: can't read file ~a"
                                    f))))
            (if (directory? f)
                (let ((files (find-files f
                                         #:test (irregex
                                                 '(: (* any)
                                                     (or ".scm"
                                                         ".sld"
                                                         ".ss"))))))
                  (for-each (lambda (file)
                              (generate-tags-for-file file))
                            files))
                (generate-tags-for-file f))))
         (filter (lambda (f)
                   (not (string=? f "")))
                 files)))))
))
