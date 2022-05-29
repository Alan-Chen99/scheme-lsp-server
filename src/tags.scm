(define-record-type <tag-info>
  (make-tag-info name line character)
  tag-info?
  (name tag-info-name)
  (line tag-info-line)
  (character tag-info-character))

  ;;; source-path -> (list-of tags)
(define source-to-tags-map
  (make-parameter (make-hash-table)))

  ;;; id -> (source-path -> tag-info)
(define identifier-to-source-tag-map
  (make-parameter (make-hash-table)))

(define (insert-tags! source-path list-of-tags)
  #;
  (hash-table-set! (source-to-tags-map)
  source-path
  list-of-tags)
  (fold (lambda (tag acc)
          (let ((identifier (tag-info-name tag)))
            (begin
              (hash-table-update!/default
               acc
               identifier
               (lambda (v)
                 (begin (hash-table-set! v
                                         source-path
                                         tag)
                        v))
               (alist->hash-table
                `((,source-path . ,tag))))
              acc)))
        (identifier-to-source-tag-map)
        list-of-tags))

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
        (cons (irregex-match-substring submatches 1)
              (irregex-match-start-index submatches 1))
        #f)))

(define (parse-source-file source-file)
  (with-input-from-file source-file
    (lambda ()
      (let loop ((line (read-line))
                 (line-number 1)
                 (results '()))
        (cond ((eof-object? line)
               (reverse results))
              (else
               (let ((parse-result (parse-definition-line line)))
                 (if parse-result
                     (let ((parsed-symbol (car parse-result))
                           (line-offset (cdr parse-result)))
                       (loop (read-line)
                             (+ line-number 1)
                             (cons (make-tag-info parsed-symbol
                                                  (- line-number 1)
                                                  line-offset)
                                   results)))
                     (loop (read-line)
                           (+ line-number 1)
                           results)))))))))

(define (parse-and-insert-tags! source-file)
  (insert-tags! source-file
                (parse-source-file source-file)))

(define (generate-tags-for-file source-file)
  (define (display-tags tags)
    (display "\f\n")
    ;; TODO figure out what is the number past the file name
    (display (format "~a,0~%" source-file))
    (for-each
     (lambda (tag-entry)
       (display (format "~a~a,~a~%"
                        (tag-info-name tag-entry)
                        (tag-info-line tag-entry)
                        (tag-info-character tag-entry))))
     tags))
  (with-input-from-file source-file
    (lambda ()
      (let loop ((line (read-line))
                 (line-number 1)
                 (results '()))
        (cond ((eof-object? line)
               (display-tags (reverse results)))
              (else
               (let ((parse-result (parse-definition-line line)))
                 (if parse-result
                     (let ((parsed-symbol (car parse-result))
                           (line-offset (cdr parse-result)))
                       (loop (read-line)
                             (+ line-number 1)
                             (cons (make-tag-info parsed-symbol
                                                  line-number
                                                  line-offset)
                                   results)))
                     (loop (read-line)
                           (+ line-number 1)
                           results)))))))))

(define (get-definition-locations identifier)
  (define locations
    (hash-table->alist
     (hash-table-ref/default (identifier-to-source-tag-map)
                             identifier
                             (make-hash-table))))
  (if (not (null? locations))
      (begin
        (write-log 'debug
                   (format "locations for identifier ~a found: ~a"
                           identifier
                           locations))
        (map (lambda (loc)
               (let* ((path (car loc))
                      (tag-info (cdr loc))
                      (line-number (tag-info-line tag-info))
                      (char-number (tag-info-character tag-info)))
                 (write-log 'debug (format "identifier ~a found: path ~a, line ~a, char ~a "
                                           identifier
                                           path
                                           line-number
                                           char-number))
                 `((uri . ,(string-append "file://" path))
                   (range . ((start . ((line . ,line-number)
                                       (character . ,char-number)))
                             (end . ((line . ,line-number)
                                     (character . ,char-number))))))))
             locations))
      '()))

(define (generate-tags! . files)
  (write-log 'debug
             (format "generate-tags! for files ~a" files))
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
                        (parse-and-insert-tags! file))
                      files))
          (parse-and-insert-tags! f))))
   (filter (lambda (f)
             (not (string=? f "")))
           files)))
