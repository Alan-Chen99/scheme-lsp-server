(define file-table
  (make-parameter (make-hash-table)))

(define (read-file! path)
  (if (hash-table-exists? (file-table) path)
      (hash-table-ref (file-table) path)
      (let ((lines (call-with-input-file path read-lines)))
        (hash-table-update!/default (file-table)
                                    path
                                    (lambda (v)
                                      lines)
                                    lines)
        lines)))

(define (update-file! path . contents)
  (let ((lines (if (null? contents)
                   (call-with-input-file path read-lines)
                   (car contents))))
    (hash-table-update!/default (file-table)
                                path
                                (lambda (v) lines)
                                lines)
    lines))

(define (free-file! path)
  (define file (hash-table-ref/default (file-table) path #f))
  (if file
      (write-log 'warning "trying to freeing a non-existing file")
      (hash-table-delete! (file-table) path)))

(define (get-word-under-cursor params)
  (define file-path (get-uri-path params))
  (define lines (read-file! file-path))
  (define line-number (alist-ref* '(position line) params))
  (define char-number (alist-ref* '(position character) params))
  (if (>= line-number (length lines))
      (begin (write-log 'error "line number out of reach: " line-number)
             #f)
      (let* ((line (list-ref lines line-number))
             (line-length (string-length line))
             (word-end
              (if (>= char-number line-length)
                  char-number
                  (let loop ((pos char-number))
                    (if (>= pos line-length)
                        pos
                        (if (identifier-char? (string-ref line pos))
                            (loop (+ pos 1))
                            pos)))))
             (word-start
              (if (= char-number 0)
                  0
                  (let loop ((pos (- char-number 1)))
                    (if (>= pos line-length)
                        (loop (- pos 1))
                        (let ((c (string-ref line pos)))
                          (if (identifier-char? c)
                              (if (= pos 0)
                                  0
                                  (loop (- pos 1)))
                              (+ pos 1))))))))
        (if (> word-start word-end)
            #f
            (let ((word (substring line word-start word-end)))
              (write-log 'debug (string-append "selected word: "
                                               word))
              word)))))

(define (get-word-up-to-cursor params)
  (define file-path (get-uri-path params))
  (define lines (read-file! file-path))
  (define line-number (alist-ref* '(position line) params))
  (define char-number (alist-ref* '(position character) params))
  (define line (list-ref lines line-number))
  (let loop ((pos char-number)
             (result '()))
    (let ((c (string-ref line pos)))
      (if (identifier-char? c)
          (if (= pos 0)
              (list->string (cons c result))
              (loop (- pos 1)
                    (cons c result)))
          (list->string result)))))
