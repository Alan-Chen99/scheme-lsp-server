(define file-table
  (make-parameter (make-hash-table)))

(define (read-contents port)
  (define raw-contents (read-string #f port))
  (if (eof-object? raw-contents)
      ""
      raw-contents))

(define (read-file! path)
  (if (hash-table-exists? (file-table) path)
      (hash-table-ref (file-table) path)
      (let ((contents (call-with-input-file path
                            (lambda (p) (read-contents p)))))
        (hash-table-update!/default (file-table)
                                    path
                                    (lambda (v)
                                      contents)
                                    contents)
        contents)))



(define (update-file! path . args)
  (define change-contents (if (null? args)
                              #f
                              (parse-change-contents (car args))))

  (write-log 'info
             (format "~s" (change-contents-text change-contents)))
  (if (change-contents-range change-contents)
      (hash-table-update!/default (file-table)
                                  path
                                  (lambda (contents)
                                    (apply-change change-contents contents))
                                  (call-with-input-file path
                                    (lambda (p)
                                      (apply-change change-contents
                                                    (read-contents p)))))
      ;; if range is not set (#f), the client will send the complete file.
      ;; TODO: read text instead of file from disk
      (let ((contents (if (null? args)
                          (call-with-input-file path
                            (lambda (p)
                              (read-contents p)))
                          (car args))))
        (hash-table-update!/default (file-table)
                                    path
                                    (lambda (v)
                                      contents)
                                    contents)
        contents)))

(define (free-file! path)
  (define file (hash-table-ref/default (file-table) path #f))
  (if (not file)
      (begin (write-log 'warning
                        "trying to freeing a non-existing file"
                        path)
             #f)
      (begin (hash-table-delete! (file-table) path)
             #t)))

(define (get-word-under-cursor params)
  (define file-path (get-uri-path params))
  (define contents (read-file! file-path))
  (define line-number (alist-ref* '(position line) params))
  (define char-number (alist-ref* '(position character) params))
  (define text-pos
    (min (line/char->pos contents line-number char-number)
         (- (document-length contents) 1)))

  (let* ((word-end
          (let loop ((pos text-pos))
            (if (identifier-char? (string-ref contents pos))
                (loop (+ pos 1))
                pos)))
         (word-start
          (if (= char-number 0)
              0
              (let loop ((pos (- text-pos 1)))
                (let ((c (string-ref contents pos)))
                  (cond ((= pos 0)
                         pos)
                        ((char=? c #\newline)
                         (+ pos 1))
                        ((identifier-char? c)
                         (if (= pos 0)
                             0
                             (loop (- pos 1))))
                        (else (+ pos 1))))))))
    (if (> word-start word-end)
        #f
        (let ((word (substring contents word-start word-end)))
          (write-log 'debug (string-append "selected word: "
                                           word))
          (make-editor-word word
                            line-number
                            line-number
                            word-start
                            word-end)))))

(define-record-type <change-contents>
  (make-change-contents range text)
  change-contents?
  (range change-contents-range)
  (text change-contents-text))

(define-record-type <range>
  (make-range start-line start-char end-line end-char length)
  range?
  (start-line range-start-line)
  (start-char range-start-char)
  (end-line range-end-line)
  (end-char range-end-char)
  (length range-length))

(define (parse-change-contents contents)
  (define change-contents (vector-ref contents 0))
  (define range-contents (alist-ref 'range change-contents))
  (define range-start (and range-contents
                           (alist-ref 'start range-contents)))
  (define range-end (and range-contents
                         (alist-ref 'end range-contents)))
  (define range-len (and range-contents
                         (alist-ref 'rangeLength change-contents)))
  (define text (and range-contents
                    (alist-ref 'text change-contents)))
  (define range
    (and range-contents
         (make-range (alist-ref 'line range-start)
                     (alist-ref 'character range-start)
                     (alist-ref 'line range-end)
                     (alist-ref 'character range-end)
                     range-len)))
  
  (make-change-contents range text))

(define (apply-change change doc)
  (define text (change-contents-text change))
  (define range (change-contents-range change))
  (define normalized-range (normalize-range range))
  
  (define start-pos
    (line/char->pos doc
                    (range-start-line normalized-range)
                    (range-start-char normalized-range)))
  (define end-pos
    (line/char->pos doc
                    (range-end-line normalized-range)
                    (range-end-char normalized-range)))
  (define old-len (- end-pos start-pos))
  (define new-len (document-length text))
  (let ((tmp-str (document-contract doc
                                    start-pos
                                    end-pos)))
    (document-insert tmp-str
                     text
                     start-pos)))


(define (normalize-range range)
  (define start-line (range-start-line range))
  (define end-line (range-end-line range))
  (if (<= start-line end-line)
      range
      (make-range end-line
                  (range-end-char range)
                  start-line
                  (range-start-char range)
                  (range-length range))))


