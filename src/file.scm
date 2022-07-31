(define file-table (make-parameter (make-hash-table)))


(define (read-file! path)
  (define result
    (cond ((hash-table-exists? (file-table) path)
           (hash-table-ref (file-table) path))
          (else
           (let ((doc (call-with-input-file path read-document)))
             (hash-table-update!/default (file-table)
                                         path
                                         (lambda (v)
                                           doc)
                                         doc)
             doc))))
  result)


(define (update-file! path . args)
  (define change-contents (if (null? args)
                              #f
                              (parse-change-contents (car args))))

  (if change-contents
      (begin
        (write-log 'info
                   (format "~s" (change-contents-text change-contents)))
        (let ((result (cond ((change-contents-range change-contents)
                             (if (hash-table-exists? (file-table) path)
                                 (hash-table-update! (file-table)
                                                     path
                                                     (lambda (contents)
                                                       (let ((new-contents (apply-change change-contents contents)))
                                                         new-contents)))
                                 (hash-table-set! (file-table)
                                                  path
                                                  (begin
                                                    (write-log 'debug
                                                               (format "reading file from disk: ~a" path))
                                                    (call-with-input-file path
                                                      (lambda (p)
                                                        (apply-change change-contents
                                                                      (read-document p))))))))
                            ;; if range is not set (#f), the client will send the complete file.
                            (else
                             (let ((contents (change-contents-text change-contents)))
                               ;; TODO is this according to the protocol possible?
                               (when (hash-table-exists? (file-table) path)
                                 (write-log 'warning
                                            (format "Replacing contents for file ~a"
                                                    path)))
                               (hash-table-set! (file-table)
                                                path
                                                (lambda (v)
                                                  contents))
                               contents)))))
          result))
      #f))

(define (free-file! path)
  (define file (hash-table-ref/default (file-table) path #f))
  (let ((result (if (not file)
                    (begin (write-log 'warning
                                      "trying to freeing a non-existing file"
                                      path)
                           #f)
                    (begin (hash-table-delete! (file-table) path)
                           #t))))
    result))

(define (get-word-under-cursor params)
  (define file-path (get-uri-path params))
  (define doc (read-file! file-path))
  (define contents (document-contents doc))
  (define contents-length (document-length doc))
  (define line-number (alist-ref* '(position line) params))
  (define char-number (alist-ref* '(position character) params))
  (write-log 'debug
             "get-word-under-cursor: computing text-pos")
  (define text-pos
    (min (line/char->pos doc line-number char-number)
         (max (- (document-length doc) 1)
              0)))
  (write-log 'debug
             (format "contents-length: ~a; text-pos: ~a"
                     contents-length
                     text-pos))

  (cond ((= contents-length 0)
         #f)
        (else
         (let* ((word-end
                 (let loop ((pos text-pos)
                            (cn char-number))
                   (cond ((>= pos contents-length)
                          cn)
                         ((identifier-char? (string-ref contents pos))
                          (loop (+ pos 1) (+ cn 1)))
                         (else cn))))
                (word-start
                 (if (<= text-pos 0)
                     (if (identifier-char? (string-ref contents text-pos))
                         0
                         #f)
                     (let loop ((pos (- text-pos 1))
                                (cn (- char-number 1)))
                       (cond ((= pos 0)
                              (if (identifier-char? (string-ref contents 0))
                                  0
                                  (+ cn 1)))
                             ((>= pos contents-length)
                              (write-log 'error
                                         (format "pos ~a bigger than contents-length ~a"
                                                 pos
                                                 contents-length)))
                             (else 
                              (let ((c (string-ref contents pos)))
                                (cond ((char=? c #\newline)
                                       (+ cn 1))
                                      ((identifier-char? c)
                                       (if (= cn 0)
                                           0
                                           (loop (- pos 1)
                                                 (- cn 1))))
                                      (else (+ cn 1))))))))))
           (cond ((or (not word-start) (not word-end))
                  #f)
                 ((> word-start word-end) #f)
                 (else
                  (let ((word (substring
                               contents
                               (line/char->pos doc line-number word-start)
                               (line/char->pos doc line-number word-end))))
                    (write-log 'debug
                               (format "word-start: ~a (~a), word-end: ~a ~a~%"
                                       word-start
                                       (line/char->pos doc line-number word-start)
                                       word-end
                                       (line/char->pos doc line-number word-end)))
                    (write-log 'debug (string-append "selected word: "
                                                     word))
                    (make-editor-word word
                                      line-number
                                      line-number
                                      word-start
                                      word-end))))))))

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
  (define new-len (string-length text))
  (write-log
   'debug
   (format "apply-change text: ~a, start-line: ~a, start-char: ~a, end-line: ~a, end-char: ~a, start-pos: ~a end-pos: ~a~%"
           text
           (range-start-line normalized-range)
           (range-start-char normalized-range)
           (range-end-line normalized-range)
           (range-end-char normalized-range)
           start-pos
           end-pos))

  (let ((contracted-doc
         (document-contract doc
                            (min start-pos end-pos)
                            (max start-pos end-pos))))
    (document-insert contracted-doc
                     text
                     (min start-pos end-pos))))

(define (invert-range range)
  (make-range (range-end-line range)
              (range-end-char range)
              (range-start-line range)
              (range-start-char range)
              (range-length range)))


(define (normalize-range range)
  (define start-line (range-start-line range))
  (define start-char (range-start-char range))
  (define end-line (range-end-line range))
  (define end-char (range-end-char range))
  (cond ((= start-line end-line)
         (if (<= start-char end-char)
             range
             (invert-range range)))
        ((< start-line end-line)
         range)
        ((> start-line end-line)
         (invert-range range))))
