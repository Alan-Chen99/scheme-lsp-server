(define-record-type <document>
  (make-document contents lines-offsets)
  document?
  (contents document-contents)
  (lines-offsets document-lines-offsets))

(define (read-document port)
  (let loop ((c (read-char port))
             (char-number 0)
             (line-number 0)
             (lines '())
             (res ""))
    (if (eof-object? c)
        (make-document res (list->vector (reverse lines)))
        (if (char=? c #\newline)
            (loop (read-char port)
                  (+ char-number 1)
                  (+ line-number 1)
                  (cons char-number
                        lines)
                  (string-append res (string c)))
            (loop (read-char port)
                  (+ char-number 1)
                  line-number
                  lines
                  (string-append res (string c)))))))

(define (string->document str)
  (with-input-from-string str
    (lambda ()
      (read-document (current-input-port)))))

(define (document-length doc)
  (string-length (document-contents doc)))

(define (compute-lines-offsets str)
  (define len (string-length str))
  (let loop ((char-number 0)
             (line-number 0)
             (lines '()))
    (if (>= char-number len)
        (list->vector (reverse lines))
        (let ((c (string-ref str char-number)))
          (if (char=? c #\newline)
              (loop (+ char-number 1)
                    (+ line-number 1)
                    (cons char-number
                          lines))
              (loop (+ char-number 1)
                    line-number
                    lines))))))

(define (vector-drop-until pred vec)
  (define len (vector-length vec))
  (let loop ((idx 0))
    (cond ((>= idx len)
           #())
          ((pred (vector-ref vec idx))
           (vector-copy vec idx))
          (else (loop (+ idx 1))))))

(define (vector-drop-while pred vec)
  (define len (vector-length vec))
  (let loop ((idx 0))
    (cond ((>= idx len)
           #())
          ((pred (vector-ref vec idx))
           (loop (+ idx 1)))
          (else (vector-copy vec idx)))))

(define (vector-take-until pred vec)
  (define len (vector-length vec))
  (let loop ((idx 0))
    (cond ((>= idx len)
           vec)
          ((pred (vector-ref vec idx))
           (vector-copy vec 0 idx))
          (else
           (loop (+ idx 1))))))

(define (shift-offsets offsets amount)
  (vector-map (lambda (v) (+ v amount))
              offsets))

(define (document-shift-offsets doc amount)
  (make-document (document-contents doc)
                 (shift-offsets (document-lines-offsets doc) amount)))

(define (document-append doc1 doc2)
  (define len1 (document-length doc1))
  (define offsets1 (document-lines-offsets doc1))
  (define offsets2 (shift-offsets (document-lines-offsets doc2) len1))
  (make-document
   (string-append (document-contents doc1)
                  (document-contents doc2))
   (vector-append offsets1
                  offsets2)))

(define (document-concat doc1 doc2)
  (define offsets1 (document-lines-offsets doc1))
  (define offsets2 (document-lines-offsets doc2))
  (make-document
   (string-append (document-contents doc1)
                  (document-contents doc2))
   (vector-append (document-lines-offsets doc1)
                  (document-lines-offsets doc2))))

(define document-copy
  (case-lambda
   ((doc)
    (document-copy doc 0 (document-length doc)))
   ((doc start)
    (document-copy doc start (document-length doc)))
   ((doc start end)
    (let ((offsets (document-lines-offsets doc))
          (len (document-length doc)))
      (when (> end len)
        (error "document-copy: end bigger than document length."))
      (when (< end 0)
        (error "document-copy: negative end."))
      (when (< start 0)
        (error "document-copy: negative start."))
      (when (> start len)
        (error "document-copy: start bigger than document length."))
      (when (> start end)
        (error "document-copy: start bigger than end."))
      (let* ((offsets-after-start
              (vector-drop-until (lambda (x)
                                   (>= x start))
                                 offsets))
             (offsets-between-start-end
              (vector-take-until (lambda (x)
                                   (>= x end))
                                 offsets-after-start)))
        (make-document (string-copy (document-contents doc) start end)
                       (shift-offsets offsets-between-start-end
                                      (- start))))))))

(define (document-insert doc text start-pos)
  (unless (<= start-pos
              (document-length doc))
    (error (format "invalid start-pos: ~a. Document length is ~a~%"
                   start-pos
                   (document-length doc))))
  (define prefix (document-copy doc 0 start-pos))
  (define suffix (document-copy doc start-pos))

  (document-append (document-append prefix
                                    (string->document text))
                   suffix))

(define (document-contract doc start-pos end-pos)
  (unless (<= end-pos (document-length doc))
    (error "invalid end-pos" end-pos))
  (let ((new-contents (string-replace (document-contents doc)
                                      ""
                                      (min start-pos end-pos)
                                      (max start-pos end-pos)))
        (offsets (document-lines-offsets doc)))
    (let* ((offsets-before-start (vector-take-until (lambda (x)
                                                      (>= x start-pos))
                                                    offsets))
           (offsets-after-end (vector-drop-until (lambda (x)
                                                   (>= x end-pos))
                                                 offsets)))
      (make-document new-contents
                     (vector-append offsets-before-start
                                    ;;; substract reduced length from
                                    ;;; after block
                                    (shift-offsets offsets-after-end
                                                   (- start-pos end-pos)))))))

(define (document-num-lines doc)
  (vector-length (document-lines-offsets doc)))

(define (line/char->pos doc line char)
  (define offsets (document-lines-offsets doc))
  (cond ((or (= line 0) (= (vector-length offsets) 0))
         char)
        (else
         (+ char
            (+ (vector-ref offsets (- line 1)) 1)))))

