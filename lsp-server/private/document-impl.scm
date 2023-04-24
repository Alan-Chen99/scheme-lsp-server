(cond-expand
 (gambit (define vector-append (primitive vector-append))
         (define vector-copy (primitive vector-copy))
         (define vector-length (primitive vector-length))
         (define vector-ref (primitive vector-ref)))
 (else))

;;; For efficiency reasons, we represent documents by a string containing the
;;; text and a vector with indices of all #\newline's found in it.
(define-record-type <document>
  (make-document contents newline-positions)
  document?
  (contents document-contents)
  (newline-positions document-newline-positions))

(define (read-document port)
  (let loop ((c (read-char port))
             (char-number 0)
             (line-number 0)
             (lines '())
             (res ""))
    (cond ((eof-object? c)
           (make-document res (list->vector (reverse lines))))
          ((char=? c #\newline)
            (loop (read-char port)
                  (+ char-number 1)
                  (+ line-number 1)
                  (cons char-number
                        lines)
                  (string-append res (string c))))
          (else
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

(define (vector-drop-until pred vec)
  (define len (vector-length vec))
  (let loop ((idx 0))
    (cond ((>= idx len)
           #())
          ((pred (vector-ref vec idx))
           (vector-copy vec idx))
          (else (loop (+ idx 1))))))

(define (vector-take-until pred vec)
  (define len (vector-length vec))
  (let loop ((idx 0))
    (cond ((>= idx len)
           vec)
          ((pred (vector-ref vec idx))
           (vector-copy vec 0 idx))
          (else
           (loop (+ idx 1))))))

(define (shift-newlines newlines amount)
  (vector-map (lambda (v) (+ v amount))
              newlines))

(define (document-append doc1 doc2)
  (define len1 (document-length doc1))
  (define newlines1 (document-newline-positions doc1))
  (define newlines2 (shift-newlines (document-newline-positions doc2) len1))
  (make-document
   (string-append (document-contents doc1)
                  (document-contents doc2))
   (vector-append newlines1
                  newlines2)))

(define document-copy
  (case-lambda
   ((doc)
    (document-copy doc 0 (document-length doc)))
   ((doc start)
    (document-copy doc start (document-length doc)))
   ((doc start end)
    (let ((newlines (document-newline-positions doc))
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
      (let* ((newlines-after-start
              (vector-drop-until (lambda (x)
                                   (>= x start))
                                 newlines))
             (newlines-between-start-end
              (vector-take-until (lambda (x)
                                   (>= x end))
                                 newlines-after-start)))
        (make-document (string-copy (document-contents doc) start end)
                       (shift-newlines newlines-between-start-end
                                       (- start))))))))

(define (document-insert doc text start-pos)
  (unless (<= start-pos
              (document-length doc))
    (error (format "invalid start-pos: ~a. Document length is ~a~%"
                   start-pos
                   (document-length doc))))
  (let ((prefix (document-copy doc 0 start-pos))
        (suffix (document-copy doc start-pos)))
    (document-append (document-append prefix
                                      (string->document text))
                     suffix)))

(define (document-contract doc start-pos end-pos)
  (unless (<= end-pos (document-length doc))
    (error "invalid end-pos" end-pos))
  (let ((new-contents (string-replace (document-contents doc)
                                      ""
                                      (min start-pos end-pos)
                                      (max start-pos end-pos)))
        (newlines (document-newline-positions doc)))
    (let* ((newlines-before-start (vector-take-until (lambda (x)
                                                       (>= x start-pos))
                                                     newlines))
           (newlines-after-end (vector-drop-until (lambda (x)
                                                    (>= x end-pos))
                                                  newlines)))
      (make-document new-contents
                     (vector-append newlines-before-start
                                    ;;; substract reduced length from
                                    ;;; after block
                                    (shift-newlines newlines-after-end
                                                    (- start-pos end-pos)))))))

(define (document-num-lines doc)
  (vector-length (document-newline-positions doc)))

(define (line/char->pos doc line char)
  (define newlines (document-newline-positions doc))
  (define num-newlines (vector-length newlines))
  (cond ((or (= line 0) (= num-newlines 0))
         char)
        ((> line num-newlines)
         (write-log 'error
          (format "line/char->pos: line (~a) exceeds number of newlines (~a)"
                  line
                  num-newlines))
         (- (document-length doc) 1))
        (else
         (+ char
            (+ (vector-ref newlines (- line 1)) 1)))))

