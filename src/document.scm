(define-record-type <document>
  (make-document contents lines-offsets)
  document?
  (contents document-contents)
  (lines-offsets document-lines-offsets))

(define (read-document port)
  (let loop ((c (read-char port))
             (char-number 0)
             (line-number 0)
             (lines '(0))
             (res ""))
    (if (eof-object? c)
        (make-document res (list->vector (reverse lines)))
        (if (char=? c #\newline)
            (loop (read-char port)
                  (+ char-number 1)
                  (+ line-number 1)
                  (cons (+ char-number 1)
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
             (lines '(0)))
    (if (>= char-number len)
        (list->vector (reverse lines))
        (let ((c (string-ref str char-number)))
          (if (char=? c #\newline)
              (loop (+ char-number 1)
                    (+ line-number 1)
                    (cons (+ char-number 1)
                          lines))
              (loop (+ char-number 1)
                    line-number
                    lines))))))

(define (document-insert doc text start-pos)
  (unless (<= start-pos
              (document-length doc))
    (error (format "invalid start-pos: ~a. Document length is ~a~%"
                   start-pos
                   (document-length doc))))

  (let* ((contents (document-contents doc))
         (new-contents (string-append (string-take contents start-pos)
                                      text
                                      (string-drop contents start-pos))))
    (make-document new-contents (compute-lines-offsets new-contents))))

(define (document-contract doc start-pos end-pos)
  (unless (<= end-pos (document-length doc))
    (error "invalid end-pos" end-pos))
  (let ((new-contents (string-replace (document-contents doc)
                                      ""
                                      (min start-pos end-pos)
                                      (max start-pos end-pos))))
    (make-document new-contents (compute-lines-offsets new-contents))))

(define (document-take doc num)
  (string-take (document-contents doc) num))

(define (document-take-right doc num)
  (string-take-right (document-contents doc) num))

(define (document-expand doc start-pos amount)
  (unless (>= (document-length doc)
              start-pos)
    (error "invalid start-pos: " start-pos))
  (let ((new-contents
         (string-append (document-take doc start-pos)
                        (string-pad "" amount)
                        (document-take-right doc (- (document-length doc)
                                                    start-pos)))))
    (make-document new-contents (compute-lines-offsets new-contents))))

(define (document-num-lines doc)
  (vector-length (document-lines-offsets doc)))

(define (line/char->pos doc line char)
  (define offsets (document-lines-offsets doc))
  (define line-offset (vector-ref offsets line))
  (+ char line-offset))
