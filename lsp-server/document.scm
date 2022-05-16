(define-library (lsp-server document)

(export make-document
        read-document
        document-contents
        document-length
        document-insert
        document-contract
        document-num-lines
        document-take
        document-take-right
        document-expand
        line/char->pos
        string->document
        compute-lines-offsets)

(import (chicken port)
        (scheme base)
        (scheme file)
        (scheme write)
        (lsp-server private))

(cond-expand
 (guile (import (srfi srfi-13)
                (srfi srfi-28)
                (srfi srfi-69)))
 (else (import (srfi 28)
               (srfi 69)
               (srfi 130))))

(begin
  (define-record-type <document>
    (make-document contents lines-offsets)
    document?
    (contents document-contents)
    (lines-offsets document-lines-offsets))
  
  (define (read-document port)
    (let loop ((c (read-char port))
               (char-number 0)
               (line-number 0)
               (lines '((0 . 0)))
               (res ""))
      (if (eof-object? c)
          (make-document res (alist->hash-table lines))
          (if (char=? c #\newline)
              (loop (read-char port)
                    (+ char-number 1)
                    (+ line-number 1)
                    (cons (cons (+ line-number 1) (+ char-number 1))
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
               (lines '((0 . 0))))
      (if (>= char-number len)
          (alist->hash-table lines)
          (let ((c (string-ref str char-number)))
            (if (char=? c #\newline)
                (loop (+ char-number 1)
                      (+ line-number 1)
                      (cons (cons (+ line-number 1) (+ char-number 1))
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
    (hash-table-size (document-lines-offsets doc)))

  (define (line/char->pos doc line char)
    (define offsets (document-lines-offsets doc))
    (define line-offset (hash-table-ref offsets line))
    (+ char line-offset))))
