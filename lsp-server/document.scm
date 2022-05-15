(define-library (lsp-server document)

(export make-document
        document-length
        document-insert
        document-contract
        document-num-lines
        document-take
        document-take-right
        document-expand
        line/char->pos)

(import (scheme base)
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
    (make-document content lines-indices)
    document?
    (content document-content)
    (lines-indices document-lines-indices))
  
  (define (read-document port)
    (let loop ((c (read-char port))
               (char-number 0)
               (line-number 0)
               (lines '())
               (res ""))
      (if (eof-object? c)
          (make-document res (alist->hash-table lines))
          (let ((ch (read-char port)))
            (loop ch
                  (+ char-number 1)
                  (if (char=? ch #\newline)
                      (+ line-number 1)
                      line-number)
                  (cons (cons line-number char-number)
                        lines)
                  (string-append res (string c)))))))

  (define (document-length doc)
    (string-length doc))

  (define (document-insert doc text start-pos)
    (unless (<= start-pos
                (document-length doc))
      (error (format "invalid start-pos: ~a. Document length is ~a~%"
                     start-pos
                     (document-length doc))))

    (string-append (string-take doc start-pos)
                   text
                   (string-drop doc start-pos)))

  (define (document-contract doc start-pos end-pos)
    (unless (<= end-pos (document-length doc))
      (error "invalid end-pos" end-pos))
    #;
    (write-log 'info
               (format "document-contract. start-pos: ~a, end-pos: ~a,~%~a"
                       start-pos
                       end-pos
                       doc))
    (let ((result (string-replace doc
                                  ""
                                  (min start-pos end-pos)
                                  (max start-pos end-pos))))
      #;
      (write-log 'info
                 (format "after contraction.~%~a"
                         result))
      result))

  (define (document-take doc num)
    (string-take doc num))

  (define (document-take-right doc num)
    (string-take-right doc num))

  (define (document-expand doc start-pos amount)
    (unless (>= (document-length doc)
                start-pos)
      (error "invalid start-pos: " start-pos))
    (string-append (document-take doc start-pos)
                   (string-pad "" amount)
                   (document-take-right doc (- (document-length doc)
                                               start-pos))))

  (define (document-num-lines doc)
    (string-count doc (lambda (c) (char=? c #\newline))))

  (define (line/char->pos doc line char)
    (define doc-size (document-length doc))
    (define pos
      (let loop ((i 0)
                 (cur-char 0)
                 (cur-line 0))
        (cond ((= i doc-size)
               i)
              ((and (= cur-line line)
                    (= cur-char char))
               i)
              ((char=? (string-ref doc i) #\newline)
               (loop (+ i 1)
                     0
                     (+ cur-line 1)))
              (else
               (loop (+ i 1)
                     (+ cur-char 1)
                     cur-line)))))
    #;
    (write-log 'debug
               (format "line/char->pos line: ~a, char: ~a~%doc: ~%~a, pos: ~s"
                       line
                       char
                       doc
                       pos))
    pos)))
