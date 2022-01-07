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
        (chicken port)
        (srfi 28)
        (srfi 130)

        (lsp-server private))


(begin
  (define (make-document)
    "")

  (define (document-length doc)
    (string-length doc))

  (define (document-insert doc text start-pos)
    (unless (<= start-pos
                (document-length doc))
      (error (format "invalid start-pos: ~a~%document: ~a"
                     start-pos
                     doc)))

    (string-append (string-take doc start-pos)
                   text
                   (string-take-right doc (- (string-length doc)
                                             start-pos))))

  (define (document-contract doc start-pos end-pos)
    (unless (<= end-pos (document-length doc))
      (error "invalid end-pos" end-pos))
    (string-replace doc
                    ""
                    (min start-pos end-pos)
                    (max start-pos end-pos)))

  (define (document-take doc num)
    (string-take doc num))

  (define (document-take-right doc num)
    (string-take-right doc num))

  (define (document-expand doc start-pos amount)
    (unless (> (document-length doc)
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
                   cur-line)))))))
