(define-library (lsp-server private)

(export make-apropos-info
        apropos-info-module
        apropos-info-name
        apropos-info-type
        apropos-info-object

        make-editor-word
        editor-word-text
        editor-word-end-char
        editor-word-end-line
        editor-word-start-char
        editor-word-start-line

        make-range
        range?
        range-start-line
        range-start-char
        range-end-line
        range-end-char

        make-change-contents
        change-contents-range
        change-contents-text

        apply-change
        append-lines
        append-lines*

        parse-change-contents

        intersperse
        
        join-module-name
        split-module-name

        alist-ref*
        get-root-path
        get-uri-path
        parse-uri

        identifier-char?
        symbols->string
        hash-table-merge-updating!

        write-log
        log-level)

(import (scheme base)
        (scheme char)
        (scheme write)
        r7rs
        (srfi 1)
        (srfi 28)
        (srfi 69)
        (srfi 130))

(cond-expand
 (chicken (import (only (chicken base) intersperse))))

(begin
  (define-record-type <apropos-info>
    (make-apropos-info module name type object)
    apropos-info?
    (module apropos-info-module)
    (name apropos-info-name)
    (type apropos-info-type)
    (object apropos-info-object))

  (define-record-type <editor-word>
    (make-editor-word text start-line end-line start-char end-char)
    editor-word?
    (text editor-word-text)
    (start-line editor-word-start-line)
    (end-line editor-word-end-line)
    (start-char editor-word-start-char)
    (end-char editor-word-end-char))

  (define-record-type <change-contents>
    (make-change-contents range text)
    change-contents?
    (range change-contents-range)
    (text change-contents-text))

  (define-record-type <range>
    (make-range start-line start-char end-line end-char)
    range?
    (start-line range-start-line)
    (start-char range-start-char)
    (end-line range-end-line)
    (end-char range-end-char))

  (define (delete-lines lines start end)
    (define len (length lines))
    (append (take lines start)
            (take-right lines (- len end 1))))

  (define (string-split-keeping str sep)
    (define lst (string->list str))
    (let loop ((rem lst)
               (cur-str '())
               (result '()))
      (cond ((null? rem)
             (if (null? cur-str)
                 (reverse result)
                 (reverse (cons (list->string (reverse cur-str))
                                result))))
            ((char=? (car rem) sep)
             (loop (cdr rem)
                   '()
                   (cons (list->string (reverse (cons sep cur-str)))
                         result)))
            (else (loop (cdr rem)
                        (cons (car rem) cur-str)
                        result)))))


  (define (append-lines lines)
    (let loop ((rem lines)
               (cur-word "")
               (result '()))
      (cond ((null? rem)
             (reverse (if (not (string=? cur-word ""))
                          (cons cur-word result)
                          result)))
            (else
             (let* ((fst (car rem))
                    (len (string-length fst)))
               (if (= len 0)
                   (loop (cdr rem)
                         cur-word
                         result)
                   (if (char=? (string-ref fst (- len 1))
                               #\newline)
                       (loop (cdr rem)
                             ""
                             (cons (string-append cur-word fst)
                                   result))
                       (loop (cdr rem)
                             (string-append cur-word fst)
                             result))))))))

  (define (append-lines* . args)
    (append-lines (apply append args)))


  (define (split-lines text)
    (define unix-text
      (string-remove (lambda (s)
                       (char=? s #\return))
                     text))
    (write-log 'info
               (format "split-lines: ~s" text))

    (string-split-keeping unix-text #\newline))


  (define (apply-delete-change change doc)
    (define range (change-contents-range change))
    (define start-line (range-start-line range))
    (define start-char (range-start-char range))
    (define end-line (range-end-line range))
    (define end-char (range-end-char range))
    (cond ((= start-line end-line)
           (append-lines*
            (take doc start-line)
            (split-lines
             (string-replace (list-ref doc start-line)
                             ""
                             start-char
                             end-char))
            (take-right doc (- (length doc)
                               end-line
                               1))))
          (else
           (append-lines*
            (take doc start-line)
            (let* ((first-line (list-ref doc start-line))
                   (new-line (string-replace first-line
                                             ""
                                             start-char
                                             (string-length first-line))))
              (if (string=? new-line "")
                  '()
                  (split-lines new-line)))
            (split-lines
             (string-replace (list-ref doc end-line)
                             ""
                             0
                             end-char))
            (take-right doc (- (length doc)
                               end-line
                               1))))))


  (define (apply-insert-change change doc)
    (define range (change-contents-range change))
    (define text (change-contents-text change))
    (define start-line (range-start-line range))
    (define start-char (range-start-char range))
    (define end-line (range-end-line range))
    (define end-char (range-end-char range))

    (cond ((= start-line end-line)
           (append-lines*
            (take doc start-line)
            (split-lines
             (let ((old-line (list-ref doc start-line)))
               (string-replace old-line
                               (car (append-lines text))
                               start-char
                               end-char)))
            (take-right doc (- (length doc)
                               end-line
                               1))))
          (else
           (append-lines*
            (take doc start-line)
            (let* ((first-line (list-ref doc start-line))
                   (new-line (string-append
                              (string-take first-line start-char)
                              (apply string-append
                                     (append-lines text)))))
              (display (format "new-line: ~s~%" new-line))
              (display (format "text: ~s~%" text))
              (display (format "append-lines: ~s~%" (append-lines text)))
              (if (string=? new-line "")
                  '()
                  (split-lines new-line)))
            ;; (if (> (length text) 2)
            ;;     (append-lines* (drop text 1) (drop-right text 1))
            ;;     '())
            ;; (let ((last-line (last text))
            ;;       (old-line (list-ref doc end-line)))
            ;;   (display (format "old-line: ~s~%" old-line))
            ;;   (split-lines
            ;;    (string-append last-line
            ;;                   old-line))
            ;;   #;
            ;;   (split-lines (string-replace old-line
            ;;   last-line
            ;;   0
            ;;   (min (string-length old-line)
            ;;   end-char))))
            (take-right doc (- (length doc)
                               start-line
                               1))))))

  #;
  (define (apply-insert-change change doc)
    (define range (change-contents-range change))
    (define text (change-contents-text change))
    (define start-line (range-start-line range))
    (define start-char (range-start-char range))
    (define end-line (range-end-line range))
    (define end-char (range-end-char range))

    (cond ((= start-line end-line)
           (append-lines*
            (take doc start-line)
            (split-lines
             (let ((old-line (list-ref doc start-line)))
               (string-replace old-line
                               (car text)
                               start-char
                               end-char)))
            (take-right doc (- (length doc)
                               end-line
                               1))))
          (else
           (append-lines*
            (take doc start-line)
            (let* ((first-line (list-ref doc start-line))
                   (new-line (string-replace first-line
                                             (car text)
                                             start-char
                                             (string-length first-line))))
              (if (string=? new-line "")
                  '()
                  (split-lines new-line)))
            (if (> (length text) 2)
                (append-lines* (drop text 1) (drop-right text 1))
                '())
            (let ((last-line (last text))
                  (old-line (list-ref doc end-line)))
              (display (format "old-line: ~s~%" old-line))
              (split-lines
               (string-append last-line
                              old-line))
              #;
              (split-lines (string-replace old-line
                                           last-line
                                           0
                                           (min (string-length old-line)
                                                end-char))))
            (take-right doc (- (length doc)
                               end-line
                               1))))))

  (define (normalize-range range)
    (define start-line (range-start-line range))
    (define end-line (range-end-line range))
    (if (<= start-line end-line)
        range
        (make-range end-line
                    (range-end-char range)
                    start-line
                    (range-start-char range))))
  
  (define (apply-change change doc)
    (define text (change-contents-text change))
    (define range (change-contents-range change))
    (define normalized-change
      (make-change-contents (normalize-range range)
                            text))
    
    (if (null? text)
        (apply-delete-change normalized-change doc)
        (apply-insert-change normalized-change doc)))

#;  
  (define (apply-change change doc)
    (define text (change-contents-text change))
    (define range (change-contents-range change))
    (define start-line (range-start-line range))
    (define start-char (range-start-char range))
    (define end-line (range-end-line range))
    (define end-char (range-end-char range))
    (cond ((= start-line end-line)
           (let ((new-line
                  (string-replace (list-ref doc start-line)
                                  (cond ((and (list? text)
                                              (null? text))
                                         "")
                                        ((list? text)
                                         (car text))
                                        ((string? text)
                                         text)
                                        (else
                                         (write-log 'error
                                                    (format "text not supported: ~s"
                                                            text))
                                         (error "apply-change")))
                                  start-char
                                  end-char)))
             (append (take doc start-line)
                     (split-lines new-line)
                     (take-right doc (- (length doc)
                                        end-line
                                        1)))))
          (else
           (let loop ((index start-line)
                      (remaining-text text)
                      (new-lines '()))
             (cond ((> index end-line)
                    (append (take doc start-line)
                            (filter (lambda (s)
                                      (not (string=? s "")))
                                    (reverse new-lines))
                            (take-right doc (- (length doc)
                                               end-line
                                               1))))
                   ((= index start-line)
                    (let* ((old-line (list-ref doc index))
                           (new-line
                            (string-replace old-line
                                            (if (null? remaining-text)
                                                ""
                                                (car remaining-text))
                                            start-char
                                            (string-length old-line))))
                      (loop (+ index 1)
                            (if (null? remaining-text)
                                '()
                                (cdr remaining-text))
                            (append (split-lines new-line)
                                    new-lines))))
                   ((= index end-line)
                    (let ((new-line
                           (string-replace (list-ref doc index)
                                           (if (null? remaining-text)
                                               ""
                                               (car remaining-text))
                                           0
                                           end-char)))
                      (loop (+ index 1)
                            (if (null? remaining-text)
                                '()
                                (cdr remaining-text))
                            (append (split-lines new-line)
                                    new-lines))))
                   (else
                    (loop (+ index 1)
                          (if (null? remaining-text)
                              '()
                              (cdr remaining-text))
                          (append (split-lines (if (null? remaining-text)
                                                   ""
                                                   (car remaining-text)))
                                  new-lines))))))))

  #;
  (define (apply-change change doc)
    (define text (change-contents-text change))
    (define range (change-contents-range change))
    (define start-line (range-start-line range))
    (define start-char (range-start-char range))
    (define end-line (range-end-line range))
    (define end-char (range-end-char range))
    (cond ((= start-line end-line)
           (let ((new-line
                  (string-replace (list-ref doc start-line)
                                  (cond ((and (list? text)
                                              (null? text))
                                         "")
                                        ((list? text)
                                         (car text))
                                        ((string? text)
                                         text)
                                        (else
                                         (write-log 'error
                                                    (format "text not supported: ~s"
                                                            text))
                                         (error "apply-change")))
                                  start-char
                                  end-char)))
             (append (take doc start-line)
                     (split-lines new-line)
                     (take-right doc (- (length doc)
                                        end-line
                                        1)))))
          (else
           (let loop ((index start-line)
                      (remaining-text text)
                      (new-lines '()))
             (cond ((> index end-line)
                    (append (take doc start-line)
                            (filter (lambda (s)
                                      (not (string=? s "")))
                                    (reverse new-lines))
                            (take-right doc (- (length doc)
                                               end-line
                                               1))))
                   ((= index start-line)
                    (let* ((old-line (list-ref doc index))
                           (new-line
                            (string-replace old-line
                                            (if (null? remaining-text)
                                                ""
                                                (car remaining-text))
                                            start-char
                                            (string-length old-line))))
                      (loop (+ index 1)
                            (if (null? remaining-text)
                                '()
                                (cdr remaining-text))
                            (append (split-lines new-line)
                                    new-lines))))
                   ((= index end-line)
                    (let ((new-line
                           (string-replace (list-ref doc index)
                                           (if (null? remaining-text)
                                               ""
                                               (car remaining-text))
                                           0
                                           end-char)))
                      (loop (+ index 1)
                            (if (null? remaining-text)
                                '()
                                (cdr remaining-text))
                            (append (split-lines new-line)
                                    new-lines))))
                   (else
                    (loop (+ index 1)
                          (if (null? remaining-text)
                              '()
                              (cdr remaining-text))
                          (append (split-lines (if (null? remaining-text)
                                                   ""
                                                   (car remaining-text)))
                                  new-lines))))))))

  (define (parse-change-contents contents)
    (define change-contents (vector-ref contents 0))
    (define range-contents (alist-ref 'range change-contents))
    (define range-start (alist-ref 'start range-contents))
    (define range-end (alist-ref 'end range-contents))
    (define range-text (alist-ref 'text change-contents))
    (define range
      (make-range (alist-ref 'line range-start)
                  (alist-ref 'character range-start)
                  (alist-ref 'line range-end)
                  (alist-ref 'character range-end)))
    
    (define text-lines
      (if (null? range-text)
          '()
          (split-lines range-text)))
    (make-change-contents range text-lines))

  (cond-expand
   (guile
    (define (intersperse lst delim)
      (let loop ((remaining lst)
                 (result '()))
        (cond ((null? remaining)
               (reverse result))
              ((null? (cdr remaining))
               (reverse (cons (car remaining) result)))
              (else
               (loop (cdr remaining)
                     (cons delim
                           (cons (car remaining)
                                 result))))))))
   (else))

  (define (join-module-name mod)
    (if mod
        (apply string-append (append '("(")
                                     (intersperse (map symbol->string mod)
                                                  " ")
                                     '(")")))
        #f))

  (define (split-module-name mod)
    (map string->symbol
         (string-split
          (substring mod
                     1
                     (- (string-length mod) 1))
          " ")))

  (define (alist-ref key alist)
    (let ((p (assoc key alist)))
      (if p
          (cdr p)
          #f)))

  (define (alist-ref* keys alist)
    (let loop ((keys keys)
               (alist alist))
      (cond ((null? keys) #f)
            ((null? (cdr keys))
             (alist-ref (car keys) alist))
            (else
             (let ((sub-alist (alist-ref (car keys) alist)))
               (if sub-alist
                   (loop (cdr keys) sub-alist)
                   #f))))))

  (define (get-uri-path params)
    (define uri (alist-ref* '(textDocument uri)
                            params))
    (if uri
        (parse-uri uri)
        #f))

  (define (get-root-path params)
    (alist-ref* '(rootPath) params))

  (define (parse-uri uri)
    (define file-prefix "file://")
    (define file-prefix-length 7)
    (define uri-length (string-length uri))
    (if (<= uri-length file-prefix-length)
        #f
        (let ((prefix (substring uri 0 file-prefix-length)))
          (if (equal? prefix file-prefix)
              (substring uri file-prefix-length uri-length)
              (error "invalid uri" uri)))))

  (define (identifier-char? char)
    (or (char-alphabetic? char)
        (char-numeric? char)
        (memq char '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~))))

  (define (char-bracket? char)
    (memq char '(#\( #\) #\{ #\} #\[ #\])))


  (cond-expand
   (guile (define (alist-ref key alist)
            (define match (assoc key alist))
            (if match
                (cdr match)
                #f)))
   (else))

  (define (symbols->string sl)
    (string-append "(" (string-join (map symbol->string sl)) ")"))

  (define (hash-table-merge-updating! target source)
    (for-each (lambda (k)
                (hash-table-set! target
                                 k
                                 (hash-table-ref source k)))
              (hash-table-keys source))
    target)

  (define log-level (make-parameter 3))

  (define (get-log-level symb)
    (cond ((eqv? symb 'error) 0)
          ((eqv? symb 'warning) 1)
          ((eqv? symb 'info) 2)
          ((eqv? symb 'debug) 3)
          (else (error "invalid log level" symb))))

  (define (write-log type msg . args)
    (define level (get-log-level type))
    (define error-port (current-error-port))
    (when (<= level (log-level))
      (display (format "[~a] ~a"
                       (string-upcase (symbol->string type))
                       msg)
               error-port)
      (when (not (null? args))
        (display ": " error-port)
        (map (lambda (s)
               (display (format "~a    " s) error-port))
             args))
      (newline error-port)
      (flush-output-port error-port))))
)
