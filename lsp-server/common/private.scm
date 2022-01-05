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

        append-lines
        append-lines*

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
        (memq char '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\<
                     #\= #\> #\? #\@ #\^ #\_ #\~))))

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
      (flush-output-port error-port)))
  ))


