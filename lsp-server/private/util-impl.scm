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

(define log-level (make-parameter 'info))

(define lsp-server-log-file (make-parameter #f))

(define server-out-port (make-parameter (current-output-port)))
(define trace-level (make-parameter 'messages))

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

(define (split-module-name mod)
  (map string->symbol
       (string-tokenize (substring mod
                                   1
                                   (- (string-length mod) 1)))))

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
      (uri-decode (parse-uri uri))
      #f))

(define (get-root-path params)
  (alist-ref* '(rootPath) params))

(define (parse-uri uri)
  (define file-prefix "file://")
  (define file-prefix-length 7)
  (define uri-length (string-length uri))
  (if (<= uri-length file-prefix-length)
      (error "invalid uri" uri)
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

(define (symbols->string sl)
  (string-append "(" (string-join (map symbol->string sl)) ")"))

  ;;; merge contents of hash-tables source and target, updating
  ;;; the contents of the latter.
(define (hash-table-merge-updating! target source)
  (for-each (lambda (k)
              (hash-table-set! target
                               k
                               (hash-table-ref source k)))
            (hash-table-keys source))
  target)



(define (get-log-level symb)
  (cond ((eqv? symb 'error) 0)
        ((eqv? symb 'warning) 1)
        ((eqv? symb 'info) 2)
        ((eqv? symb 'debug) 3)
        (else (error "invalid log level" symb))))

(define (satisfies-log-level? target-level)
  (>= (get-log-level (log-level)) (get-log-level target-level)))

(define (write-log target-level msg)
  (let ((msg-type (+ (get-log-level target-level) 1)))
    (when (satisfies-log-level? target-level)
      (json-rpc-send-notification
       "window/logMessage"
       `((message . ,msg)
         (type . ,msg-type))
       (server-out-port)))))

(define (stringify elem)
  (format "~a" elem))

(define (module-name->string mod-name)
  (cond ((list? mod-name)
         (let ((lib-parts (map symbol->string mod-name)))
           (string-append "("
                          (string-join lib-parts " ")
                          ")")))
        (else (symbol->string mod-name))))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (string-lines str)
  (with-input-from-string str
    (lambda ()
      (let loop ((line (read-line))
                 (res '()))
        (if (eof-object? line)
            (reverse res)
            (loop (read-line)
                  (cons line res)))))))


