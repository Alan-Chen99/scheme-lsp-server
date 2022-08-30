(define-module (lsp-server)

#:export (lsp-server-log-level
          lsp-spawner-start
          lsp-server-start/stdio
          lsp-server-start/tcp
          lsp-server-version)

#:re-export (lsp-server-log-file)

#:use-module ((scheme base)
              #:select (define-record-type
                        guard
                        let-values))
#:use-module (scheme file)
#:use-module (scheme write)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-8) ;; receive
#:use-module (srfi srfi-13)
#:use-module (srfi srfi-18)
#:use-module (srfi srfi-28) ;; simple-format
#:use-module (srfi srfi-43)
#:use-module (srfi srfi-69)

#:use-module (json-rpc)
#:use-module (json-rpc lolevel)
#:use-module (ice-9 documentation)
#:use-module (ice-9 session)

#:use-module (system vm program)

#:use-module (lsp-server guile util)
#:use-module (lsp-server guile)
#:use-module (lsp-server document)
#:use-module (lsp-server parse)
#:use-module (lsp-server private)

#:use-module (lsp-server adapter)

#:declarative? #f)

(define file-table (make-hash-table))
(define file-table-mutex (make-mutex "file-table"))

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

(define (read-file! path)
  (mutex-lock! file-table-mutex)
  (let ((result
         (cond ((hash-table-exists? file-table path)
                (hash-table-ref file-table path))
               (else
                (let ((doc (call-with-input-file path read-document)))
                  (hash-table-update!/default file-table
                                              path
                                              (lambda (v)
                                                doc)
                                              doc)
                  doc)))))
    (mutex-unlock! file-table-mutex)
    result))


(define (update-file! path . args)
  (let ((raw-change-contents
         (if (null? args)
             #f
             (car args))))

    (cond (raw-change-contents
           (mutex-lock! file-table-mutex)
           (let ((result
                  (if (hash-table-exists? file-table path)
                      (hash-table-update! file-table
                                          path
                                          (lambda (old-doc)
                                            (apply-all-changes
                                             raw-change-contents
                                             old-doc)))
                      (hash-table-set! file-table
                                       path
                                       (begin
                                         (write-log 'debug
                                                    (format "reading file from disk: ~a" path))
                                         (call-with-input-file path
                                           (lambda (p)
                                             (apply-all-changes
                                              raw-change-contents
                                              (read-document p)))))))))
             (mutex-unlock! file-table-mutex)
             result))
          (else #f))))

(define (free-file! path)
  (mutex-lock! file-table-mutex)
  (let* ((file (hash-table-ref/default file-table path #f))
         (result (cond ((not file)
                       (write-log 'warning
                                  "trying to freeing a non-existing file"
                                  path)
                       #f)
                      (else (hash-table-delete! file-table path)
                            #t))))
    (mutex-unlock! file-table-mutex)
    result))

(define (get-word-under-cursor params)
  (define file-path (get-uri-path params))
  (define doc (read-file! file-path))
  (define contents (document-contents doc))
  (define contents-length (document-length doc))
  (define line-number (alist-ref* '(position line) params))
  (define char-number (alist-ref* '(position character) params))
  (define text-pos
    (min (line/char->pos doc line-number char-number)
         (max (- (document-length doc) 1)
              0)))
  (write-log 'debug (format "get-word-under-cursor: params: ~a"
                            params))

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

(define (parse-change-contents change-contents)
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
  (cond ((not range)
         ;; if range is not set (#f), the client will send the complete file.
         text)
        (else
         (let* ((normalized-range (normalize-range range))
                (start-pos
                  (line/char->pos doc
                                  (range-start-line normalized-range)
                                  (range-start-char normalized-range)))
                (end-pos
                 (line/char->pos doc
                                 (range-end-line normalized-range)
                                 (range-end-char normalized-range)))
                (old-len (- end-pos start-pos))
                (new-len (string-length text))
                (contracted-doc
                 (document-contract doc
                                    (min start-pos end-pos)
                                    (max start-pos end-pos))))
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
           (document-insert contracted-doc
                            text
                            (min start-pos end-pos))))))

(define (apply-all-changes raw-changes doc)
  (vector-fold (lambda (doc change)
                 (apply-change (parse-change-contents change)
                               doc))
               doc
               raw-changes))

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

(define lsp-server-log-level (make-parameter 'debug))
(define server-out-port (make-parameter #f))

(define lsp-server-state 'off)
(define lsp-server-version "0.2.0")

(define listening-threads '())
(define listening-threads-mutex (make-mutex))

(define (shutting-down?)
  (eqv? lsp-server-state 'shutdown))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler (handler params #:exit? exit?) body ...)
     (define (handler params)
       (when (and (shutting-down?) (not exit?))
         (raise (make-json-rpc-invalid-request-error
                 "Only exit request allowed after shutdown.")))
       (write-log 'debug
                  (format "Handler ~a called with params ~s"
                          'handler
                          (truncate-string (format "~a" params))))
       body ...))
    ((define-handler (handler params) body ...)
     (define-handler (handler params #:exit? #f) body ...))))

;; all implementations should implement at least following
;; capabilities.
(define mandatory-capabilities
  '((textDocumentSync . ((save . #t)
                         (openClose . #t)
                         (change . 2)))
    (hoverProvider . #t)
    (completionProvider . ((resolveProvider . #t)))
    (signatureHelpProvider . ())))

(define-handler (initialize-handler params)
  (let ((root-path (get-root-path params)))
    (when ($initialize-lsp-server! root-path)
      (json-rpc-send-notification (server-out-port)
                                  "$/logTrace"
                                  `((message . "LSP server initialized")))
      (write-log 'info "LSP server initialized"))
    (set! lsp-server-state 'on)
    `((capabilities . ,(append mandatory-capabilities
                               $server-capabilities))
      (serverInfo . ((name . ,$server-name)
                     (version . ,lsp-server-version))))))

(define-handler (initialized-handler params)
  (write-log 'info
             "LSP Server initialized")
  (json-rpc-send-notification (server-out-port)
                                  "$/logTrace"
                                  `((message . "LSP server running")))
  'null)

(define-handler (shutdown-handler params)
  (write-log 'info
             "shutting down")
  'null)

(define-handler (lsp-exit-handler params #:exit? #t)
  (write-log 'info "exiting")
  (json-rpc-exit)
  #f)

(define-handler (ignore-request params)
  (write-log 'debug (format "ignoring request. Params: ~a" params))
  #f)

(define-handler (text-document/definition params)
  (let* ((editor-word (get-word-under-cursor params))
         (file-path (get-uri-path params))
         (mod-name (and file-path
                        (parse-library-name-from-file file-path))))
    (if editor-word
        (let* ((word-text (editor-word-text editor-word))
               (def-locs ($get-definition-locations mod-name
                                                    (string->symbol word-text))))
          (cond ((not (null? def-locs))
                 (let ((v (list->vector def-locs)))
                   (write-log 'debug
                              (format "$get-definition-locations resulted in ~a"
                                      v))
                   v))
                (else
                 (write-log 'debug
                            (format "no definitions found for ~a"
                                    (editor-word-text editor-word)))
                 'null)))
        'null)))

(define-handler (text-document/did-change params)
  (let* ((file-path (get-uri-path params))
         (changes
          (string-tokenize
           (alist-ref 'text
                      (vector-ref (alist-ref 'contentChanges params) 0))
           (char-set #\newline #\return)))
         (file-already-read?
          (begin
            (mutex-lock! file-table-mutex)
            (let ((res (hash-table-exists? file-table file-path)))
              (mutex-unlock! file-table-mutex)
              res))))
    (cond ((and file-path file-already-read?)
           ;;(generate-meta-data! file-path)
           (read-file! file-path)
           (update-file! file-path
                         (alist-ref 'contentChanges params))
           (write-log 'debug
                      (format "file contents read: ~a"
                              file-path))
           ;; TODO first make this portable (i.e. not relying on /tmp), then
           ;; uncomment it.
           ;; We leave it in, since it's helpful when debugging problems
           ;; regarding the internal document representation (out-of-index etc).
           (when (satisfies-log-level? 'debug)
             (let ((tmp-file (string-append "/tmp/" (remove-slashes file-path))))
               (write-log 'debug
                          (format "dumping content read into ~a" tmp-file))
               (mutex-lock! file-table-mutex)
               (with-output-to-file tmp-file
                 (lambda ()
                   (let ((doc (hash-table-ref file-table file-path)))
                     (display (document-contents doc)))))
               (mutex-unlock! file-table-mutex))))

          (file-path
           (update-file! file-path
                         (alist-ref 'contentChanges params))
           (write-log 'debug
                      (format "file contents updated: ~a"
                              file-path)))
          (else
           (write-log 'debug
                      (format "file-path not found: ~a"
                              file-path)))))
  #f)

(define-handler (text-document/did-close params)
  (let ((file-path (get-uri-path params)))
    (when (free-file! file-path)
      (write-log 'info "file closed" file-path))
    #f))

(define-handler (text-document/did-open params)
  (let ((file-path (get-uri-path params)))
    (if file-path
        (begin ($open-file! file-path) ;;(generate-meta-data! file-path)
               (read-file! file-path)
               (write-log 'debug
                          (format "file contents read: ~a"
                                  file-path)))
        (write-log 'debug
                   (format "file-path not found: ~a"
                           file-path)))
    #f))

(define-handler (text-document/did-save params)
  (let ((file-path (get-uri-path params)))
    (write-log 'info "file saved.")
    ($save-file! file-path)
    #f))

(define-handler (text-document/completion params)
  (let* ((cur-char-number
          (alist-ref* '(position character) params))
         (editor-word (get-word-under-cursor params))
         (file-path (get-uri-path params))
         (mod-name (and file-path
                        (parse-library-name-from-file file-path))))
    (if (or (not editor-word)
            (< (string-length (editor-word-text editor-word))
               1))
        'null
        (let* ((word (editor-word-text editor-word))
               (suggestions ($apropos-list mod-name word)))
          (write-log 'debug "getting completion suggestions for word "
                     word)
          (write-log 'debug (format "suggestions list: ~a" suggestions))

          `((isIncomplete . #t)
            (items .
                   ,(list->vector
                     (map (lambda (suggestion)
                            (let* ((id-name (car suggestion))
                                   (mod-name-str (stringify (cdr suggestion)))
                                   (start-line (alist-ref* '(position line)
                                                           params))
                                   (start-char (editor-word-start-char
                                                editor-word))
                                   (end-char (editor-word-end-char
                                              editor-word))
                                   (text-edit
                                    `((range . ((start . ((line . ,start-line)
                                                          (character . ,start-char)))
                                                (end . ((line . ,start-line)
                                                        (character . ,cur-char-number)))))
                                      (newText . ,id-name))))
                              `((label . ,id-name)
                                (insertText . ,id-name)
                                (sortText . ,id-name)
                                (textEdit . ,text-edit)
                                (data . ((identifier . ,id-name) (module . ,mod-name-str))))))
                          suggestions))))))))


(define-handler (completion-item/resolve params)
  (let* ((id (string->symbol
             (alist-ref* '(data identifier) params)))
        (file-path (get-uri-path params))
        (mod-name (and file-path
                       (parse-library-name-from-file file-path)))
        (mod (let ((m (alist-ref* '(data module) params)))
               (if m
                   (split-module-name m)
                   mod-name))))
    (write-log 'debug (format "params: ~a" params))
    (guard (condition
            (#t (begin
                  (write-log 'warning
                             "error resolving "
                             mod
                             id)
                  (if (satisfies-log-level? 'debug)
                      (raise (make-json-rpc-internal-error
                              (format "Error resolving ~a ~a"
                                      mod
                                      id)))
                      'null))))
           (begin
             (let ((doc (or ($fetch-documentation mod id)
                            "")))
               (cons `(documentation . ,doc)
                     params))))))

(define (fetch-signature-under-cursor params)
  (let* ((editor-word
          (get-word-under-cursor params))
         (file-path (get-uri-path params))
         (mod-name (and file-path
                        (parse-library-name-from-file file-path))))

    (if (and editor-word (not (string=? (editor-word-text editor-word)
                                        "")))
        (begin
          (let* ((cur-word (editor-word-text editor-word))
                 (signature ($fetch-signature mod-name
                                              (string->symbol cur-word))))
            (if (not signature)
                (begin
                  (write-log 'warning
                             (format "no signature found for: ~a" cur-word))
                  "")
                signature)))
        "")))

(define-handler (text-document/signature-help params)
  (let ((signature (fetch-signature-under-cursor params)))
    (if signature
        `((signatures . ,(vector `((label . ,signature)))))
        `((signatures . ,(vector))))))

(define-handler (text-document/hover params)
  (write-log 'debug
             (format "hover with params: ~a" params))

  (let ((signature (fetch-signature-under-cursor params)))
    (if (and signature
             (not (equal? signature ""))
             (not (equal? signature 'null)))
        `((contents . ((kind . "plaintext")
                       (value . ,signature))))
        'null)))

(define-handler (custom/load-file params)
  (let ((file-path (get-uri-path params)))
    (write-log 'debug "loading file: " file-path)
    (guard (condition
            (#t (write-log 'warning "error loading file")))
           (load file-path))
    #f))

(define (parameterize-and-run out-port thunk)
  (parameterize
      ((server-out-port out-port)
       (json-rpc-log-file (lsp-server-log-file))
       (json-rpc-log-level (lsp-server-log-level))
       (log-level (lsp-server-log-level))
       (custom-error-codes '((definition-not-found-error . -32000)
                             (load-error . -32001)))
       (json-rpc-handler-table
        `(("initialize" . ,initialize-handler)
          ("initialized" . ,initialized-handler)
          ("textDocument/definition" . ,text-document/definition)
          ("textDocument/didChange" . ,text-document/did-change)
          ("workspace/didChangeConfiguration" . ,ignore-request)
          ("textDocument/didClose" . ,text-document/did-close)
          ("textDocument/didOpen" . ,text-document/did-open)
          ("textDocument/didSave" . ,text-document/did-save)
          ("textDocument/completion" . ,text-document/completion)
          ("textDocument/hover" . ,text-document/hover)
          ("completionItem/resolve" . ,completion-item/resolve)
          ("textDocument/signatureHelp" . ,text-document/signature-help)
          ("$/setTraceNotification" . ,ignore-request)
          ("$/cancelRequest" . ,ignore-request)
          ("exit" . ,lsp-exit-handler)
          ("shutdown" . ,shutdown-handler)
          ;; custom commands
          ("custom/loadFile" . ,custom/load-file))))
    (thunk)))

(define lsp-server-start/stdio
  (case-lambda
   (()
    (lsp-server-start/stdio (current-input-port) (current-output-port)))
   ((in-port out-port)
    (parameterize-and-run out-port
                          (lambda ()
                            (json-rpc-loop in-port out-port))))))

(define (lsp-server-start/tcp port-num)
  (parameterize ((tcp-read-timeout #f))
    (let ((listener (tcp-listen port-num)))
      (write-log 'info
                 (format "listening on port ~a with log level ~a~%"
                         port-num
                         (json-rpc-log-level)))
      (guard
       (condition (#t (begin
                        (write-log 'error (format "JSON-RPC error: ~a" condition))
                        (cond-expand (chicken (print-error-message condition))
                                     (else (display condition)))
                        #f)))
       (let loop ()
         (let-values (((in-port out-port)
                       (tcp-accept listener)))
           (parameterize-and-run
            out-port
            (lambda () (if (eqv? (json-rpc-loop in-port out-port) 'json-rpc-exit)
                           (begin
                             (close-input-port in-port)
                             (close-output-port out-port)
                             (tcp-close listener))
                           (begin
                             (write-log 'debug (format "Accepted incoming request"))
                             (loop)))))))))))

(define (parameterize-log-levels thunk)
  (parameterize ((log-level (lsp-server-log-level))
                 (json-rpc-log-level (lsp-server-log-level)))
    (thunk)))

(define (lsp-spawner-loop command-port-num)
  (write-log 'debug
             (format "lsp-spawner-loop: ~a [log level: ~a]"
                     command-port-num
                     (lsp-server-log-level)))
  (parameterize (($tcp-read-timeout #f))
    (let ((listener ($tcp-listen command-port-num)))
      (let loop ()
        (let-values (((in-port out-port)
                      (guard
                       (condition
                        (#t (begin
                              (write-log 'error
                                         (string-append
                                          (format "Unable to open command listener of LSP server on port ~a.~%"
                                                  command-port-num)
                                          "Is the server already running?"
                                          "If not, try changing the LSP's command port of your LSP client."))
                              (exit 1))))
                       ($tcp-accept listener))))
          (thread-start!
           (make-thread
            (lambda () (lsp-server-start/stdio in-port out-port))))
          (loop))))))

(define (lsp-spawner-start port-num)
  (parameterize-log-levels
   (lambda ()
     (write-log 'info
                         (format "LSP command server started on port ~a"
                                 port-num))
     (thread-start!
      (make-thread (lambda () (lsp-spawner-loop port-num)))))))

(define (remove-slashes path)
  (define new-path (make-string (string-length path)))
  (string-fold (lambda (c i)
                 (if (char=? c #\/)
                     (string-set! new-path i #\.)
                     (string-set! new-path i c))
                 (+ i 1))
               0
               path)
  new-path)

(define (truncate-string str)
  (define max-length 40)
  (if (< (string-length str) max-length)
      str
      (string-append (string-take str max-length) " ...")))
