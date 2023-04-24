(define lsp-server-log-level (make-parameter 'debug))


(define lsp-server-state 'off)
(define lsp-server-version "0.2.2")

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

(cond-expand
 (gambit (define thread-start! (primitive thread-start!))
         (define make-thread (primitive make-thread)))
 (else))

(define-handler (initialize-handler params)
  (let ((root-path (get-root-path params)))
    ;; (thread-start!
    ;;  (make-thread (lambda () ($initialize-lsp-server! root-path))))
    ($initialize-lsp-server! root-path)
    (set! lsp-server-state 'on)
    `((capabilities . ,(append mandatory-capabilities
                               $server-capabilities))
      (serverInfo . ((name . ,$server-name)
                     (version . ,lsp-server-version))))))

(define-handler (initialized-handler params)
  (write-log 'info "LSP server running")
  'null)

(define-handler (shutdown-handler params)
  'null)

(define-handler (lsp-exit-handler params #:exit? #t)
  (write-log 'info "Exiting LSP server.")
  (json-rpc-exit)
  #f)

(define-handler (ignore-request params)
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
           (write-log 'debug (format "file contents updated: ~a"
                                      file-path)))
          (else
           (write-log 'warning (format "file-path not found: ~a"
                                       file-path)))))
  #f)

(define-handler (text-document/did-close params)
  (let ((file-path (get-uri-path params)))
    (when (free-file! file-path)
      (write-log 'info (format "File closed: ~a" file-path)))
    #f))

(define-handler (text-document/did-open params)
  (let ((file-path (get-uri-path params)))
    (if file-path
        (begin ($open-file! file-path) ;;(generate-meta-data! file-path)
               (read-file! file-path)
               (write-log 'debug (format "file contents read: ~a"
                                         file-path)))
        (write-log 'warning (format "file-path not found: ~a"
                                    file-path)))
    #f))

(define-handler (text-document/did-save params)
  (let ((file-path (get-uri-path params)))
    (write-log 'info (format "File saved: ~a." file-path))
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
          (write-log 'debug
           (format "getting completion suggestions for word ~a."
                   word))
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
            ((json-rpc-error? condition)
             (write-log 'error (format "Error resolving ~a ~a"
                                       mod
                                       id))
             (if (satisfies-log-level? 'debug)
                 (raise (make-json-rpc-internal-error
                         (format "Error resolving ~a ~a"
                                 mod
                                 id)))
                 'null))
            (else (raise condition)))
           (let ((doc (or ($fetch-documentation mod id)
                          "")))
             (cons `(documentation . ,doc)
                   params)))))

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
                  (write-log 'debug
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
    (write-log 'info (format "Loading file: ~a." file-path))
    (guard (condition
            (else (write-log 'error (format "Error loading file: ~a."
                                            file-path))))
           (load file-path))
    #f))

(define (parameterize-and-run out-port thunk)
  (parameterize
      ((server-out-port out-port)
       ;;(json-rpc-log-file "/tmp/example.log")
       ;; logging to stderr may cause problems with some clients
       ;; (particularly vscode), so we silence log messages from json-rpc lib.
       (json-rpc-log-level 'silent)
       ;; log messages are sent using window/logMessage notification,
       ;; so no problem allowing them here.
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
          ("window/logMessage" . ,ignore-request)
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
  (parameterize (($tcp-read-timeout #f))
    (let ((listener ($tcp-listen port-num)))
      (guard
       (condition (else
                   (write-log 'error
                              (format "LSP-SERVER: JSON-RPC error: ~a"
                                      condition))
                   (cond-expand (chicken (print-error-message condition))
                                (else (display condition)))
                   (write-log 'info "Exiting.")
                   (exit 1)))
       (let loop ()
         (call-with-values (lambda () ($tcp-accept listener))
           (lambda (in-port out-port)
             (parameterize-and-run
              out-port
              (lambda ()
                (write-log 'info
                           (format "listening on port ~a with log level ~a~%"
                                   port-num
                                   (json-rpc-log-level)))
                (cond ((eqv? (json-rpc-loop in-port out-port) 'json-rpc-exit)
                       (close-input-port in-port)
                       (close-output-port out-port)
                       ($tcp-close listener))
                      (else
                       (write-log 'info "Accepted incoming request")
                       (loop))))))))))))

(define (parameterize-log-levels thunk)
  (parameterize ((log-level (lsp-server-log-level))
                 (json-rpc-log-level (lsp-server-log-level)))
    (thunk)))

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
