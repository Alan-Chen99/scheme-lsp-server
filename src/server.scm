(define lsp-server-log-level (make-parameter 'debug))
(define lsp-server-state 'off)

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
                         (change . 2)))
    (hoverProvider . #t)
    (completionProvider . ((resolveProvider . #t)))
    (signatureHelpProvider . ())))

(define-handler (initialize-handler params)
  (define root-path (get-root-path params))
  (when ($initialize-lsp-server root-path)
    (write-log 'info "LSP server initialized"))
  (set! lsp-server-state 'on)
  `((capabilities . ,(append mandatory-capabilities
                             $server-capabilities))
    (serverInfo . ((name . ,$server-name)
                   (version . "0.1.0")))))

(define-handler (initialized-handler params)
  (write-log 'info
             "initialized")
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
  (define editor-word (get-word-under-cursor params))
  (write-log 'debug (format "got word: ~a" editor-word))
  (if editor-word
      (let ((def-locs ($get-definition-locations (editor-word-text editor-word))))
        (if (not (null? def-locs))
            (let ((v (list->vector def-locs)))
              (write-log 'debug
                         (format "$get-definition-locations resulted in ~a"
                                 v))
              v)
            (begin
              (write-log 'debug
                         (format "no definitions found for ~a"
                                 (editor-word-text editor-word)))
              'null)))
      'null))

(define-handler (text-document/did-change params)
  (define file-path (get-uri-path params))
  (define changes
    ($string-split
     (alist-ref 'text
                (vector-ref (alist-ref 'contentChanges params) 0))
     "\r\n"
     'infix))
  (cond ((and file-path (not (hash-table-ref/default file-table file-path #f)))
         ($open-file file-path)
         (read-file! file-path)
         (update-file! file-path
                       (alist-ref 'contentChanges params))
         (write-log 'debug
                    (format "file contents read: ~a"
                            file-path)))
        (file-path
         (update-file! file-path
                       (alist-ref 'contentChanges params))
         (write-log 'debug
                    (format "file contents updated: ~a"
                            file-path)))
        (else
         (write-log 'debug
                    (format "file-path not found: ~a"
                            file-path))))
  #f)

(define-handler (text-document/did-close params)
  (define file-path (get-uri-path params))
  (when (free-file! file-path)
    (write-log 'info "file closed" file-path))
  #f)

(define-handler (text-document/did-open params)
  (define file-path (get-uri-path params))
  (if file-path
      (begin ($open-file file-path)
             (read-file! file-path)
             (let ((tmp-file (string-append "/tmp/" (remove-slashes file-path))))
               (write-log 'debug
                          (format "dumping content read into ~a" tmp-file))
               #;
               (with-output-to-file tmp-file
                       (lambda ()
                         (display (hash-table-ref (file-table) file-path)))))
             (write-log 'debug
                        (format "file contents read: ~a"
                                file-path)))
      (write-log 'debug
                 (format "file-path not found: ~a"
                         file-path)))
  #f)

(define-handler (text-document/did-save params)
  (define file-path (get-uri-path params))
  (write-log 'info "file saved.")
  ($save-file file-path)
  #f)

(define-handler (text-document/completion params)
  (define cur-char-number
    (alist-ref* '(position character) params))
  (define editor-word (get-word-under-cursor params))
  (write-log 'debug
             (format "editor-word: ~a, start-char: ~a, end-char: ~a~%"
                     (editor-word-text editor-word)
                     (editor-word-start-char editor-word)
                     (editor-word-end-char editor-word)))
  (if (or (not editor-word)
          (< (string-length (editor-word-text editor-word))
             3))
      'null
      (let* ((word (editor-word-text editor-word))
             (suggestions ($apropos-list word)))
        (write-log 'debug "getting completion suggestions for word "
                   word)

        (write-log 'debug
                   (format "suggestions found: ~a~%"
                           (fold (lambda (sug acc)
                                   (format "~a ~a"
                                           acc
                                           (apropos-info-name sug)))
                                 ""
                                 suggestions)))
        `((isIncomplete . #t)
          (items .
                 ,(list->vector
                   (map (lambda (ainfo)
                          (let* ((ainfo-module (apropos-info-module ainfo))
                                 (module-name
                                  (if ainfo-module
                                      (join-module-name ainfo-module)
                                      ""))
                                 (id-name
                                  (symbol->string
                                   (apropos-info-name ainfo)))
                                 (label (if ainfo-module
                                            (format "~a ~a"
                                                    module-name
                                                    id-name)
                                            (format "~a" id-name)))
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
                            `((label . ,label)
                              (insertText . ,id-name)
                              (sortText . ,id-name)
                              (textEdit . ,text-edit)
                              (data . ,(if ainfo-module
                                           `((identifier . ,id-name)
                                             (module . ,module-name))
                                           `((identifier . ,id-name)))))))
                        suggestions)))))))

(define-handler (completion-item/resolve params)
  (define id (string->symbol
              (alist-ref* '(data identifier) params)))
  (define mod (let ((m (alist-ref* '(data module) params)))
                (if m
                    (split-module-name m)
                    '())))
  (guard (condition
          (#t (begin
                (write-log 'warning
                           "error resolving "
                           mod
                           id)
                (if (satisfies-log-level? 'info)
                    (raise (make-json-rpc-internal-error
                            (format "Error resolving ~a ~a"
                                    mod
                                    id)))
                    'null))))
         (begin
           (write-log 'debug
                      (format "Calling $fetch-documentation for mod ~a id ~a"
                              mod id))
           (let ((doc ($fetch-documentation mod id)))
             (cons `(documentation . ,doc)
                   params)))))

(define (fetch-signature-under-cursor params)
  (define editor-word
    (get-word-under-cursor params))
  (if editor-word
      (let* ((cur-word (editor-word-text editor-word))
             (matches (filter (lambda (ap)
                                (string=? (symbol->string (apropos-info-name ap))
                                          cur-word))
                              ($apropos-list cur-word))))
        (if (null? matches)
            (begin
              (write-log 'warning
                         (format "no signature found for: ~a" cur-word))
              'null)
            (guard
             (condition
              (#t (begin (write-log 'warning
                                    (format "Unable to fetch signature of `~a`"
                                            cur-word))
                         'null)))
             (let* ((ainfo (car matches))
                    (signature
                     ($fetch-signature (apropos-info-module ainfo)
                                       (apropos-info-name ainfo))))
               signature))))
      ""))

(define-handler (text-document/signature-help params)
  (let ((signature (fetch-signature-under-cursor params)))
    `((signatures . ,(vector `((label . ,signature)))))))

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
  (define file-path (get-uri-path params))
  (write-log 'debug "loading file: " file-path)
  (guard (condition
          (#t (write-log 'warning "error loading file")))
         (load file-path))
  #f)

(define (parameterize-and-run thunk)
  (parameterize
      ((json-rpc-log-level (lsp-server-log-level))
       (log-level (lsp-server-log-level))
       (custom-error-codes '((definition-not-found-error . -32000)))
       (json-rpc-handler-table
        `(("initialize" . ,initialize-handler)
          ("initialized" . ,initialized-handler)
          ("textDocument/definition" . ,text-document/definition)
          ("textDocument/didChange" . ,text-document/did-change)
          ("textDocument/didClose" . ,text-document/did-close)
          ("textDocument/didOpen" . ,text-document/did-open)
          ("textDocument/didSave" . ,text-document/did-save)
          ("textDocument/completion" . ,text-document/completion)
          ("textDocument/hover" . ,text-document/hover)
          ("completionItem/resolve" . ,completion-item/resolve)
          ("textDocument/signatureHelp" . ,text-document/signature-help)
          ("$/cancelRequest" . ,ignore-request)
          ("exit" . ,lsp-exit-handler)
          ("shutdown" . ,shutdown-handler)
          ;; custom commands
          ("custom/loadFile" . ,custom/load-file))))
    (thunk)))

(define (start-lsp-loop)
  (write-log 'info "LSP loop started")
  (parameterize-and-run
   (lambda () (json-rpc-loop (current-input-port) (current-output-port)))))

(define (start-lsp-server tcp-port-number tcp-error-port-number)
  (parameterize-and-run
   (lambda () (json-rpc-start-server/tcp tcp-port-number
                                         tcp-error-port-number))))

(define (start-lsp-server/background tcp-port-number tcp-error-port-number)
  (define thread
    (make-thread (lambda () (start-lsp-server tcp-port-number tcp-error-port-number))))
  (thread-start! thread)
  thread)

(define (lsp-spawn-server port-number error-port-number)
  (let ((th (thread-start!
             (make-thread
              (lambda ()
                (guard
                 (condition (#t (begin (write-log 'error
                                                  (format "CRASH: ~a" condition))
                                       (raise condition))))
                 (start-lsp-server port-number error-port-number)))))))
    (mutex-lock! listening-threads-mutex)
    (set! listening-threads (cons th listening-threads))
    (mutex-unlock! listening-threads-mutex)
    (when th
      (write-log 'debug
                 (format "LSP server listening on port ~a with log level ~a"
                         port-number
                         (lsp-server-log-level))))))

(define (lsp-server-request-connection command-port-number
                                       lsp-port-number
                                       lsp-error-port-number)
  (write-log 'debug
             (format "Started lsp-server-request-connection on command port ~a,
                      error port ~a and lsp port ~a~%"
                     command-port-number
                     lsp-error-port-number
                     lsp-port-number))

  (let-values (((inp outp) ($tcp-connect "127.0.0.1"
                                         command-port-number)))
    (write-log 'info (format "requesting new LSP connection at port ~a~%"
                              lsp-port-number))
    (write-log 'debug (format "sending command: spawn-lsp-server ~a ~a~%"
                              lsp-port-number
                              lsp-error-port-number))
    (display (format "spawn-lsp-server ~a ~a~%"
                     lsp-port-number
                     lsp-error-port-number)
             outp)
    (flush-output-port outp)
    (write-log 'info (format "LSP connection successfull.~%"))

    (let ((listener ($tcp-listen lsp-error-port-number)))
      (write-log 'info "Listening for incomming error messages\n")
      (let-values (((in-err-port out-err-port)
                    ($tcp-accept listener)))
        (let loop ((msg (read-line in-err-port)))
          (if (eof-object? msg)
              #f
              (begin
                (write-log 'info (format "RECEIVED: ~a\n" msg))
                (newline (current-error-port))
                (flush-output-port (current-error-port))
                (loop (read-line in-err-port)))))))))

(define (dispatch-command cmd)
  (if (string-prefix? "spawn-lsp-server" cmd)
      (let ((cmd-lst (string-tokenize cmd)))
        (cond ((not (= (length cmd-lst) 3))
               ;;(write "missing port in command~%" out-port)
               (write-log 'info (format "missing ports in command: ~a~%" cmd)))
              (else
               (let ((port-num (string->number (list-ref cmd-lst 1)))
                     (error-port-num (string->number (list-ref cmd-lst 2))))
                 (if (and port-num error-port-num)
                     (begin
                       (write-log 'debug (format "Spawning LSP server on port ~a"
                                                 port-num))
                       (lsp-spawn-server port-num error-port-num))
                     (write-log 'info (format "invalid command  ~a~%" cmd)))))))
      (write-log 'info (format "ignoring unknown command: ~s" cmd))))

(define (lsp-command-loop command-port-num)
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
          (let ((cmd (read-line in-port)))
            (dispatch-command cmd)
            (loop)))))))

(define (lsp-command-server-start port-num)
  (write-log 'info
             (format "LSP command server started on port ~a"
                     port-num))
  (thread-start!
   (make-thread (lambda () (lsp-command-loop port-num)))))

(define (start-lsp-server-full lsp-port-num
                               lsp-error-port-num
                               command-port-num
                               repl-port-num)
  (log-level 'debug)

  (when (= lsp-port-num 0)
    (write-log 'info "ignoring port 0")
    (exit 0))
  (guard
   (condition
    (#t (guard
         (condition (#t (write-log 'error
                                   (format "Connection request for LSP connection at port ~a failed" lsp-port-num))))
         (let-values (((inp outp) ($tcp-connect "127.0.0.1"
                                                command-port-num)))
           (write-log 'info
                      (format "requesting new LSP connection at port ~a"
                              lsp-port-num))
           (display (format "spawn-lsp-server ~a ~a" lsp-port-num lsp-error-port-num)
                    outp)
           (flush-output-port outp)
           (write-log 'info
                      (format "LSP connection successfull. Idleing."))
           (read-char)))))
   (begin
     ($spawn-repl-server repl-port-num)
     (write-log 'debug (format "REPL server created on port ~a." repl-port-num))

     (lsp-spawn-server lsp-port-num lsp-error-port-num)

     (write-log 'debug (format "LSP server created on port ~a." lsp-port-num))

     (lsp-command-loop command-port-num))))

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
