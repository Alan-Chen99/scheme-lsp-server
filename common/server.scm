(define lsp-server-log-level (make-parameter 2))

;; (define (run)
;;   (json-rpc-loop (current-input-port) (current-output-port)))

(define (ignore-request params)
  (write-log 'debug (format "ignoring request. Params: ~a" params))
  #f)

(define (text-document/did-change params)
  (define file-path (get-uri-path params))
  (define changes
    (string-split
     (alist-ref 'text
                (vector-ref (alist-ref 'contentChanges params) 0))
     "\r\n"
     #t))
  (if file-path
      (begin (update-file! file-path changes)
             (write-log 'debug
                        (format "file contents updated: ~a"
                                file-path)))
      (write-log 'debug
                 (format "file-path not found: ~a"
                         file-path)))
  #f)

(define (text-document/did-open params)
  (define file-path (get-uri-path params))
  (if file-path
      (begin (read-file! file-path)
             (write-log 'debug
                        (format "file contents read: ~a"
                                file-path)))
      (write-log 'debug
                        (format "file-path not found: ~a"
                                file-path)))
  #f)

(define (text-document/did-save params)
  (define file-path (get-uri-path params))
  (write-log 'info "file saved.")
  #f)

(define (text-document/completion params)
  (define word (get-word-under-cursor params))
  (write-log 'debug "getting completion suggestions for word " word)
  (if (or (not word)
          (< (string-length word) 3))
      #f
      (let ((suggestions ($apropos-list word)))
        (write-log 'debug (format "suggestions found: ~a~%"
                                  suggestions))
        `((items .
                 ,(list->vector
                   (map (lambda (ainfo)
                          (let* ((module-name 
                                  (join-module-name
                                   (apropos-info-module ainfo)))
                                 (id-name
                                  (symbol->string
                                   (apropos-info-name ainfo)))
                                 (label (if module-name
                                            (format "~a ~a"
                                                    module-name
                                                    id-name)
                                            (format "~a" id-name))))
                            `((label . ,label)
                              (insertText . ,id-name)
                              (data . ((identifier . ,id-name)
                                       (module . ,module-name))))))
                        suggestions)))))))

(define (completion-item/resolve params)
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
                (if (> (lsp-server-log-level) 2)
                    (raise (make-json-rpc-internal-error
                            (format "Error resolving ~a ~a"
                                    mod
                                    id)))
                    #f))))
    (let ((doc ($fetch-documentation mod id)))
      (cons `(documentation . ,doc)
            params))))

(define (text-document/signature-help params)
  (define cur-word (get-word-under-cursor params))
  (define matches
    (filter (lambda (ap)
              (string=? (symbol->string (apropos-info-name ap))
                        cur-word))
            ($apropos-list cur-word)))
  (if (null? matches)
      (begin
        (write-log 'info
                   (format "no signature found for: ~a" cur-word))
        #f)
      (guard
          (condition
           (#t (if (> (lsp-server-log-level) 2)
                   (raise (make-json-rpc-internal-error
                           (format "Error fetching signature of `~a`"
                                   cur-word)))
                   #f)))
        (let* ((ainfo (car matches))
               (signature
                ($fetch-signature (apropos-info-module ainfo)
                                  (apropos-info-name ainfo))))
          `((signatures . ,(vector `((label . ,signature)))))))))

(define (custom/load-file params)
  (define file-path (get-uri-path params))
  (write-log 'debug "loading file: " file-path)
  (guard (condition
          (#t (write-log 'warning "error loading file")))
    (load file-path))
  #f)

(define (start-lsp-server tcp-port)
  (parameterize
      ((json-rpc-log-level (lsp-server-log-level))
       (log-level (lsp-server-log-level))
       (json-rpc-handler-table
        `(("initialize" .
           ,(lambda (params)
              ($initialize-lsp-server)
              `((capabilities . ,$server-capabilities)
                (serverInfo . ((name . ,$server-name)
                               (version . "0.1.0"))))))
          ("shutdown" . ,(lambda (params)
                           (write-log 'info
                                      "shutting down")
                           (json-rpc-exit)))
          ("initialized" . ,(lambda (params)
                              (write-log 'info
                                         "initialized")
                              #f))
          ("textDocument/didChange" . ,text-document/did-change)
          ("textDocument/didOpen" . ,text-document/did-open)
          ("textDocument/didSave" . ,text-document/did-save)
          ("textDocument/completion" . ,text-document/completion)
          ("completionItem/resolve" . ,completion-item/resolve)
          ("textDocument/signatureHelp" . ,text-document/signature-help)
          ("$/cancelRequest" . ,ignore-request)
          ("exit" . ,(lambda (params)
                       (json-rpc-exit)))

          ;; custom commands
          ("custom/loadFile" . ,custom/load-file))))
    (json-rpc-start-server/tcp tcp-port)))

(define (start-lsp-server/background tcp-port)
  (define thread
    (make-thread (lambda () (start-lsp-server tcp-port))))
  (thread-start! thread)
  thread)

;; (cond-expand ((or chicken-script compiling)
;;               (let ((thread (start-lsp-server)))
;;                (thread-join! thread)))
;;              (else))

