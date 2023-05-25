(define-record-type <diagnostic>
  (make-diagnostic source file-path line-number char-number message)
  diagnostic?
  (source diagnostic-source diagnostic-source-set!)
  (file-path diagnostic-file-path)
  (line-number diagnostic-line-number)
  (char-number diagnostic-char-number)
  (message diagnostic-message))

(define (clear-diagnostics file-path)
  (json-rpc-send-notification
   (server-out-port)
   "textDocument/publishDiagnostics"
   `((uri . ,file-path)
     (diagnostics . #()))))

(define (alist->diagnostic alist)
  (make-diagnostic #f
                   (alist-ref 'filename alist)
                   (alist-ref 'line-number alist)
                   (alist-ref 'char-number alist)
                   (alist-ref 'message alist)))

(define (send-diagnostics file-path diags)
  (let ((diags-as-lists
         (map (lambda (diag)
                (let* ((file-path (diagnostic-file-path diag))
                       (line-num (diagnostic-line-number diag))
                       (char-num (diagnostic-char-number diag))
                       (msg (diagnostic-message diag))
                       (doc (read-text! file-path))
                       (word (get-word-at-position doc
                                                   line-num
                                                   char-num
                                                   (lambda (c)
                                                     (or (identifier-char? c)
                                                         (char=? c #\()
                                                         (char=? c #\{)
                                                         (char=? c #\[)))
                                                   (lambda (c)
                                                     (or (identifier-char? c)
                                                         (char=? c #\space)
                                                         (char=? c #\))
                                                         (char=? c #\})
                                                         (char=? c #\]))))))
                  (let* ((end (or (and word
                                       (editor-word-end-char word))
                                  79))
                         (start (if (> (- end char-num) 1)
                                    char-num
                                    0)))
                    `((message . ,msg)
                      (source . ,(or (diagnostic-source diag)
                                     "compiler"))
                      (range . ((start . ((line . ,line-num)
                                          (character . ,start)))
                                (end . ((line . ,line-num)
                                        (character . ,end)))))))))
              diags)))
    (when (not (null? diags-as-lists))
      (json-rpc-send-notification
       (server-out-port)
       "textDocument/publishDiagnostics"
       `((uri . ,file-path)
         (diagnostics . ,(list->vector diags-as-lists))))))
  #f)
