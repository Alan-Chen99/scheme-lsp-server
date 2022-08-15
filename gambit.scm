(define-library (lsp-server gambit)

(export $apropos-list
        $fetch-signature
        $initialize-lsp-server!
        $open-file!
        $save-file!
        $server-capabilities
        $server-name
        $tcp-accept
        $tcp-connect
        $tcp-listen
        $tcp-read-timeout)

(import (gambit)
        (_geiser)
        (srfi 28)
        (lsp-server adapter)
        (lsp-server private))

(begin

  (define $server-name
    "Gambit LSP server")

  ;;; Ignored for now
  (define $tcp-read-timeout (make-parameter #f))

  (define ($initialize-lsp-server! root-path)
    (write-log 'info (format "initializing LSP server with root ~a"
                             root-path))
    #f)

  (define $server-capabilities
    '())

  (define ($tcp-listen port-number)
    (open-tcp-server port-number))

  (define ($tcp-accept listener)
    (let ((p (read listener)))
      (values p p)))

  (define ($tcp-connect tcp-address tcp-port-number)
    (let ((p (open-tcp-client tcp-port-number)))
      (values p p)))

  (define ($get-definition-locations mod-name identifier)
    '())

  (define ($open-file! file-path)
    #f)

  (define ($save-file! file-path)
    #f)

  (define ($apropos-list module prefix)
    (write-log 'debug (format "$apropos-list ~a ~a" module prefix))
    '())

  (define ($fetch-signature mod-name identifier)
    (write-log 'debug
               (format "$fetch-signature ~s ~s"
                       mod-name
                       identifier))

    (lsp-geiser-signature identifier))))
