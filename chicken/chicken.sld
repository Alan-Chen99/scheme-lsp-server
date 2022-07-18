(define-library (lsp-server chicken)

(export $apropos-list
        $open-file!
        $save-file!
        $fetch-documentation
        $fetch-signature
        $get-definition-locations
        $initialize-lsp-server!
        $server-capabilities
        $server-name
        spawn-repl-server
        $tcp-accept
        $tcp-connect
        $tcp-listen
        $tcp-read-timeout
        get-module-path
        pathname-directory
        pathname-join)

(import (apropos)
        (chicken base)
        (chicken file)
        (chicken foreign)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken platform)
        (chicken port)
        (chicken process)
        (chicken process-context)
        (chicken random)
        (only (chicken string) string-intersperse)
        (chicken tcp)
        nrepl
        chicken-doc
        medea
        r7rs
        scheme
        srfi-1
        srfi-18
        srfi-69
        srfi-130

        (geiser)
        (srfi 18)
        (lsp-server private)
        (lsp-server chicken util)
        (lsp-server parse)
        (lsp-server adapter))

(begin

  ;;; A hash table mapping modules (extensions) to eggs. This is needed
  ;;; to fetch the correct documentation with chicken-doc
  (define module-egg-table (make-hash-table))

  (define eggs-path
    (make-pathname (system-cache-directory) "chicken-install"))

  (define chicken-source-path
    (or (get-environment-variable "CHICKEN_SOURCE_PATH") ""))

  (define root-path
    (make-parameter #f))

  (define $tcp-read-timeout tcp-read-timeout)

  (define $server-name
    "CHICKEN LSP server")

  (define (pick-port)
    (+ (pseudo-random-integer 2000)
       8001))

  ;;; Initialize LSP server to manage project at ROOT (a string). Used
  ;;; for implementation-specific side effects only.
  (define ($initialize-lsp-server! root)
    (root-path (if (and root (not (equal? root 'null)))
                   root
                   "."))
    (verify-repository) ; needed to use chicken-doc in compiled code
    (set! module-egg-table (build-module-egg-table))
    (generate-meta-data! eggs-path)
    (generate-meta-data! chicken-source-path)
    (generate-meta-data! (root-path))

    #t)

  ;;; An alist with implementation-specific server capabilities. See:
  ;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
  ;;; Note: These capabilities are joined to the implementation independent
  ;;; mandatory-capabilities (see server.scm).
  (define $server-capabilities
    `((definitionProvider . ())))

  (define $tcp-accept tcp-accept)

  (define $tcp-connect tcp-connect)

  (define $tcp-listen tcp-listen)

  ;;; Return apropos instances of all functions matching IDENTIFIER (a symbol).
  (define ($apropos-list identifier)
    (lsp-geiser-completions identifier))

  ;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
  ;;; MODULE (a symbol). Return #f if nothing found.
  ;;; Example call: $fetch-documentation '(srfi-1) 'map
  (define ($fetch-documentation identifier)
    (define match (alist-ref identifier (geiser-autodoc identifier)))
    (define module (and match
                        (alist-ref "module" match equal?)))
    (define egg (or (module-egg module) module))
    (write-log 'debug
               (format "$fetch-documentation identifier: ~a, match: ~a, module: ~a, egg: ~a"
                       identifier
                       match
                       module
                       egg))
    (if (and egg (not (null? egg)))
        (let ((doc-path
               (append (if (list? egg)
                           egg
                           (list egg))
                       (list identifier))))
          (write-log 'debug
                     (format "looking up doc-path: ~a" doc-path))
          (with-output-to-string
            (lambda ()
              (guard (condition
                      (#t (write-log
                           'error
                           (format "#fetch-documentation: docmuentation not found: (~a ~a)"
                                   egg
                                   identifier))
                          #f))
               (describe (lookup-node doc-path))))))
        #f))

  ;;; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
  ;;; symbol). Return #f if nothing found.
  ;;; Example call: $fetch-documentation '(srfi 1) 'map
  (define ($fetch-signature identifier)
    (define match (alist-ref identifier (geiser-autodoc (list identifier))))
    (define module (and match (alist-ref "module" match equal?)))
    (define egg (or (module-egg module) module))
    (write-log 'debug
               (format "$fetch-signature egg: ~a identifier: ~a"
                       egg
                       identifier))
    (or (if (or (not egg)
                (null? egg))
            #f
            (guard (condition
                    (#t (write-log 'error
                                   (format "#fetch-signature: signature not found: (~a ~a)"
                                           egg
                                           identifier))
                        #f))
              (node-signature
               (lookup-node (list egg identifier)))))
        (lsp-geiser-signature identifier)))

  (define (load-or-import file-path)
    (guard
     (condition
      (#t (write-log 'error (format "Can't load file ~a: ~a"
                                    file-path
                                    condition))))
     (let ((lib-name (parse-library-name-from-file file-path)))

       (if (not lib-name)
           (lsp-geiser-load-file file-path)
           (eval `(import ,lib-name))))))

  ;;; Action to execute when FILE-PATH is opened. Used for side effects only.
  (define ($open-file! file-path)
    (load-or-import file-path)
    (generate-meta-data! file-path)
    #f)

  ;;; Action to execute when FILE-PATH is saved. Used for side effects only.
  (define ($save-file! file-path)
    (load-or-import file-path)
    (generate-meta-data! file-path)
    #f)

  (define (join-definition-tables! left right)
    (for-each
     (lambda (k)
       (let ((left-locations (hash-table-ref/default left k '()))
             (right-locations (hash-table-ref right k)))
         (if (not (null? left-locations))
             (let ((updated-locations
                    (fold (lambda (right-loc acc)
                            (let* ((right-loc-path (car right-loc))
                                   (acc-loc (assoc right-loc-path
                                                   acc)))
                              (if acc-loc
                                  (cons right-loc
                                        (alist-delete right-loc-path acc))
                                  (cons right-loc acc))))
                          left-locations
                          right-locations)))
               (hash-table-set! left k updated-locations))
             (hash-table-set! left k right-locations))))
     (hash-table-keys right))
    left)

  ;;; Return a list of locations found for IDENTIFIER (a symbol).
  ;;; Each location is represented by an alist
  ;;; '((url . "file:///<path>")
  ;;;   (range . ((start . ((line  . <line number>)
  ;;;                       (character . <character number))
  ;;;             (end . ((line  . <line number>)
  ;;;                     (character . <character number))))
  ;;;
  (define ($get-definition-locations identifier)
    (fetch-definition-locations identifier))

  (define (build-module-egg-table)
    (define-values (in out pid)
      (process (chicken-status) '("-c")))
    (define (egg-line? str)
      (irregex-match
       '(: (submatch (+ any)) (+ space) (+ #\.) (* any))
       str))
    (define (extension-line? str)
      (irregex-match
       '(: (+ space) "extension" (+ space)
           (submatch (+ (~ space))) (* space))
       str))
    (let loop ((line (read-line in))
               (table '())
               (cur-egg #f))
      (cond ((eof-object? line)
             (alist->hash-table table))
            ((egg-line? line) =>
             (lambda (m)
               (loop (read-line in)
                     table
                     (string->symbol (irregex-match-substring m 1)))))
            ((extension-line? line) =>
             (lambda (m)
               (loop (read-line in)
                     (let ((mod (map string->symbol
                                     (string-split (irregex-match-substring m 1)
                                                   "."))))

                       (cons (cons mod cur-egg)
                             table))
                     cur-egg)))
            (else (loop (read-line in)
                        table
                        cur-egg)))))

  (define (module-egg mod)
    (hash-table-ref/default module-egg-table
                            mod
                            #f))

  (define (chicken-status)
    (make-pathname
     (foreign-value "C_TARGET_BIN_HOME" c-string)
     (foreign-value "C_CHICKEN_STATUS_PROGRAM" c-string)))

  (define (spawn-repl-server port-num)
    (nrepl port-num))))
