
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

(define $tcp-close tcp-close)

(define $tcp-connect tcp-connect)

(define $tcp-listen tcp-listen)

;;; Return completion suggestions for PREFIX (a symbol).
;;; MODULE is ignored for now, but belongs to the API, since other
;;; implementations (e.g. guile) use it.
;;; A suggestion is a pair of strings (identifier . library)
(define ($apropos-list module prefix)
  (define suggestions
    (apropos-information-list prefix #:macros? #t #:imported? #f))
  (fold (lambda (s acc)
          (let* ((mod-id-pair (car s))
                 (mod (let ((fst (car mod-id-pair)))
                        (if (eqv? fst '||)
                            #f
                            (map string->symbol
                                 (string-split (symbol->string fst) ".")))))
                 (id (cdr mod-id-pair))
                 (type (cdr s)))
            (if (string-prefix? prefix (symbol->string id))
                (cons (cons (symbol->string id)
                            (module-name->chicken-string mod))
                      acc)
                acc)))
        '()
        suggestions))

;;; Return the documentation (a string) found for IDENTIFIER (a symbol) in
;;; MODULE (a symbol). Return #f if nothing found.
;;; Example call: $fetch-documentation '(srfi-1) 'map
(define ($fetch-documentation mod-name identifier)
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
                    (#t (write-log 'debug
                         (format "#fetch-documentation: documentation not found: (~a ~a)"
                                 egg
                                 identifier))
                        #f))
             (describe (lookup-node doc-path))))))
      #f))


(define (find-identifier-module identifier)
  (let* ((id-str (symbol->string identifier))
         (suggestions ($apropos-list #f id-str)))
    (alist-ref id-str suggestions equal?)))

; Return the signature (a string) for IDENTIFIER (a symbol) in MODULE (a
; symbol). Return #f if nothing found.
; Example call: $fetch-documentation '(srfi 1) 'map
(define ($fetch-signature module identifier)
  (define egg (find-identifier-module identifier))
  (write-log 'debug
             (format "$fetch-signature egg: ~a identifier: ~a"
                     egg
                     identifier))
  (or (if (or (not egg)
              (null? egg))
          #f
          (guard (condition
                  (#t (write-log 'debug
                       (format "#fetch-signature: signature not found: (~a ~a)"
                               egg
                               identifier))
                      #f))
                 (node-signature
                  (lookup-node (list egg identifier)))))
      ;;(lsp-geiser-signature identifier)
      (fetch-signature module identifier)))

(define (load-or-import file-path)
  (guard
   (condition
    (#t (write-log 'error
                   (format "Can't load file ~a: ~a"
                           file-path
                           (with-output-to-string
                             (lambda ()
                               (print-error-message condition)))))))
   (let ((mod-name (parse-library-name-from-file file-path)))

     (if (not mod-name)
         (lsp-geiser-load-file file-path)
         (begin (lsp-geiser-load-file file-path)
                (eval `(import ,mod-name)))))))

;;; Action to execute when FILE-PATH is opened. Used for side effects only.
(define ($open-file! file-path)
  (generate-meta-data! file-path)
  #f)

;;; Action to execute when FILE-PATH is saved. Used for side effects only.
(define ($save-file! file-path)
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
(define ($get-definition-locations mod-name identifier)
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

(define (module-name->chicken-string mod-name)
  (cond ((not mod-name)
         #f)
        ((list? mod-name)
         (let ((lib-parts (map symbol->string mod-name)))
           (string-join lib-parts ".")))
        (else (symbol->string mod-name))))

(define (module-egg mod)
  (hash-table-ref/default module-egg-table
                          mod
                          #f))

(define (chicken-status)
  (make-pathname
   (foreign-value "C_TARGET_BIN_HOME" c-string)
   (foreign-value "C_CHICKEN_STATUS_PROGRAM" c-string)))

(define (spawn-repl-server port-num)
  (nrepl port-num))

(define (get-module-path module-name)
    #f)
(define pathname-join make-pathname)
(define (pathname-base p)
  (format "~a.~a"
          (pathname-file p)
          (pathname-extension p)))

(define (get-absolute-pathname path)
  (if (absolute-pathname? path)
      path
      (pathname-join (current-directory) path)))

(define (alist-ref/default key alist default)
  (or (alist-ref key alist)
      default))

(define uri-decode uri-decode-string)
