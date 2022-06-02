(define-library (lsp-server chicken)

(export $apropos-list
        $open-file
        $save-file
        $fetch-documentation
        $fetch-signature
        $get-definition-locations
        $initialize-lsp-server
        $server-capabilities
        $server-name
        $tcp-accept
        $tcp-connect
        $tcp-listen
        $tcp-read-timeout
        $spawn-repl-server)

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
        chicken-doc
        medea
        r7rs
        scheme
        srfi-1
        srfi-69
        srfi-130

        nrepl
        (srfi 18)
        (lsp-server private)
        (lsp-server tags))

(begin

;;; A hash table mapping modules (extensions) to eggs. This is needed
;;; to fetch the correct documentation with chicken-doc
  (define module-egg-mapping #f)

  ;;; Maps identifiers (string) to an alist that maps the path of
  ;;; a source file (string) to a <tag-info> record.
  (define tags-table
    (make-parameter (make-hash-table)))

  (define eggs-path
    (make-pathname (system-cache-directory) "chicken-install"))

  (define chicken-source-path
    (or (get-environment-variable "CHICKEN_SRC") ""))

  (define tags-path
    (make-parameter #f))

  (define root-path
    (make-parameter #f))

  (define $tcp-read-timeout tcp-read-timeout)

  (define $server-name
    "chicken lsp server")

  (define (initialize-tags-path)
    (when (not (tags-path))
      (tags-path (if (eq? (root-path) 'null)
                     (create-temporary-file)
                     (make-pathname (root-path) "CHICKEN-TAGS")))))

  (define (pick-port)
    (+ (pseudo-random-integer 2000)
       8001))

  (define (spawn-nrepl-on-random-port)
    (define port (pick-port))
    (write-log 'info (format "open nrepl on port ~a" port))
    (nrepl port))

  (define ($initialize-lsp-server root)
    (root-path (if (and root (not (equal? root 'null)))
                   root
                   "."))
    ;;(initialize-tags-path)
    (set! module-egg-mapping (build-module-egg-mapping))
    ;;(generate-tags (tags-path) #t eggs-path chicken-source-path (root-path))
    (generate-tags! eggs-path)
    (generate-tags! chicken-source-path)
    (generate-tags! (root-path))

    ;;(tags-table (parse-tags-file (tags-path)))
    #t)

  (define $server-capabilities
    `((definitionProvider . ())))

  (define $tcp-accept tcp-accept)

  (define $tcp-connect tcp-connect)

  (define $tcp-listen tcp-listen)

  (define ($spawn-repl-server port-num)
    (thread-start!
     (make-thread
      (lambda () (nrepl port-num)))))


  (define ($apropos-list identifier)
    (define suggestions
      (apropos-information-list identifier #:macros? #t #:imported? #t))
    (fold (lambda (s acc)
            (let* ((mod-id-pair (car s))
                   (mod (let ((fst (car mod-id-pair)))
                          (if (eqv? fst '||)
                              #f
                              (map string->symbol
                                   (string-split (symbol->string fst) ".")))))
                   (id (cdr mod-id-pair))
                   (type (cdr s)))
              (if (string-prefix? identifier (symbol->string id))
                  (cons (make-apropos-info mod id type #f)
                        acc)
                  acc)))
          '()
          suggestions))


  (define ($fetch-documentation module identifier)
    (define egg (or (module-egg module)
                    module))
    (define doc-path
      (append (if (list? egg)
                  egg
                  (list egg))
              (list identifier)))
    (begin
      (write-log 'debug
                 (format "looking up doc-path: ~a" doc-path))
      (with-output-to-string
        (lambda ()
          (describe (lookup-node doc-path))))))

  (define ($fetch-signature module identifier)
    (define egg (or (module-egg module)
                    (car module)))
    (if (not egg)
        #f
        (node-signature
         (lookup-node (list egg identifier)))))

  (define ($open-file file-path)
    (generate-tags! file-path))

  (define ($save-file file-path)
    (generate-tags! file-path))

  (define (read-definitions src-path)
    (define regex
      (irregex '(: (* whitespace)
                   (submatch (+ (~ whitespace)))
                   (* whitespace)
                   #\delete
                   (? (: (~ numeric) (* any) #\x1))
                   (submatch (+ numeric))
                   #\,
                   (submatch (+ numeric)))))
    (let loop ((line (read-line))
               (res (make-hash-table)))
      (if (or (eof-object? line)
              (string-prefix? "\f" line))
          res
          (let ((submatches (irregex-match regex line)))
            (if (and submatches
                     (>= (irregex-match-num-submatches submatches)
                         3))
                (let ((identifier (irregex-match-substring submatches 1))
                      (line-number
                       (- (string->number
                           (irregex-match-substring submatches 2))
                          1))
                      (char-number
                       (string->number (irregex-match-substring submatches 3))))
                  (loop (read-line)
                        (begin (hash-table-set! res
                                                identifier
                                                `((,src-path . ,(make-tag-info src-path line-number char-number))))
                               res)))
                (begin (write-log 'debug
                                  (format  "skipping ill-formed TAGS line: ~a"
                                           line))
                       (loop (read-line) res)))))))

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

  (define ($get-definition-locations identifier)
    (get-definition-locations identifier))

  (define (build-module-egg-mapping)
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
    (hash-table-ref/default module-egg-mapping
                            mod
                            #f))

  (define (chicken-status)
    (make-pathname
     (foreign-value "C_TARGET_BIN_HOME" c-string)
     (foreign-value "C_CHICKEN_STATUS_PROGRAM" c-string))))
)
