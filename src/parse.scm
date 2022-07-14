(define-record-type <procedure-info>
  (make-procedure-info name arguments module line character docstring)
  procedure-info?
  (name procedure-info-name)
  (arguments procedure-info-arguments)
  (module procedure-info-module)
  (line procedure-info-line set-procedure-info-line!)
  (character procedure-info-character set-procedure-info-character!)
  (docstring procedure-info-docstring))

(define-record-type <source-meta-data>
  (make-source-meta-data library-name procedure-info-table imports)
  source-meta-data?
  (library-name source-meta-data-library-name set-source-meta-data-library-name!)
  (procedure-info-table source-meta-data-procedure-info-table)
  (imports source-meta-data-imports))

(define-record-type <parse-context>
  (make-parse-context directory library-name)
  parse-context?
  (directory parse-context-directory)
  (library-name parse-context-library-name))

;;;; Parameters

(define identifier-to-source-meta-data-table
  (make-parameter (make-hash-table)))

(define source-path-timestamps
  (make-parameter (make-hash-table)))

(define all-identifiers (make-parameter (make-trie)))

;;;; Predicates

(define (tagged-expression? expr procedure)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) procedure)))

(define (r6rs-library-definition-form? expr)
  (tagged-expression? expr 'library))

(define (r7rs-library-definition-form? expr)
  (tagged-expression? expr 'define-library))

(define (chicken-library-definition-form? expr)
  (tagged-expression? expr 'module))

(define (guile-library-definition-form? expr)
  (tagged-expression? expr 'define-module))

(define (library-definition-form? expr)
  (cond-expand
   (chicken (or (r7rs-library-definition-form? expr)
                (chicken-library-definition-form? expr)))
   (guile (or (r6rs-library-definition-form? expr)
              (r7rs-library-definition-form? expr)
              (guile-library-definition-form? expr)))))

(define (import-form? expr)
  (or (tagged-expression? expr 'import)
      (tagged-expression? expr 'use-modules)))

(define (include-form? expr)
  (tagged-expression? expr 'include))

(define (load-form? expr)
  (tagged-expression? expr 'load))

(define (begin-form? expr)
  (tagged-expression? expr 'begin))

(define (lambda-form? expr)
  (tagged-expression? expr 'lambda))

(define (case-lambda-form? expr)
  (tagged-expression? expr 'case-lambda))

(define (procedure-definition-with-case-lambda? expr)
  (and (symbol? (cadr expr))
       (not (null? (cddr expr)))
       (case-lambda-form? (caddr expr))))

(define (procedure-definition-with-lambda? expr)
  (and (symbol? (cadr expr))
       (not (null? (cddr expr)))
       (lambda-form? (caddr expr))))

(define (procedure-definition-with-parenthesis? expr)
  (and (pair? (cadr expr))
       (not (null? (cddr expr)))))

;; TODO support set!
(define (procedure-definition-form? expr)
  (and (or (tagged-expression? expr 'define)
           (tagged-expression? expr 'define*))
       (not (null? (cdr expr)))
       (or (procedure-definition-with-case-lambda? expr)
           (procedure-definition-with-lambda? expr)
           (procedure-definition-with-parenthesis? expr))))

(define (cond-expand-form? expr)
  (tagged-expression? expr 'cond-expand))

(define (cond-expand-clause-satisfied? clause)
  (if (or (not (list? clause))
          (null? clause))
      #f
      (let ((predicate (car clause)))
        (cond ((and (symbol? predicate)
                    (eq? predicate 'else))
               #t)
              ((symbol? predicate)
               (memq predicate (features)))
              ((and (list? predicate)
                    (not (null? predicate)))
               (case (car predicate)
                 ((and) (every (compose cond-expand-clause-satisfied? list) (cdr predicate)))
                 ((or) (any (compose cond-expand-clause-satisfied? list) (cdr predicate)))
                 ((library) (and (not (null? (cdr predicate)))
                                 (library-available? (cadr predicate))))
                 ((not) (not (cond-expand-clause-satisfied? (cdr predicate))))
                 ((else) #t)
                 (else (memq (car predicate) (features)))))
              (else (error "unknown predicate " predicate))))))

;;;; Syntax accessors

(define (case-lambda-arguments expr)
  (reverse (fold (lambda (clause acc)
                   (if (and (pair? clause)
                            (not (null? clause)))
                       (cons (car clause)
                             acc)
                       acc))
                 '()
                 (cdr expr))))

(define (case-lambda-docstring expr)
  (define body (cdr expr))
  (cond ((null? body) #f)
        ((string? (car body)) (car body))
        (else #f)))

(define (lambda-arguments expr)
  (cadr expr))

(define (lambda-docstring expr)
  (define body (cddr expr))
  (cond ((null? body) #f)
        ((string? (car body)) (car body))
        (else #f)))

(define (procedure-definition-name expr)
  (cond ((procedure-definition-with-parenthesis? expr)
         (caadr expr))
        ((procedure-definition-with-lambda? expr)
         (cadr expr))))

(define (procedure-definition-arguments expr)
  (cond ((procedure-definition-with-parenthesis? expr)
         (cdr (cadr expr)))
        ((procedure-definition-with-case-lambda? expr)
         (case-lambda-arguments (caddr expr)))
        ((procedure-definition-with-lambda? expr)
         (lambda-arguments (caddr expr)))))

(define (procedure-definition-docstring expr)
  (cond ((procedure-definition-with-parenthesis? expr)
         (let ((body (cddr expr)))
           (cond ((null? body) #f)
                 ((string? (car body)) (car body))
                 (else #f))))
        ((procedure-definition-with-lambda? expr)
         (lambda-docstring (caddr expr)))
        ((procedure-definition-with-case-lambda? expr)
         (case-lambda-docstring (caddr expr)))))

;;;; Main procedures

(define (parse-guile-module expression)
  (define lib-name (cadr expression))
  (let loop ((expr (cddr expression))
             (previous-keyword #f)
             (imports '()))
    (cond ((null? expr)
           (make-source-meta-data lib-name
                                  (make-hash-table)
                                  (reverse imports)))
          ((keyword? (car expr))
           (loop (cdr expr)
                 (car expr)
                 imports))
          ((eq? previous-keyword #:use-module)
           (loop (cdr expr)
                 #f
                 (if (and (not (null? expr))
                          (list? (car expr))
                          (not (null? (car expr)))
                          (list? (caar expr)))
                     ;; handle clauses of the form (ignore #:select for now):
                     ;; #:use-module ((scheme file) #:select (with-input-from-file))
                     (cons (caar expr) imports)
                     (cons (car expr) imports))))
          (else
           (loop (cdr expr)
                 #f
                 imports)))))

(define (parse-r7rs-import-set expr)
  (cond ((symbol? expr) expr)
        ((null? expr) '())
        ((member (car expr)
                 '(only except prefix rename))
         (parse-r7rs-import-set (cadr expr)))
        (else expr)))

(define (parse-expression expr context)
  (cond ((not (list? expr))
         #f)
        ((null? expr)
         #f)
        ((guile-library-definition-form? expr)
         (parse-guile-module expr))
        ((library-definition-form? expr)
         (let* ((lib-name (cadr expr))
                (subforms-meta-data
                 (fold (lambda (e acc)
                         (let ((sub-meta-data
                                (parse-expression e
                                                  (make-parse-context
                                                   (parse-context-directory
                                                    context)
                                                   lib-name))))
                           (if sub-meta-data
                               (cons sub-meta-data acc)
                               acc)))
                       '()
                       (cddr expr))))
           (make-source-meta-data
            lib-name
            (fold (lambda (sub-ht acc)
                    (hash-table-join! acc sub-ht))
                  (make-hash-table)
                  (map source-meta-data-procedure-info-table
                       subforms-meta-data))
            (flatmap source-meta-data-imports
                     subforms-meta-data))))
        ((begin-form? expr)
         (let ((subforms-meta-data
                (fold (lambda (e acc)
                        (let ((sub-meta-data
                               (parse-expression e context)))
                          (if sub-meta-data
                              (cons sub-meta-data acc)
                              acc)))
                      '()
                      (cdr expr))))
           (make-source-meta-data
            (parse-context-library-name context)
            (fold (lambda (sub-ht acc)
                    (hash-table-join! acc sub-ht))
                  (make-hash-table)
                  (map source-meta-data-procedure-info-table
                       subforms-meta-data))
            (flatmap source-meta-data-imports
                     subforms-meta-data))))
        ((cond-expand-form? expr)
         (let* ((matching-clause (cond-expand-find-satisfied-clause expr))
                (subform-meta-data
                 (parse-expression matching-clause context)))
           (if subform-meta-data
               (make-source-meta-data
                (parse-context-library-name context)
                (source-meta-data-procedure-info-table subform-meta-data)
                (source-meta-data-imports subform-meta-data))
               #f)))
        ((import-form? expr)
         (make-source-meta-data
          (parse-context-library-name context)
          (make-hash-table)
          (map parse-r7rs-import-set (cdr expr))))
        ((procedure-definition-form? expr)
         (make-source-meta-data
          (parse-context-library-name context)
          (let ((proc-name (procedure-definition-name expr)))
            (alist->hash-table
             `((,proc-name .
                           ,(make-procedure-info
                             proc-name
                             (procedure-definition-arguments expr)
                             (parse-context-library-name context)
                             #f
                             #f
                             (procedure-definition-docstring expr))))))
          '()))
        ((or (include-form? expr)
             (load-form? expr))
         (when (and (not (null? (cdr expr)))
                    (string? (cadr expr)))
           (generate-meta-data!
            (pathname-join (parse-context-directory context)
                           (cadr expr))))
         #f)
        (else #f)))

(define (cond-expand-find-satisfied-clause expr)
  (cons 'begin (cdr (find cond-expand-clause-satisfied?
                          (cdr expr)))))

(define (merge-meta-data lst)
  (define lib-name #f)
  (define merged
    (fold (lambda (m acc)
            (let ((lib-name-found (source-meta-data-library-name m)))
              (when lib-name-found
                (set! lib-name lib-name-found)))
            (make-source-meta-data #f
                                   (hash-table-join!
                                    (source-meta-data-procedure-info-table m)
                                    (source-meta-data-procedure-info-table acc))
                                   (append (source-meta-data-imports m)
                                           (source-meta-data-imports acc))))
          (make-source-meta-data #f (make-hash-table) '())
          lst))
  (set-source-meta-data-library-name! merged lib-name)
  merged)

(define (print-meta-data meta-data)
  (write-log 'debug (format "imports: ~s" (source-meta-data-imports meta-data)))
  (write-log 'debug "procedure-info-table: ")
  (let ((pinfos (source-meta-data-procedure-info-table meta-data)))
    (hash-table-walk pinfos
                     (lambda (pname pinfo)
                       (write-log 'debug (format "\t~s" pname))))))

(define (print-source-meta-data-table)
  (hash-table-walk
   (identifier-to-source-meta-data-table)
   (lambda (identifier source-meta-data-table)
     (write-log 'debug
                (format "~s: " identifier))
     (hash-table-walk
      source-meta-data-table
      (lambda (source-path identifier-pinfo-table)
        (write-log 'debug
                   (format "\t~s: " source-path)))))))

(define definition-regex
  (irregex '(: (* whitespace)
               (* #\()
               (: (or "define" "define*" "define-syntax" "set!")
                  (+ whitespace)
                  (? #\()
                  (submatch (+ (~ (or whitespace
                                      #\))))))
               (* any))))

(define (parse-definition-line line)
  (let ((submatches (irregex-match definition-regex line)))
    (if submatches
        (cons (irregex-match-substring submatches 1)
              (irregex-match-start-index submatches 1))
        #f)))

(define (collect-procedure-locations procedure-info-table filename)
  (define symbol-location-map
    (with-input-from-file filename
      (lambda ()
        (let loop ((line (read-line))
                   (line-number 1)
                   (results '()))
          (cond ((eof-object? line)
                 (alist->hash-table results))
                (else
                 (let ((parse-result (parse-definition-line line)))
                   (if parse-result
                       (let ((parsed-symbol (car parse-result))
                             (line-offset (cdr parse-result)))
                         (loop (read-line)
                               (+ line-number 1)
                               (cons (list (string->symbol parsed-symbol)
                                           (- line-number 1)
                                           line-offset)
                                     results)))
                       (loop (read-line)
                             (+ line-number 1)
                             results)))))))))
  (hash-table-fold procedure-info-table
                   (lambda (identifier pinfo acc)
                     (let* ((pname (procedure-info-name pinfo))
                            (loc (hash-table-ref/default symbol-location-map
                                                         pname
                                                         #f)))
                       (hash-table-set! acc
                                        identifier
                                        (if loc
                                            (make-procedure-info
                                             pname
                                             (procedure-info-arguments pinfo)
                                             (procedure-info-module pinfo)
                                             (list-ref loc 0)
                                             (list-ref loc 1)
                                             (procedure-info-docstring pinfo))
                                            pinfo))
                       acc))
                   (make-hash-table)))

(define (parse-library-name-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((expr (read)))
        (cond ((eof-object? expr) #f)
              ((library-definition-form? expr)
               (cadr expr))
              (else
               (loop (read))))))))

(define (parse-file filename)
  (define meta-data-without-location
    (with-input-from-file filename
      (lambda ()
        (let loop ((expr (read))
                   (meta-data '()))
          (if (eof-object? expr)
              (merge-meta-data meta-data)
              (loop (read)
                    (let ((sub-meta-data (parse-expression expr (make-parse-context
                                                                 (pathname-directory filename)
                                                                 #f))))
                      (if sub-meta-data
                          (cons sub-meta-data meta-data)
                          meta-data))))))))
  (make-source-meta-data
   (source-meta-data-library-name meta-data-without-location)
   (collect-procedure-locations
    (source-meta-data-procedure-info-table meta-data-without-location)
    filename)
   (source-meta-data-imports meta-data-without-location)))

(define (update-identifier-to-source-meta-data-table! source-path meta-data)
  (hash-table-walk
   (source-meta-data-procedure-info-table meta-data)
   (lambda (identifier pinfo)
     (hash-table-update!/default (identifier-to-source-meta-data-table)
                                 identifier
                                 (lambda (v)
                                   (begin (hash-table-set! v
                                                           source-path
                                                           pinfo)
                                          v))
                                 (alist->hash-table
                                  `((,source-path . ,pinfo))))
     (trie-insert! (all-identifiers)
                   (stringify identifier)
                   (procedure-info-module pinfo)))))

(define (parse-and-update-table! source-path)
  (define abs-source-path (get-absolute-pathname source-path))
  (write-log 'debug
             (format "parse-and-update-table!: ~s~%" abs-source-path))
  (guard (condition
          (#t (write-log 'error
                         (format "parse-and-update-table!: error parsing file ~a"
                                 abs-source-path))
              #f))
    (let ((meta-data (parse-file abs-source-path)))
      (update-identifier-to-source-meta-data-table! abs-source-path meta-data)
      ;; (for-each (lambda (path)
      ;;             (let ((module-path (get-module-path path)))
      ;;               (when module-path
      ;;                 (generate-meta-data! module-path))))
      ;;           (source-meta-data-imports meta-data))
      )))

(define scheme-file-regex
  (irregex '(: (* any)
               (or ".scm"
                   ".sld"
                   ".ss")
               eol)))

(cond-expand
 (guile (define (generate-meta-data! . files)
          (write-log 'debug
                     (format "generate-meta-data! for files ~a" files))
          (for-each
           (lambda (f)
             (ftw f
                  (lambda (filename statinfo flag)
                    (let ((abs-filename (get-absolute-pathname filename)))
                     (when (and (eq? flag 'regular)
                                (irregex-search scheme-file-regex
                                                abs-filename))
                       (let ((old-time-stamp (hash-table-ref/default
                                              (source-path-timestamps)
                                              abs-filename
                                              #f)))
                         (when (or (not old-time-stamp)
                                   (< old-time-stamp
                                      (stat:mtime statinfo)))
                           (begin
                             (hash-table-set! (source-path-timestamps)
                                              abs-filename
                                              (stat:mtime statinfo))
                             (parse-and-update-table! abs-filename))))))
                    #t)))
           (filter (lambda (f)
                     (not (string=? f "")))
                   files))))
 (chicken
  (define (generate-meta-data! . files)
    (write-log 'debug
               (format "generate-meta-data! for files ~a" files))
    (for-each
     (lambda (f)
       (guard
        (condition
         (#t (write-log 'warning
                        (format "generate-tags: can't read file ~a"
                                f))))
        (if (directory? f)
            (let ((files (find-files f
                                     #:test scheme-file-regex)))
              (for-each
               (lambda (filename)
                 (let* ((abs-filename (get-absolute-pathname filename))
                        (stats (file-stat abs-filename))
                        (mtime (vector-ref stats 8))
                        (old-time-stamp (hash-table-ref/default
                                         (source-path-timestamps)
                                         abs-filename
                                         #f)))
                   (when (or (not old-time-stamp)
                             (> mtime old-time-stamp))
                     (begin
                       (hash-table-set! (source-path-timestamps)
                                        abs-filename
                                        mtime)
                       (parse-and-update-table! abs-filename)))))
               files))
            (parse-and-update-table! f))))
     (filter (lambda (f)
               (not (string=? f "")))
             files)))))

;;; Return a list of locations found for IDENTIFIER (a symbol).
;;; Each location is represented by an alist
;;; '((url . "file:///<path>")
;;;   (range . ((start . ((line  . <line number>)
;;;                       (character . <character number))
;;;             (end . ((line  . <line number>)
;;;                     (character . <character number))))
;;;
(define (fetch-definition-locations module identifier)
  (write-log 'debug
             (format "fetch-definition-locations: ~a (~a)"
                     identifier
                     (if (symbol? identifier)
                         'symbol
                         'string)))
  (define locations
    (hash-table->alist
     (hash-table-ref/default (identifier-to-source-meta-data-table)
                             (string->symbol identifier)
                             (make-hash-table))))
  (if (not (null? locations))
      (begin
        (write-log 'debug
                   (format "locations for identifier ~a found: ~a"
                           identifier
                           locations))
        (map (lambda (loc)
               (let* ((path (car loc))
                      (pinfo (cdr loc))
                      (line-number (procedure-info-line pinfo))
                      (char-number (procedure-info-character pinfo)))
                 (write-log 'debug (format "identifier ~a found: path ~a, line ~a, char ~a "
                                           identifier
                                           path
                                           line-number
                                           char-number))
                 `((uri . ,(string-append "file://" path))
                   (range . ((start . ((line . ,line-number)
                                       (character . ,char-number)))
                             (end . ((line . ,line-number)
                                     (character . ,char-number))))))))
             locations))
      '()))

(define (pinfo-signature pinfo)
  (define name (procedure-info-name pinfo))
  (define args (procedure-info-arguments pinfo))

  (format "~a" (cons name args)))

(define (fetch-pinfo identifier)
  (define id
    (if (symbol? identifier)
        identifier
        (string->symbol identifier)))
  (define source-meta-data-table
    (hash-table-ref/default
     (identifier-to-source-meta-data-table)
     id
     #f))
  (define pinfos
    (if source-meta-data-table
        (hash-table-values source-meta-data-table)
        '()))
  (if (not (null? pinfos))
      (car pinfos)
      #f))

(define (file-already-parsed? file-path)
  (hash-table-exists? (source-path-timestamps) file-path))

(define (fetch-signature module identifier)
  (define pinfo (fetch-pinfo
                 (if (symbol? identifier)
                     identifier
                     (string->symbol identifier))))
  (if pinfo
      (pinfo-signature pinfo)
      #f))

;;; ignore module for now, but keep the API as before
(define (fetch-documentation module identifier)
  (define pinfo (fetch-pinfo identifier))
  (if pinfo
      (procedure-info-docstring pinfo)
      #f))

(define (list-completions word)
  (write-log 'debug
             (format "list-completions ~s" word))
  (map (lambda (entry)
         (make-apropos-info #f (car entry) (cdr entry) #f))
       (trie-entries-with-prefix (all-identifiers)
                                 (stringify word))))

(define (procedure-info-equal? left right)
  (and (equal? (procedure-info-name left)
               (procedure-info-name right))
       (equal? (procedure-info-arguments left)
               (procedure-info-arguments right))
       (equal? (procedure-info-name left)
               (procedure-info-name right))
       (equal? (procedure-info-character left)
               (procedure-info-character right))
       (equal? (procedure-info-docstring left)
               (procedure-info-docstring right))))

(define (source-meta-data-equal? left right)
  (and (procedure-info-equal? (source-meta-data-procedure-info-table left)
                              (source-meta-data-procedure-info-table right))
       (equal? (source-meta-data-imports left)
               (source-meta-data-imports right))))

(define (procedure-info->alist pinfo)
  `((name . ,(procedure-info-name pinfo))
    (arguments . ,(procedure-info-arguments pinfo))
    (line . ,(procedure-info-line pinfo))
    (character . ,(procedure-info-character pinfo))
    (docstring . ,(procedure-info-docstring pinfo))))

(define (alist->procedure-info alist)
  (make-procedure-info (alist-ref 'name alist)
                       (alist-ref 'arguments alist)
                       (alist-ref 'module alist)
                       (alist-ref 'line alist)
                       (alist-ref 'character alist)
                       (alist-ref 'docstring alist)))

(define (source-meta-data->alist* meta-data)
  `((procedure-info-table . ,(map procedure-info->alist
                             (source-meta-data-procedure-info-table
                              meta-data)))
    (import . ,(source-meta-data-imports meta-data))))

(define (alist->source-meta-data alist)
  (make-source-meta-data (alist-ref alist 'library-name)
                         (map alist->procedure-info
                                 (alist-ref alist 'procedure-info-table))
                         (alist-ref alist 'imports)))

(define (procedure-info-table->alist* pinfo-table)
  (hash-table-fold pinfo-table
                   (lambda (k v acc)
                     (cons (cons k (procedure-info->alist v))
                           acc))
                   '()))

(define (alist->procedure-info-table alist)
  (alist->hash-table
   (map (lambda (p)
          (cons (car p)
                (alist->procedure-info (cdr p))))
        alist)))

;; (define (serialize-source-meta-data-table)
;;   (hash-table-walk
;;    (identifier-to-source-meta-data-table)
;;    (lambda (k v)
;;      )))

(define (identifier-to-source-meta-data-table->alist* table)
  (hash-table-fold table
                   (lambda (k v acc)
                     (cons (cons k (source-meta-data->alist* v))
                           acc))
                   '()))

(define (alist->identifier-to-source-meta-data-table alist)
  (alist->hash-table
   (map (lambda (p)
          (cons (car p)
                (alist->source-meta-data (cdr p))))
        alist)))
