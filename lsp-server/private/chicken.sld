(define-library (lsp-server private chicken)

(export absolute-pathname?
        alist-ref
        alist-ref/default
        build-module-egg-table
        chicken-program-path
        condition->string
        current-directory
        get-module-path
        module-egg
        init-module-egg-table!
        get-module-path
        get-absolute-pathname
        pathname-base
        pathname-directory
        pathname-extension
        pathname-strip-extension
        pathname-join
        uri-decode
        with-input-from-string)

(import (only (chicken base)
              alist-ref)
        (except (r7rs)
                string-length string-ref string-set! make-string string substring
                string->list list->string string-fill! write-char read-char)
        (chicken condition)
        (chicken file)
        (chicken foreign)
        (chicken irregex)
        (chicken pathname)
        (chicken platform)
        (only (chicken port) with-input-from-string with-output-to-string)
        (chicken process)
        (chicken process-context)
        (chicken tcp)
        (only (utf8-srfi-13) string-join string-concatenate)
        (srfi 28)
        (srfi 69)
        (uri-generic)
        (only utf8 string-split))

(begin
  ;;; A hash table mapping module (symbol) to egg (symbol). This is needed
  ;;; to fetch the correct documentation with chicken-doc
  (define module-egg-table (make-hash-table))

  (define eggs-path
    (make-pathname (system-cache-directory) "chicken-install"))

  (define (chicken-status)
    (make-pathname
     (foreign-value "C_TARGET_BIN_HOME" c-string)
     (foreign-value "C_CHICKEN_STATUS_PROGRAM" c-string)))

  (define (chicken-program-path)
    (make-pathname
     (foreign-value "C_TARGET_BIN_HOME" c-string)
     (foreign-value "C_CSC_PROGRAM" c-string)))

  (define (init-module-egg-table!)
    (set! module-egg-table (build-module-egg-table)))

  (define (join-module-name-parts mod)
    (cond ((pair? mod)
           (list
            (string->symbol
             (format "~a"
                     (string-join (map (lambda (p) (format "~a" p))
                                       mod)
                                  "-")))))
          (else mod)))

  (define (find-egg-directory-path egg-name)
    (let ((path (glob (make-pathname eggs-path (format "~a" egg-name)))))
      (if (null? path)
          #f
          (car path))))

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

  (define (get-module-path mod)
    (let ((egg (or (hash-table-ref/default module-egg-table mod #f)
                   (hash-table-ref/default module-egg-table
                                           (join-module-name-parts mod)
                                           #f))))
      (if egg
          (find-egg-directory-path egg)
          #f)))

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

  (define (module-egg mod)
    (hash-table-ref/default module-egg-table
                            mod
                            #f))

  (define (condition->string exc)
    (with-output-to-string
      (lambda () (print-error-message exc)))))
)
