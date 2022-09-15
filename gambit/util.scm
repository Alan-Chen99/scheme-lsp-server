(define-library (lsp-server gambit util)

(export alist-ref
        alist-ref/default
        irregex
        irregex-match
        irregex-match-substring
        irregex-match-start-index
        irregex-search
        prefix-identifier
        vector-fold
        with-input-from-string

        tcp-read-timeout
        tcp-accept
        tcp-close
        tcp-connect
        tcp-listen
        get-absolute-pathname
        find-files
        pathname-directory
        pathname-join)

(import (gambit)
        (only (srfi 1) append-map)
        (only (srfi 13) string-join)
        (srfi 28)
        (github.com/ashinn/irregex irregex))

(begin
  ;;; copied over from srfi-133
  (define (vector-fold kons knil vec1 . o)
    (let ((len (vector-length vec1)))
      (if (null? o)
          (let lp ((i 0)
                   (acc knil))
            (if (>= i len)
                acc
                (lp (+ i 1)
                    (kons acc (vector-ref vec1 i)))))
          (let lp ((i 0)
                   (acc knil))
            (if (>= i len)
                acc
                (lp (+ i 1)
                    (apply kons acc (vector-ref vec1 i)
                           (map (lambda (v)
                                  (vector-ref v i))
                                o))))))))

  (define (with-input-from-string str thunk)
    (define p (open-input-string str))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (parameterize ((current-input-port p))
            (thunk)))
        (lambda () (close-input-port p))))

  (define (alist-ref key lst)
    (define res (assoc key lst))
    (if res
        (cdr res)
        #f))

  (define (alist-ref/default key lst default)
    (define res (assoc key lst))
    (if res
        (cdr res)
        default))

  (define (prefix-identifier mod-name identifier)
    (string->symbol
     (format "~a#~a"
             (canonicalize-module-name mod-name)
             identifier)))

  (define (canonicalize-module-name mod-name)
    (cond ((symbol? mod-name)
           mod-name)
          ((list? mod-name)
           (string->symbol
            (string-join (map symbol->string mod-name)
                         "/")))
          (else (error "expecting a valid module name" mod-name))))

  (define tcp-read-timeout (make-parameter #f))

  (define (tcp-listen port-number)
    (open-tcp-server port-number))

  (define (tcp-accept listener)
    (let ((p (read listener)))
      (values p p)))

  (define (tcp-close listener)
    (close-port listener))

  (define (tcp-connect tcp-address tcp-port-number)
    (let ((p (open-tcp-client tcp-port-number)))
      (values p p)))

  (define (get-absolute-pathname p)
    (path-expand (path-normalize p)))

  (define (pathname-join dirname filename)
    (string-append dirname "/" filename))

  (define pathname-directory path-directory)

  (define (directory? f)
    (eq? (file-type f) 'directory))

  (define (regular-file? f)
    (eq? (file-type f) 'regular))

  (define (find-files path test)
    (if (and (regular-file? path)
             (test path))
        (list path)
        (let ((files (map (lambda (f) (pathname-join path f))
                          (directory-files path))))
          (append (filter (lambda (f)
                            (and (regular-file? f)
                                 (test f)))
                          files)
                  (append-map (lambda (d) (find-files d test))
                              (filter directory? files))))))))
