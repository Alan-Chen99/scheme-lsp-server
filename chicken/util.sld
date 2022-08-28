(define-library (lsp-server chicken util)

(export absolute-pathname?
        alist-ref
        alist-ref/default
        current-directory
        get-module-path
        get-absolute-pathname
        pathname-directory
        pathname-base
        pathname-join
        uri-decode
        with-input-from-string)

(import (scheme base)
        (chicken base)
        (chicken pathname)
        (only (chicken port) with-input-from-string)
        (chicken process-context)
        (srfi 28)
        (uri-generic))

(begin
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

  (define uri-decode uri-decode-string))
)
