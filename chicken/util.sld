(define-library (lsp-server chicken util)

(export absolute-pathname?
        alist-ref
        alist-ref/default
        current-directory
        get-module-path
        get-absolute-pathname
        pathname-directory
        pathname-join)

(import (scheme base)
        (chicken base)
        (chicken pathname)
        (chicken process-context))

(begin
  (define (get-module-path module-name)
    #f)
  (define pathname-join make-pathname)

  (define (get-absolute-pathname path)
    (if (absolute-pathname? path)
        path
        (pathname-join (current-directory) path)))

  (define (alist-ref/default key alist default)
    (or (alist-ref key alist)
        default)))
)
