
(define-library (chibi uri)
  (export uri? uri->string make-uri string->uri string->path-uri uri-has-scheme?
          uri-scheme uri-user uri-host uri-port uri-path uri-query uri-fragment
          uri-with-scheme uri-with-user uri-with-host uri-with-path
          uri-with-query uri-with-fragment uri-resolve
          uri-encode uri-decode uri-query->alist uri-alist->query)
  (cond-expand
   (chibi
    (import (chibi) (srfi 9)))
   (else
    (import (except (scheme base) string-map string-for-each)
            (scheme char))
    (begin
      (define (string-concatenate ls)
        (apply string-append ls)))))
  (cond-expand
   (gambit (import (only (chibi string)
                         string-cursor>=?
                         string-cursor<=?
                         string-cursor-end
                         string-cursor-next
                         string-cursor-ref
                         string-cursor-start
                         substring-cursor)))
   (else (import (chibi string))))
  (import (chibi pathname))
  (include "uri.scm"))
