(define (alist-ref* keys alist)
  (let loop ((keys keys)
             (alist alist))
    (cond ((null? keys) #f)
          ((null? (cdr keys))
           (alist-ref (car keys) alist))
          (else
           (let ((sub-alist (alist-ref (car keys) alist)))
             (if sub-alist
                 (loop (cdr keys) sub-alist)
                 #f))))))

(define (get-uri-path params)
  (define uri (alist-ref* '(textDocument uri)
                          params))
  (if uri
      (parse-uri uri)
      #f))

(define (parse-uri uri)
  (define file-prefix "file://")
  (define file-prefix-length 7)
  (define uri-length (string-length uri))
  (if (<= uri-length file-prefix-length)
      #f
      (let ((prefix (substring uri 0 file-prefix-length)))
        (if (equal? prefix file-prefix)
            (substring uri file-prefix-length uri-length)
            (error "invalid uri" uri)))))

(define (identifier-char? char)
  (or (char-alphabetic? char)
      (char-numeric? char)
      (memq char '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~))))

(define (char-bracket? char)
  (memq char '(#\( #\) #\{ #\} #\[ #\])))


(cond-expand
  (guile (define (alist-ref key alist)
           (define match (assoc key alist))
           (if match
               (cdr match)
               #f)))
  (else))

(define (symbols->string sl)
  (string-append "(" (string-join (map symbol->string sl)) ")"))
