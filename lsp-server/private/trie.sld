(define-library (lsp-server private trie)

(export make-trie
        trie?
        trie-insert!
        trie-keys
        trie-entries-with-prefix
        trie-words-with-prefix
        trie->alist
        alist->trie)

(import (only (srfi 1) every fold)
        (srfi 69))

(cond-expand
 (chicken (import (except (scheme base)
                          string-length string-ref string-set! make-string string substring
                          string->list list->string string-fill! write-char read-char)
                  (only utf8 string-ref list->string string-length)))
 (gambit (import (except (scheme base) for-each)))
 (guile (import (only (scheme base) define-record-type))))

(include "trie-impl.scm"))
