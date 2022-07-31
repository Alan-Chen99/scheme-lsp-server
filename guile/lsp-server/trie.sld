(define-module (lsp-server trie)

#:export (make-trie
          trie?
          trie-insert!
          trie-keys
          trie-entries-with-prefix
          trie-words-with-prefix
          trie->alist
          alist->trie)

#:use-module (scheme base)
#:use-module ((srfi srfi-1) #:select (every fold))
#:use-module (srfi srfi-69)
#:use-module (lsp-server guile util))

(include "trie.scm")
