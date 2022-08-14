(define-library (lsp-server trie)

(export make-trie
        trie?
        trie-insert!
        trie-keys
        trie-entries-with-prefix
        trie-words-with-prefix
        trie->alist
        alist->trie)

(import (scheme base)
        (srfi 1)
        (srfi 69))

(include "trie.scm"))
