(define-library (lsp-server trie)

(export make-trie
        trie?
        trie-insert!
        trie-keys
        trie-entries-with-prefix
        trie-words-with-prefix
        trie->alist
        alist->trie)

(import (only (srfi 1) fold)
        (srfi 69))

(cond-expand
 (chicken (import (scheme base)))
 (gambit (import (except (scheme base) for-each))))

(include "trie.scm"))
