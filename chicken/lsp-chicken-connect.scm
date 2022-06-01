#! /usr/local/bin/csi -ss

(import (lsp-server chicken))

(include "../src/lsp-connect.scm")

(main (cdr (command-line)))


