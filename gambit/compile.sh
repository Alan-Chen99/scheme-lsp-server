#!/bin/sh

BASEDIR=$(dirname $0)

echo -n "Checking for existing installation...   "
gsi -e '(import (codeberg.org/rgherdt/scheme-lsp-server gambit util)) (exit)' >/dev/null
if [ $? -ne 0 ]; then
    echo "not found."
    echo "This scripts only compiles an existing installation of the library."
    echo "Please follow the install instructions and run this script again."
    exit 1
fi

echo "found."
echo "Building installed modules."

gsc codeberg.org/rgherdt/scheme-lsp-server/gambit/util \
    codeberg.org/rgherdt/scheme-lsp-server/private \
    codeberg.org/rgherdt/scheme-lsp-server/parse \
    codeberg.org/rgherdt/scheme-lsp-server/adapter \
    codeberg.org/rgherdt/scheme-lsp-server/gambit \
    codeberg.org/rgherdt/scheme-lsp-server/trie \
    codeberg.org/rgherdt/scheme-lsp-server/lsp-server

echo "Compiling gambit-lsp-server executable."

gsc -exe -nopreload . \
    ../gambit/util.scm \
    ../private.sld \
    ../parse.sld \
    ../adapter.sld \
    ../gambit.scm \
    ../trie.sld \
    ../lsp-server.sld \
    gambit-lsp-server.scm
