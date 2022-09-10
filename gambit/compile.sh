#!/bin/sh

BASE_DIR=$(dirname $0)
SOURCE_DIR=${BASE_DIR}/..

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

gsc -exe -nopreload ${BASE_DIR} \
    ${SOURCE_DIR}/gambit/util.scm \
    ${SOURCE_DIR}/private.sld \
    ${SOURCE_DIR}/parse.sld \
    ${SOURCE_DIR}/adapter.sld \
    ${SOURCE_DIR}/gambit.scm \
    ${SOURCE_DIR}/trie.sld \
    ${SOURCE_DIR}/lsp-server.sld \
    ${SOURCE_DIR}/gambit/gambit-lsp-server.scm
