#!/bin/bash

REPO_PATH=${CHICKEN_EGG_CACHE}/../
CHICKEN_DOC_PATH=${REPO_PATH}/share/chicken-doc

if ! [ -z "$SALMONELLA_RUNNING" ]; then
    mkdir -p ${CHICKEN_DOC_PATH}
    echo "((version . 4))" > ${CHICKEN_DOC_PATH}/.chicken-doc-repo
fi
