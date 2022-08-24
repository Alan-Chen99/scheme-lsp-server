GSI=gsi
GAMBIT_SYS_VERSION = $(shell $(GSI) -e "(display (system-version))")
GAMBIT_USER_LIB_DIR = $(shell $(GSI) -e '(display (path-expand "~~userlib"))')
INSTALL_DIR = $(GAMBIT_USER_LIB_DIR)/lsp-server/@/

SOURCES = ../private.sld \
          ../private.scm \
          ../document.scm \
          ../document.sld \
          ../file.scm \
          ../main.scm \
          ../server.scm \
          ../trie.scm \
          ../trie.sld \
          ../adapter.scm \
          ../adapter.sld \
          ../parse.scm \
          ../parse.sld \
          ../lsp-server.sld \
          ../gambit.scm \
          ../irregex.scm

OBJECTS = lsp-server/private@gambit$(GAMBIT_SYS_VERSION)@C/private.o1 \
          lsp-server/trie@gambit$(GAMBIT_SYS_VERSION)@C/trie.o1 \
          lsp-server/document@gambit$(GAMBIT_SYS_VERSION)@C/document.o1 \
          lsp-server/adapter@gambit$(GAMBIT_SYS_VERSION)@C/adapter.o1 \
          lsp-server/parse@gambit$(GAMBIT_SYS_VERSION)@C/parse.o1 \
          lsp-server/gambit@gambit$(GAMBIT_SYS_VERSION)@C/gambit.o1 \
          lsp-server/gambit/util@gambit$(GAMBIT_SYS_VERSION)@C/util.o1 \
          lsp-server/lsp-server@gambit$(GAMBIT_SYS_VERSION)@C/lsp-server.o1


.PHONY: all clean copy_files install uninstall

all: copy_files $(OBJECTS)
	echo $(OBJECTS)

copy_files: $(SOURCES)
	mkdir -p lsp-server/gambit
	for s in $(SOURCES); do cp -f $$s lsp-server/; done
	cp util.scm lsp-server/gambit

clean:
	rm -rf lsp-server/ *@gambit*

install: $(OBJECTS)
	mkdir -p $(INSTALL_DIR)/gambit
	cp -Rf $(SOURCES) $(INSTALL_DIR)
	cp -f util.scm $(INSTALL_DIR)/gambit/
	cp -fr lsp-server/private@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/trie@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/document@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/parse@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/gambit/util@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)/gambit
	cp -fr lsp-server/adapter@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/gambit@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)
	cp -fr lsp-server/lsp-server@gambit$(GAMBIT_SYS_VERSION)@C $(INSTALL_DIR)

uninstall:
	gsi -uninstall lsp-server

%/private.o1: lsp-server/private.sld lsp-server/private.scm
	gsc . lsp-server/private

%/trie.o1: lsp-server/trie.sld lsp-server/trie.scm
	gsc . lsp-server/trie

%/document.o1: lsp-server/document.sld lsp-server/document.scm
	gsc . lsp-server/document

%/adapter.o1: lsp-server/adapter.sld lsp-server/adapter.scm
	gsc . lsp-server/adapter

%/parse.o1: lsp-server/parse.sld lsp-server/parse.scm
	gsc . lsp-server/parse

%/gambit.o1: lsp-server/gambit.scm
	gsc . lsp-server/gambit

%/util.o1: lsp-server/gambit/util.scm
	gsc . lsp-server/gambit/util

%/lsp-server.o1: lsp-server/lsp-server.sld lsp-server/server.scm lsp-server/file.scm
	gsc . lsp-server
