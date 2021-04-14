include config.mk

OUTPUT_DIR = $(abspath build)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

makefile_dir = $(dir $(mkfile_path))
common_dir = $(abspath ../common)
srfi_dir = ../srfi
json_rpc_dir = $(common_dir)/json-rpc

COMMON_SOURCES = $(common_dir)/base.scm $(common_dir)/basic-log.scm $(common_dir)/file.scm $(common_dir)/util.scm $(common_dir)/server.scm
GUILE_SOURCES = lsp-server-guile.sld

SOURCES = $(COMMON_SOURCES) $(GUILE_SOURCES)

JSON_RPC_GOBJECT := $(OUTPUT_DIR)/json-rpc/json-rpc.go
SRFI_145_GOBJECT := $(OUTPUT_DIR)/srfi/srfi-145.go
SRFI_180_GOBJECT := $(OUTPUT_DIR)/srfi/srfi-180.go
GUILE_GOBJECT = $(OUTPUT_DIR)/lsp-server.go

GOBJECTS = $(GUILE_GOBJECT) $(JSON_RPC_GOBJECT)

all: directories $(GOBJECTS)

directories:
	mkdir -p $(OUTPUT_DIR)/json-rpc
	mkdir -p $(OUTPUT_DIR)/srfi

$(JSON_RPC_GOBJECT): $(json_rpc_dir)/json-rpc-guile.sld
	cd $(json_rpc_dir)
	GUILE_LOAD_COMPILED_PATH=${GUILE_LOAD_COMPILED_PATH} \
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"
	cd $(makefile_dir)

$(SRFI_145_GOBJECT): srfi-145.sld
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"

$(SRFI_180_GOBJECT): $(srfi_dir)/srfi-180.scm $(SRFI_145_GOBJECT)
	cd $(srfi_180_dir)
	GUILE_LOAD_COMPILED_PATH=${OUTPUT_DIR}/:${GUILE_LOAD_COMPILED_PATH} \
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"
	cd $(makefile_dir)

$(OUTPUT_DIR)/common/%.go: $(common_dir)/%.scm $(JSON_RPC_GOBJECT)
	GUILE_LOAD_COMPILED_PATH=$(OUTPUT_DIR)/json-rpc/:${GUILE_LOAD_COMPILED_PATH} \
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"

$(OUTPUT_DIR)/%.go: $(GUILE_SOURCES) $(JSON_RPC_GOBJECT) $(COMMON_SOURCES) $(SRFI_180_GOBJECT)
	GUILE_LOAD_COMPILED_PATH=$(OUTPUT_DIR)/json-rpc/:$(OUTPUT_DIR)/:${GUILE_LOAD_COMPILED_PATH} \
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"

install: directories
	mkdir -p $(GUILE_TARGET_SCM)/lsp-server-guile/common
	mkdir -p $(GUILE_TARGET_SCM)/srfi

	mkdir -p $(GUILE_TARGET_GO)/lsp-server-guile/common
	mkdir -p $(GUILE_TARGET_GO)/srfi

	cp $(COMMON_SOURCES) $(GUILE_TARGET_SCM)/lsp-server-guile/common
	cp $(json_rpc_dir)/json-rpc-guile.sld $(GUILE_TARGET_SCM)/json-rpc.scm
	cp lsp-server-guile.sld $(GUILE_TARGET_SCM)/lsp-server.scm
	cp -R $(srfi_dir)/srfi-180.scm $(srfi_dir)/check.scm $(srfi_dir)/180 $(GUILE_TARGET_SCM)/srfi/

	cp -r $(JSON_RPC_GOBJECT) \
	      $(GUILE_TARGET_GO)/
	cp $(OUTPUT_DIR)/lsp-server.go $(GUILE_TARGET_GO)/
	cp $(OUTPUT_DIR)/srfi/*.go $(GUILE_TARGET_GO)/srfi/


uninstall:
	rm -Rf $(GUILE_TARGET_GO)/json-rpc*
	rm -Rf $(GUILE_TARGET_GO)/lsp-server-guile
	rm -Rf $(GUILE_TARGET_GO)/srfi/srfi-{145,180}*
	rm -Rf $(GUILE_TARGET_SCM)/json-rpc*
	rm -Rf $(GUILE_TARGET_SCM)/lsp-server-guile
	rm -Rf $(GUILE_TARGET_SCM)/srfi/srfi-145*
	rm -Rf $(GUILE_TARGET_SCM)/srfi/srfi-180*

clean:
	rm -rf $(OUTPUT_DIR)/*

.PHONY: all clean
