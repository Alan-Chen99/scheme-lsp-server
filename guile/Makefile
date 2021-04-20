include config.mk

OUTPUT_DIR = $(abspath build)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

makefile_dir = $(dir $(mkfile_path))
common_dir = $(abspath ../common)

COMMON_SOURCES = $(common_dir)/base.scm $(common_dir)/basic-log.scm $(common_dir)/file.scm $(common_dir)/util.scm $(common_dir)/server.scm
GUILE_SOURCES = ../lsp-server-guile.sld guile.scm

SOURCES = $(COMMON_SOURCES) $(GUILE_SOURCES)

GOBJECTS = $(OUTPUT_DIR)/lsp-server.go

all: directories $(GOBJECTS)

directories:
	mkdir -p $(OUTPUT_DIR)

$(OUTPUT_DIR)/%.go: $(GUILE_SOURCES) $(COMMON_SOURCES)
	GUILE_LOAD_COMPILED_PATH=$(OUTPUT_DIR):$(GUILE_TARGET_GO)/:$(OUTPUT_DIR)/:$(GUILE_LOAD_COMPILED_PATH) \
	$(GUILD) compile $(GUILE_WARNINGS) --r7rs -o "$@" "$<"

install: directories
	mkdir -p $(GUILE_TARGET_SCM)/lsp-server/common
	mkdir -p $(GUILE_TARGET_SCM)/lsp-server/guile

	cp $(COMMON_SOURCES) $(GUILE_TARGET_SCM)/lsp-server/common
	cp ../lsp-server-guile.sld $(GUILE_TARGET_SCM)/lsp-server.sld

	cp $(OUTPUT_DIR)/lsp-server.go $(GUILE_TARGET_GO)/

uninstall:
	rm -Rf $(GUILE_TARGET_GO)/lsp-server.go
	rm -Rf $(GUILE_TARGET_SCM)/lsp-server*

clean:
	rm -rf $(OUTPUT_DIR)/*

.PHONY: all clean
