GUILD = guild
GUILE = guile

GUILE_TARGET_SCM ?= $(shell guile -c "(display (%site-dir))")
GUILE_TARGET_GO ?= $(shell guile -c "(display (%site-ccache-dir))")
