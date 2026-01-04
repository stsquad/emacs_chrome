# Makefile for Edit with Emacs

.PHONY: test release

# Get the version from manifest.json
VERSION := $(shell grep "\"version\"" manifest.json | cut -d: -f2 | cut -d\" -f2)
NAME=edit-with-emacs
RELEASE_NAME=edit-with-emacs-$(VERSION)
RELEASE_ZIP=$(RELEASE_NAME).zip

# Files to include in the release zip.
# We have to be careful not to include any dev tooling or secrets.
FILES = \
	manifest.json \
	icons \
	html \
	javascript \
	css \
	lib \
	fancy-settings \
	servers/edit-server.el \
	README.md \
	COPYING \
	NEWS

test:
	emacs -Q --batch -l ert -l servers/edit-server.el -l servers/edit-server-ert.el -f ert-run-tests-batch-and-exit

release:
	rm -f $(RELEASE_ZIP)
	zip -r $(RELEASE_ZIP) $(FILES)
