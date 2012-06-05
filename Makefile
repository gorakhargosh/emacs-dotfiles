RM = rm -rf
EMACS = emacs -Q -batch

ELISP_LOAD_PATH := -L .
# find vendor -type d -not \( -path '*.git*' -or -path '*.svn*' -or -path '*.hg*' \) -print0
ELISP_SOURCES := $(wildcard *.el)
ELISP_TARGET := $(ELISP_SOURCES:.el=.elc)

.PHONY: all build clean distclean push help

all: clean build

help:
	@echo "Possible targets:"
	@echo "    clean       - Cleans up."
	@echo "    push        - 'git push' to all hosted repositories"

push: push-google push-github

push-google:
	@echo "Pushing repository to remote:google [code.google.com]"
	@git push google master

push-github:
	@echo "Pushing repository to remote:origin [github.com]"
	@git push origin master


%.elc: %.el
	$(EMACS) $(ELISP_LOAD_PATH) -f batch-byte-compile $<

$(ELISP_TARGET): $(ELISP_SOURCES)

build: $(ELISP_TARGET)

clean:
	find . -iname '*.elc' -print0 | xargs -0 rm
	#@$(RM) *.elc *~

distclean: clean
	@$(RM) elpa/ backups/ auto-save-list/ eshell/ swank/ url/ ac-comphist.dat
