RM = rm -rf

.PHONY: all clean distclean push help

all:

help:
	@echo "Possible targets:"
	@echo "    clean       - Cleans up."
	@echo "    push        - 'git push' to all hosted repositories"

push: push-google push-github push-origin

push-google:
	@echo "Pushing repository to remote:google [code.google.com]"
	@git push google master

push-github:
	@echo "Pushing repository to remote:github [github.com]"
	@git push github master

push-origin:
	@echo "Pushing repository to remote:origin"
	@git push origin master

clean:
	@$(RM) *.elc *~

distclean: clean
	@$(RM) elpa/ backups/ auto-save-list/ eshell/ swank/ url/ ac-comphist.dat
