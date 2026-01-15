.PHONY: test compile format lint clean

EMACS ?= emacs

# Find all elisp files except test files
ELS = $(filter-out test/%, $(wildcard *.el))
TEST_ELS = $(wildcard test/*.el)

test:
	$(EMACS) -batch -L . -l test/lichess-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -L . -f batch-byte-compile $(ELS)

format:
	$(EMACS) -batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'elisp-autofmt) (package-refresh-contents) (package-install 'elisp-autofmt))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(setq elisp-autofmt-python-bin \"python3\")" \
		--eval "(mapc (lambda (f) (with-current-buffer (find-file-noselect f) (elisp-autofmt-buffer) (save-buffer))) (directory-files \".\" t \"^[^.].*\\\\.el$$\"))"

format-check:
	$(EMACS) -batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'elisp-autofmt) (package-refresh-contents) (package-install 'elisp-autofmt))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(setq elisp-autofmt-python-bin \"python3\")" \
		--eval "(setq elisp-autofmt-buffer-dry-run t)" \
		--eval "(mapc (lambda (f) (with-current-buffer (find-file-noselect f) (let ((formatted (elisp-autofmt-buffer))) (when (stringp formatted) (message \"File %s needs formatting\" f) (kill-emacs 1))))) (directory-files \".\" t \"^[^.].*\\\\.el$$\"))"

lint:
	$(EMACS) -batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		-f package-lint-batch-and-exit $(ELS)

clean:
	rm -f *.elc
