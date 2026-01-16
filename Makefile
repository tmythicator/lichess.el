.PHONY: test compile format lint checkdoc clean

EMACS ?= emacs

# Find all elisp files except test files
ELS = $(filter-out test/%, $(wildcard *.el))
TEST_ELS = $(wildcard test/*.el)

test:
	$(EMACS) -batch -L . $(foreach f,$(TEST_ELS),-l $(f)) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -L . -f batch-byte-compile $(ELS)

# Common Emacs command for installing packages and running commands
EMACS_BATCH = $(EMACS) -batch -Q \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	--eval "(package-initialize)" \
	--eval "(unless package-archive-contents (package-refresh-contents))"

format:
	$(EMACS_BATCH) \
		--eval "(unless (package-installed-p 'elisp-autofmt) (package-install 'elisp-autofmt))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(setq elisp-autofmt-python-bin \"python3\")" \
		--eval "(mapc (lambda (f) (with-current-buffer (find-file-noselect f) (elisp-autofmt-buffer) (save-buffer))) (directory-files \".\" t \"^[^.].*\\\\.el$$\"))"

format-check:
	$(EMACS_BATCH) \
		--eval "(unless (package-installed-p 'elisp-autofmt) (package-install 'elisp-autofmt))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(setq elisp-autofmt-python-bin \"python3\")" \
		--eval "(setq elisp-autofmt-buffer-dry-run t)" \
		--eval "(mapc (lambda (f) (with-current-buffer (find-file-noselect f) (let ((formatted (elisp-autofmt-buffer))) (when (stringp formatted) (message \"File %s needs formatting\" f) (kill-emacs 1))))) (directory-files \".\" t \"^[^.].*\\\\.el$$\"))"

lint:
	$(EMACS_BATCH) \
		--eval "(unless (package-installed-p 'package-lint) (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(ELS)

checkdoc:
	$(EMACS) -Q -batch \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-force-docstrings-flag nil)" \
		--eval "(let ((files (directory-files \".\" t \"\\\\.el$$\")) (err 0)) \
			(dolist (file files) \
				(when (and (not (string-match-p \"test/\" file)) \
						   (not (string-match-p \"secrets\" file)) \
						   (not (string-match-p \".dir-locals\" file))) \
					(checkdoc-file file) \
					(let ((buf (get-buffer \"*checkdoc-messages*\"))) \
						(when buf \
							(with-current-buffer buf \
								(when (> (buffer-size) 0) \
									(princ (format \"\nCheckdoc output for %s:\n%s\" file (buffer-string))) \
									(setq err 1))))))) \
			(kill-emacs err))"

clean:
	rm -f *.elc test/*.elc
