EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L . -L test

ELS = wakari.el wakari-review.el wakari-index.el
ELCS = $(ELS:.el=.elc)

.PHONY: all clean test compile

all: clean compile test

compile: $(ELCS)

clean:
	rm -f $(ELCS)

test:
	$(BATCH) --eval '(setq debug-on-error t)' -f package-initialize -L . -f buttercup-run-discover

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

install-deps:
	$(BATCH) -l package \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'buttercup)" \
		--eval "(package-install 'rec-mode)" \
		--eval "(package-install 'dash)"
