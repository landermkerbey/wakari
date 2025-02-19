EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L .

ELS = wakari.el
ELCS = $(ELS:.el=.elc)

.PHONY: all clean test compile

all: clean compile test

compile: $(ELCS)

clean:
	rm -f $(ELCS)

test:
	$(BATCH) -l buttercup -f buttercup-run-discover

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

install-deps:
	$(BATCH) -l package \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'buttercup)" \
		--eval "(package-install 'rec-mode)"
