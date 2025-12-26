EMACS ?= emacs

.PHONY: test clean

test: clean
	$(EMACS) -batch -Q -L . -l hubi-ert.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
