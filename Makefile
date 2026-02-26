# markdown-table-wrap Makefile

SHELL := /bin/bash
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .

# Test selector: run a subset of tests by ERT pattern
# Example: make test SELECTOR=parse
SELECTOR ?=

# Verbose output for tests
# Example: make test VERBOSE=1
VERBOSE ?=

.PHONY: test compile lint lint-checkdoc lint-package check check-parens bench bench-batch clean help install-hooks

help:
	@echo "Targets:"
	@echo "  make test           Run ERT tests (SELECTOR=pattern, VERBOSE=1)"
	@echo "  make compile        Byte-compile with warnings-as-errors"
	@echo "  make lint           Checkdoc + package-lint"
	@echo "  make lint-checkdoc  Docstring warnings only"
	@echo "  make lint-package   MELPA package conventions only"
	@echo "  make check-parens   Verify balanced parentheses"
	@echo "  make check          compile + lint + test (pre-commit)"
	@echo "  make install-hooks  Set up git pre-commit hook"
	@echo "  make bench          Benchmark (GUI via xvfb — matches real Emacs)"
	@echo "  make bench-batch    Benchmark (--batch — no font engine, faster)"
	@echo "  make clean          Remove .elc files"

test:
	@echo "=== Tests ==="
	@set -o pipefail; \
	OUTPUT=$$(mktemp); \
	$(BATCH) -L test \
		-l markdown-table-wrap-test \
		$(if $(SELECTOR),--eval '(ert-run-tests-batch-and-exit "$(SELECTOR)")',-f ert-run-tests-batch-and-exit) \
		>$$OUTPUT 2>&1; \
	STATUS=$$?; \
	if [ "$(VERBOSE)" = "1" ] || [ $$STATUS -ne 0 ]; then \
		cat $$OUTPUT; \
	else \
		grep -v "^   passed\|^Running [0-9]\|^$$" $$OUTPUT; \
	fi; \
	rm -f $$OUTPUT; \
	exit $$STATUS

compile:
	@rm -f *.elc
	@echo "=== Byte-compile ==="
	@$(BATCH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile markdown-table-wrap.el

lint: lint-checkdoc lint-package

lint-checkdoc:
	@echo "=== Checkdoc ==="
	@OUTPUT=$$($(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"markdown-table-wrap.el\")" 2>&1); \
	WARNINGS=$$(echo "$$OUTPUT" | grep -A1 "^Warning" | grep -v "^Warning\|^--$$"); \
	if [ -n "$$WARNINGS" ]; then echo "$$WARNINGS"; exit 1; else echo "OK"; fi

lint-package:
	@echo "=== Package-lint ==="
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) \
		          (package-refresh-contents) \
		          (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"markdown-table-wrap.el\")" \
		-f package-lint-batch-and-exit markdown-table-wrap.el

check-parens:
	@echo "=== Check Parens ==="
	@OUTPUT=$$($(BATCH) \
		--eval '(condition-case err \
		          (with-current-buffer (find-file-noselect "markdown-table-wrap.el") \
		            (check-parens) \
		            (message "markdown-table-wrap.el OK")) \
		          (user-error \
		           (message "FAIL: %s" (error-message-string err)) \
		           (kill-emacs 1)))' 2>&1); \
	echo "$$OUTPUT" | grep -E "OK$$|FAIL:"; \
	echo "$$OUTPUT" | grep -q "FAIL:" && exit 1 || true

check: compile lint test

# ============================================================
# Benchmarks
# ============================================================

# GUI mode (via xvfb-run): string-width uses the font engine, same as
# real Emacs.  This is the number that matters for users.
# Pass REPS=N to change repetition count (default 5).
REPS ?= 5
BENCH_LOAD = -L . -l bench/markdown-table-wrap-bench.el

bench:
	@xvfb-run -a env GDK_BACKEND=x11 $(EMACS) -Q $(BENCH_LOAD) \
		-f markdown-table-wrap-bench-run-batch -- -c $(REPS) 2>&1

# Batch mode: no font engine, string-width uses character tables.
# Faster but not representative of real usage.
bench-batch:
	@$(BATCH) $(BENCH_LOAD) \
		-f markdown-table-wrap-bench-run-batch -- -c $(REPS)

install-hooks:
	@git config core.hooksPath hooks
	@echo "Git hooks installed (using hooks/)"

clean:
	@rm -f *.elc test/*.elc
