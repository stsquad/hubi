# Agent Guide for Hubi

## Build and Test
- **Test All**: `make test`
- **Single Test**: `emacs -batch -Q -L . -l hubi-ert.el --eval '(ert-run-tests-batch-and-exit "test-name-regexp")'`
- **Lint**: `emacs -batch -Q -L . --eval '(checkdoc-file "hubi.el")'`

## Coding Style
- **Language**: Emacs Lisp (Elisp) with `lexical-binding: t` is mandatory.
- **Naming**: Use `hubi-` prefix for public functions/variables and `hubi--` for internal/private ones.
- **Dependencies**: Use `(require 'feature)` at the top level.
- **Configuration**: Use `defcustom` for user-facing options with appropriate `:group` and `:type`.
- **Formatting**: Adhere to standard Elisp indentation. Use `cl-lib` for common Lisp utilities.
- **Documentation**: All public functions MUST have a docstring. Use `checkdoc` to verify.
- **Error Handling**: Use `(error "Message")` for user errors or `(signal 'symbol ...)` for specific conditions.
- **Tests**: Add new tests to `hubi-ert.el` using `ert-deftest`.
