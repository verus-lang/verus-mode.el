# The default recipe, shows this help message
help:
    @just --list --unsorted

# Run all integration tests (or filter by PATTERN if provided). Tip: Use VERBOSE=1 for verbose output
test PATTERN='':
    @echo "Running verus-mode integration tests..."
    @test/run-tests.sh {{PATTERN}}

# Open an Emacs sandbox, with Verus-mode installed
emacs-sandbox *args='':
    with-emacs.sh --no-refresh-packages --no-package --dir .emacs-sandbox -- ./verus-examples {{args}}
