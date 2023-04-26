# The default recipe, shows this help message
help:
    @just --list --unsorted

# Open an Emacs sandbox, with Verus-mode installed
emacs-sandbox *args='':
    with-emacs.sh --no-refresh-packages --no-package --dir .emacs-sandbox -- ./verus-examples {{args}}
