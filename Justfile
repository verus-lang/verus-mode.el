# The default recipe, shows this help message
help:
    @just --list --unsorted

emacs-sandbox:
    with-emacs.sh --dir .emacs-sandbox
