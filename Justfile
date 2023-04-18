# The default recipe, shows this help message
help:
    @just --list --unsorted

# Open an Emacs sandbox, with Verus-mode installed
emacs-sandbox:
    with-emacs.sh --dir .emacs-sandbox ./verus-examples
