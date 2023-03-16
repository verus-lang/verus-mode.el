# Verus-mode

An Emacs mode for [Verus](https://github.com/verus-lang/verus).

## Features

+ Invoking Verus on current file or project (see [keybindings](#keybindings)),
  with support for jumping to errors (via `compilation-mode`).
+ Real-time verification (via `flycheck`).
+ Syntax highlighting (improved Verus-specific highlighting on top of `rustic-mode`).
+ Unicode math support (via prettify-symbols-mode)
+ Automagic detection of Verus files (via a `verus!` regex search), to enable
  the mode.
+ Automatic (configurable) verus-mode.el version checking to remind you to
  update the mode when a new version is available.

More features are planned.

## Keybindings

| Keybinding      | Description                                           |
|-----------------|-------------------------------------------------------|
| `C-c C-c C-c`   | Run Verus on the current file                         |
| `C-c C-c C-p`   | Run Verus on the current file, with profiling enabled |
| `C-c C-c C-S-c` | Run Verus on the current crate                        |

The above keybindings are only active when the current buffer is a Verus file.
If a file is not detected as a Verus file, you can manually enable Verus-mode
with `M-x verus-mode`.

## Install

### Vanilla Emacs

Clone this repository and add the following to your `~/.emacs`:

```emacs-lisp
; Path to the directory where you've cloned this repository
(add-to-list 'load-path "PATH_TO_VERUS_MODE_DIR")

(require 'verus-mode)

; Path to where you've cloned https://github.com/verus-lang/verus
(setq verus-home "PATH_TO_VERUS_DIR")
; Path to where you've cloned https://github.com/verus-lang/rust-analyzer
(setq verus-analyzer "PATH_TO_VERUS_ANALYZER"))
```

### Doom Emacs

Add the following to your `~/.doom.d/packages.el`:

```emacs-lisp
(package! verus-mode
  :recipe (:host github :repo "jaybosamiya/verus-mode.el"))
```

and the following to your `~/.doom.d/config.el`:

```emacs-lisp
(use-package! verus-mode
  :init
  ; Path to where you've cloned https://github.com/verus-lang/verus
  (setq verus-home "PATH_TO_VERUS_DIR")
  ; Path to where you've cloned https://github.com/verus-lang/rust-analyzer
  (setq verus-analyzer "PATH_TO_VERUS_ANALYZER"))
```

## Contributing

Contributions are welcome! Please open an issue or a pull request.

## License

This project is licensed under the terms of the BSD 3-Clause License. See the
[LICENSE](LICENSE) file for details.
