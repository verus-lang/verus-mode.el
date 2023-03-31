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

## Extra Arguments Support

To pass extra arguments to Verus, either use `C-u` before using any of the above
"Run Verus" commands, or add a new table to the `Cargo.toml` for your Verus
project to have it automatically picked up:

``` toml
[package.metadata.verus.ide]
extra_args = "......"
```

## Real-time verification

Since verus-mode uses flycheck (run upon every save), regular flycheck
keybindings work too. For quick reference, some of the particularly useful ones
are included here:

| Keybinding | Description                          |
|------------|--------------------------------------|
| `C-c ! l`  | List all errors in a separate buffer |
| `C-c ! n`  | Jump to next error                   |
| `C-c ! p`  | Jump to previous error               |

## Unicode Math Prettification

Verus-mode displays keywords like `forall` and `exists` using mathematical
symbols `∀` and `∃`. If you dislike this behavior, you can disable this by
turning off `prettify-symbols-mode` (use `M-x prettify-symbols-mode` to toggle
this in a single buffer).

## Install

Your installation steps for using `verus-mode.el` may vary depending on how your
personal Emacs is set up.

### If you are using regular vanilla Emacs

Clone this repository and add the following to your `~/.emacs`:

```emacs-lisp
; Path to the directory where you've cloned this repository
(add-to-list 'load-path "PATH_TO_VERUS_MODE_DIR")

(require 'verus-mode)

; Path to where you've cloned https://github.com/verus-lang/verus
(setq verus-home "PATH_TO_VERUS_DIR")
```

`verus-mode.el` expects that you already have a working
[rustic](https://github.com/brotzeit/rustic) setup and such; if you are starting
_entirely_ from scratch with Emacs, then you may wish to include the following
code in your `.emacs` file _before_ the above code to make sure you have all the
pre-requisites:

<details><summary>Click to expand</summary>

``` emacs-lisp
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)
(or (require 'use-package nil t)
  (progn
	  (package-install 'use-package)
	  (message "On a new system. Just installed use-package!")))
(use-package flycheck :ensure t)
(use-package rustic :ensure t)
```

</details>

### If you are using Doom Emacs

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
```

## Contributing

Contributions are welcome! Please open an issue or a pull request.

## License

This project is licensed under the terms of the BSD 3-Clause License. See the
[LICENSE](LICENSE) file for details.
