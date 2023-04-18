# Verus-mode

An Emacs mode for [Verus](https://github.com/verus-lang/verus).

## Features

+ Invoking Verus on current file or project (see [keybindings](#keybindings)),
  with support for jumping to errors (via `compilation-mode`).
+ Real-time verification (via `flycheck`).
+ Syntax highlighting (improved Verus-specific highlighting on top of `rustic-mode`).
+ Jump-to-definition (via `dumb-jump`)
+ Unicode math support (via `prettify-symbols-mode`)
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
| `C-c C-n`       | Jump to next (flycheck) error                         |
| `C-c C-p`       | Jump to previous (flycheck) error                     |

The above keybindings are only active when the current buffer is a Verus file.
If a file is not detected as a Verus file, you can manually enable Verus-mode
with `M-x verus-mode`.

The movement commands to jump to previous or next error move across the flycheck
highlights (i.e., the underlines/squigglies/... you see in your buffer when you
hit save). If you instead prefer to move across the `*compilation*` buffer that
opens when you use any of the "Run Verus on ..." commands, then you can use
standard Emacs keybindings `M-g n` or `M-g p` for next/previous error
respectively.

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

| Keybinding | Description                                                         |
|------------|---------------------------------------------------------------------|
| `C-c ! l`  | List all errors in a separate buffer                                |
| `C-c ! n`  | Jump to next error (verus-mode.el also binds this to `C-c C-n`)     |
| `C-c ! p`  | Jump to previous error (verus-mode.el also binds this to `C-c C-p`) |

## Jump to definition

Verus-mode sets up the necessary things to make `dumb-jump` work. This means,
the regular xref keybindings should work. For quick reference, some useful ones
are included here:

| Keybinding | Description                          |
|------------|--------------------------------------|
| `M-.`      | Jump to definition                   |
| `M-,`      | Jump back                            |

To make sure that jump to definition works properly as well as fast, you need to
make sure you have [ripgrep](https://github.com/BurntSushi/ripgrep) installed on
your system with PCRE2 support (you can confirm this by running `rg --engine
pcre2 foo` in an empty directory: it'll complain about lack of pcre2 support if
you don't have it).

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
[rustic](https://github.com/brotzeit/rustic) setup and such.

If you are starting _entirely_ from scratch with Emacs, then you may wish to
instead just copy [this `init.el` file](./.emacs-sandbox/init.el) to
`~/.emacs.d/init.el`, and start from there, since it will install all the
pre-requisites for Verus-mode.el.

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
