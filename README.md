# Exsequor

Project-aware command runner for Emacs using [consult](https://github.com/minad/consult).

Detects project type and offers relevant commands through a unified completion interface. Supports both project-local and global commands (e.g., global justfile, system rake tasks).

## Supported Tools

Built-in support for common build tools and task runners. Extensible via `exsequor-add-command-set`.

- **Just** (justfiles, including global)
- **Cargo** (Rust)
- **Bun** (JavaScript/TypeScript)
- **Mix** (Elixir)
- **NPM/Yarn** (Node.js)
- **Rake** (Ruby, including global)
- **Cask** (Emacs Lisp)
- **Bundle** (Ruby gems)
- **Sorbet** (Ruby type checker)
- **Gentoo overlay** tools

## Installation

Requires [Eldev](https://github.com/emacs-eldev/eldev) for development.

```sh
eldev prepare  # install dependencies
eldev compile  # compile elisp
eldev test     # run tests
```

Example configuration with `use-package` and `straight.el`:

```elisp
(use-package exsequor
  :straight (:host github :repo "vderyagin/exsequor")
  :after project
  :bind ("C-c r" . exsequor-run-global)
  :bind (:map project-prefix-map
              ("C-c C-c" . exsequor-run-in-project)))
```

## Usage

```elisp
(exsequor-run-in-project)  ; run command in current project
(exsequor-run-global)      ; run global commands (e.g., global justfile)
```

Press `C-c C-a` during completion to toggle hidden/private tasks.

## Adding Custom Command Sets

```elisp
(exsequor-add-command-set
 "MyTool"
 :items '((:name "build" :action "mytool build")
          (:name "test" :action "mytool test"))
 :predicate (lambda ()
              (file-regular-p "mytool.config")))
```

Use `:items-fn` instead of `:items` for dynamic command lists. Set `:global t` for commands available via `exsequor-run-global`.

## License

GPL-2.0-or-later
