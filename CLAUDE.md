# CLAUDE.md

Guidance for Claude Code working in this repository.

This file records only what the code cannot tell you. Anything answerable by reading
`init.el` (one `leaf` block per concern) is deliberately left out — grep it.

## Conventions

- Configuration is written as `leaf` blocks. Add a new block rather than a bare `setq`.
- Prefer a `:custom` entry over `setq` in `:config`. Several user options only take effect
  through their `:set` function, which `:custom` (`customize-set-variable`) runs and `setq`
  does not.
- Packages come from package.el + MELPA/GNU ELPA. Not straight.el, not elpaca.
- LSP is lsp-mode. Not eglot.
- Prefer built-in Emacs functionality over a third-party package when both exist.
- `custom.el` is tracked, because it holds `package-selected-packages`. Caches and
  per-machine state are ignored; see `.gitignore`.

## Environment

- Requires Emacs 31 or later: `treesit-enabled-modes` does not exist before that.
- `/Applications/Emacs.app` is managed by `brew install --cask emacs-app`. Do not suggest
  the Homebrew `emacs` *formula*: it is built `--without-ns --without-x` and has no GUI,
  which this config depends on.
- Node is managed by mise. Emacs picks the toolchain up through `exec-path-from-shell`,
  so a language server installed globally with npm is found without extra configuration.

## Why the TypeScript setup looks like this

The `treesit` block enumerates modes in `treesit-enabled-modes` instead of setting it to
`t`. The `t` value remaps every tree-sitter mode at once, which would swap the major mode
out from under hooks written against the non-ts modes (yaml, js, python, c are all hooked
somewhere in `init.el`). Adding a language means moving its hooks first.

The `lsp-mode` block disables the `ts-ls` client and points the `tsgo` client at `tsc`.
TypeScript 7 is the native compiler: it ships no `tsserver.js`, so `typescript-language-server`
cannot run, and the compiler binary serves LSP itself via `--lsp --stdio`. `ts-ls` outranks
`tsgo` by priority, so it has to be disabled explicitly rather than merely left unused.

## Checking a change

```bash
emacs -Q --batch -l ~/.emacs.d/init.el --eval '(message "ok")'
```

Warnings from `leaf` blocks are reported as `Warning (leaf): Error in ... block`, so grep the
output for `leaf` rather than trusting the exit code. To check which major mode or LSP client
a file would get, visit it in the same batch invocation and print `major-mode` or
`(mapcar #'lsp--client-server-id (lsp--find-clients))`.
