# Doom Emacs Configuration

## Configuration

`~/.config/emacs` exists and contains the Doom Emacs framework itself (the core code — you can see `lisp/`, `modules/`, `bin/`, etc., and it's a git repo).

There's also a second relevant directory:

`~/.config/doom` — this is a symlink pointing to `~/Projects/dotfiles/.doom.d`. This is your personal Doom configuration (where your `init.el`, `config.el`, `packages.el` live).

So the two directories serve different purposes:

| Path | Purpose |
|---|---|
| `~/.config/emacs` | Doom Emacs framework/core (installed code) |
| `~/.config/doom` → `~/Projects/dotfiles/.doom.d` | Your personal config (customizations) |

The traditional `~/.emacs.d` and `~/.doom.d` directories don't exist — everything is under `~/.config/`, which is the modern XDG-style layout that newer Doom Emacs versions use.
