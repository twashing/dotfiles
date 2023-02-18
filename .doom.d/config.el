;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Timothy Washington"
      user-mail-address "twashing@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "PragmataPro" :size 16 :weight 'light)
      doom-variable-pitch-font (font-spec :family "PragmataPro" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(load! "sunra")


;; https://github.com/tumashu/vertico-posframe
;; not working... (require 'vertico-posframe)

;; (load! "vertico-posframe")
;; (vertico-posframe-mode nil)
;;
;; (setq vertico-multiform-commands
;;       '((consult-line
;;          posframe
;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
;;          (vertico-posframe-border-width . 10)
;;          ;; NOTE: This is useful when emacs is used in both in X and
;;          ;; terminal, for posframe do not work well in terminal, so
;;          ;; vertico-buffer-mode will be used as fallback at the
;;          ;; moment.
;;          (vertico-posframe-fallback-mode . vertico-buffer-mode))
;;         (t posframe)))
;; (vertico-multiform-mode 1)


;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
