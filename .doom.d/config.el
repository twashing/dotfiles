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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys


;; General
(map! "M-<backspace>" #'sp-backward-kill-word)
(map! "C-c C-k" #'eval-buffer)
(map! "C-c M-c" #'upcase-word)
(map! "C-x M-x" #'isearch-forward-symbol-at-point)
(map! "C-x RET" #'magit-status)
(map! "M-W" #'whitespace-cleanup)


;; Swiper
(map! "M-m" nil
      "M-m s s" #'swiper)
(map! "M-m s S" #'swiper-isearch-thing-at-point)
(map! "M-m s B" #'swiper-all)  ;; search all buffers


;; Projectile
(map! "M-m p p" #'projectile-switch-project)
(map! "M-m p f" #'projectile-find-file)
(map! "M-m p r" #'projectile-replace)
(map! "M-m p R" #'projectile-replace-regexp)
(map! "M-m p S" #'projectile-save-project-buffers)


;; Avy
(map! "C-c g c" #'avy-goto-char-2)


;; Navigation
(fset 'buf-move-up "\C-u10\C-p")
(fset 'buf-move-down "\C-u10\C-n")
(map! "M-U" #'buf-move-up)
(map! "M-D" #'buf-move-down)
(map! "C-d" #'sp-kill-sexp)
(map! "C-M-k" #'copy-sexp-at-point)
(map! "M-[" #'ace-select-window)
(map! "C-c M-[" #'ace-swap-window)
(map! "C-x M-[" #'ace-delete-window)
(map! "M-y" #'browse-kill-ring)
(map! "C-M-SPC" #'delete-whitespace-except-one)
(map! "C-M-[" #'scroll-other-window-down)
(map! "C-M-]" #'scroll-other-window)
(map! "C-M-s" #'sp-splice-sexp)


;; Smartparens Navigation
(map! "C-M-u" #'sp-up-sexp)
(map! "M-u" #'sp-backward-up-sexp)
(map! "C-M-d" #'sp-down-sexp)
(map! "M-d" #'sp-backward-down-sexp)
(map! "C-M-j" #'sp-forward-slurp-sexp)
(map! "C-x C-M-j" #'sp-forward-barf-sexp)
(map! "C-M-y" #'sp-backward-slurp-sexp)
(map! "C-x C-M-y" #'sp-backward-barf-sexp)
(map! "C-M-n" #'sp-next-sexp)
(map! "M-r" #'sp-raise-sexp)


;; Multiple cursors
(map! "C-c m n l" #'mc/mark-next-lines)
(map! "C-c m n t" #'mc/mark-next-like-this)
(map! "C-c m n w" #'mc/mark-next-like-this-word)
(map! "C-c m n W" #'mc/mark-next-word-like-this)
(map! "C-c m n s" #'mc/mark-next-like-this-symbol)
(map! "C-c m n S" #'mc/mark-next-symbol-like-this)
(map! "C-c s n" #'mc/skip-to-next-like-this)
(map! "C-c s p" #'mc/skip-to-previous-like-this)
(map! "C-c m i n" #'mc/insert-numbers)
(map! "C-c m p l" #'mc/mark-previous-lines)
(map! "C-c m a t" #'mc/mark-all-like-this)
(map! "C-c m a w" #'mc/mark-all-words-like-this)
(map! "C-c m a s" #'mc/mark-all-symbols-like-this)
(map! "C-c m a r" #'mc/mark-all-in-region)
(map! "C-c m a x" #'mc/mark-all-in-region-regexp)
(map! "C-c m a d" #'mc/mark-all-like-this-dwim)
(map! "C-c m a D" #'mc/mark-all-dwim)
(map! "C-c m e l" #'mc/edit-lines)
(map! "C-c m e b" #'mc/edit-beginnings-of-lines)
(map! "C-c m e e" #'mc/edit-ends-of-lines)


;; Hide / Show
(map! "C-o" #'hs-toggle-hiding)
(map! "C-c @ C-M-h" #'hs-hide-all)
(map! "C-c @ C-M-s" #'hs-show-all)


;; Miscellaneous
;; To get information about any of these functions/macros, move the cursor over
(map! "C-," #'+default/newline-above)
(map! "C-." #'+default/newline-below)


;; Copy line
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
  kill-line, so see documentation of kill-line for how to use it including prefix
  argument and relevant variables.  This function works by temporarily making the
  buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-line arg)))

(map! "C-c k" #'copy-line)
(map! "C-c K" #'avy-copy-line)


;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
