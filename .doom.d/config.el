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
(map! "M-<backspace>" #'sp-backward-kill-word
      "C-c C-k" #'eval-buffer
      "C-c M-c" #'upcase-word
      "C-x M-x" #'isearch-forward-symbol-at-point
      "C-x RET" #'magit-status
      "M-W" #'delete-trailing-whitespace)


(map! "M-m" nil
      "M-m s s" #'swiper
      "M-m s S" #'swiper-isearch-thing-at-point
      "M-m s B" #'swiper-all)  ;; search all buffers


;; consult-line
;; consult-line-multi
;; consult-yank-from-kill-ring
;; orderless-flex
(map! :after consult
      "M-m s l" #'consult-line
      "M-m s m" #'consult-line-multi
      "M-y" #'consult-yank-from-kill-ring)


(map! "M-m p p" #'projectile-switch-project
      "M-m p f" #'projectile-find-file
      "M-m p r" #'projectile-replace
      "M-m p R" #'projectile-replace-regexp
      "M-m p S" #'projectile-save-project-buffers)

;; (orderless-matching-styles
;;  '(orderless-flex                       ; Basically fuzzy finding
;;    orderless-regexp
;;    ;; orderless-literal
;;    ;; orderless-prefixes
;;    ;; orderless-initialism
;;    ;; orderless-strict-leading-initialism
;;    ;; orderless-strict-initialism
;;    ;; orderless-strict-full-initialism
;;    ;; orderless-without-literal          ; Recommended for dispatches instead
;;    ))

;; Avy
(setq avy-all-windows 'all-frames)
(map! "C-c g c" #'avy-goto-char-2)


;; Navigation
(fset 'buf-move-up "\C-u10\C-p")
(fset 'buf-move-down "\C-u10\C-n")
(map! "M-U" #'buf-move-up
      "M-D" #'buf-move-down
      "C-d" #'sp-kill-sexp)


;; Smartparens Navigation
(after! smartparens
  (turn-on-smartparens-strict-mode)
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p)))

(map! :map smartparens-mode-map
      :after smartparens
      "C-M-k" #'sp-copy-sexp
      "C-M-u" #'sp-up-sexp
      "M-u" #'sp-backward-up-sexp
      "C-M-d" #'sp-down-sexp
      "M-d" #'sp-backward-down-sexp
      "C-M-j" #'sp-forward-slurp-sexp
      "C-x C-M-j" #'sp-forward-barf-sexp
      "C-M-y" #'sp-backward-slurp-sexp
      "C-x C-M-y" #'sp-backward-barf-sexp
      "C-M-n" #'sp-next-sexp
      "M-r" #'sp-raise-sexp)

(after! ace-window

  ;; Switch window letter SIZE
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 6.0)))))

  ;; Ensure ace-window works across frames.
  (setq aw-scope 'global))

(map! "M-[" #'ace-select-window
      "C-c M-[" #'ace-swap-window
      "C-x M-[" #'ace-delete-window
      ;; "M-y" #'browse-kill-ring
      "C-M-[" #'scroll-other-window-down
      "C-M-]" #'scroll-other-window
      "C-M-s" #'sp-splice-sexp
      "C-x b" #'consult-buffer
      "C-M-l" #'transpose-lines)



;; Multiple cursors
(map! "C-c m N l" #'mc/mark-next-lines
      "C-c m N t" #'mc/mark-next-like-this
      "C-c m N w" #'mc/mark-next-like-this-word
      "C-c m N W" #'mc/mark-next-word-like-this
      "C-c m N s" #'mc/mark-next-like-this-symbol
      "C-c m N S" #'mc/mark-next-symbol-like-this
      "C-c m P l" #'mc/mark-previous-lines

      "C-c s n" #'mc/skip-to-next-like-this
      "C-c s p" #'mc/skip-to-previous-like-this
      "C-c m i n" #'mc/insert-numbers

      "C-c m a t" #'mc/mark-all-like-this
      "C-c m a w" #'mc/mark-all-words-like-this
      "C-c m a s" #'mc/mark-all-symbols-like-this
      "C-c m a r" #'mc/mark-all-in-region
      "C-c m a x" #'mc/mark-all-in-region-regexp
      "C-c m a d" #'mc/mark-all-like-this-dwim
      "C-c m a D" #'mc/mark-all-dwim

      "C-c m e l" #'mc/edit-lines
      "C-c m e b" #'mc/edit-beginnings-of-lines
      "C-c m e e" #'mc/edit-ends-of-lines)

(defhydra hydra-multiple-cursors-next (general-override-mode-map "C-c m n")
  "
   Mark next"
  ("l" mc/mark-next-lines "lines")
  ("t" mc/mark-next-like-this "next")
  ("w" mc/mark-next-like-this-word "word")
  ("s" mc/mark-next-like-this-symbol "symbol")
  ("W" mc/mark-next-word-like-this "whole word")
  ("S" mc/mark-next-symbol-like-this "whole symbol")

  ("q" nil "quit" :color blue))

(defhydra hydra-multiple-cursors-previous (general-override-mode-map "C-c m p")
  "
   Mark previous"
  ("l" mc/mark-previous-lines "lines")
  ("t" mc/mark-previous-like-this "previous")
  ("w" mc/mark-previous-like-this-word "word")
  ("s" mc/mark-previous-like-this-symbol "symbol")
  ("W" mc/mark-previous-word-like-this "whole word")
  ("S" mc/mark-previous-symbol-like-this "whole symbol")

  ("q" nil "quit" :color blue))


;; Hide / Show
(map! "C-o" #'hs-toggle-hiding
      "C-c @ C-M-h" #'hs-hide-all
      "C-c @ C-M-s" #'hs-show-all)



;; Cider
(after! cider

  ;; DONT open new window on cider-connect, et al
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-auto-select-error-buffer nil))

;; (map! "C-c M-c" #'cider-connect-clj
;;       "C-c C-k" #'cider-eval-buffer)

(map! :after cider-mode
      :map clojure-mode-map
      "C-c M-c" #'cider-connect-clj
      "C-c C-k" #'cider-eval-buffer
      "," #'cider-eval-last-sexp)


;; Miscellaneous
;; To get information about any of these functions/macros, move the cursor over

(defun delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))

(map! "C-M-SPC" #'delete-whitespace-except-one
      "C-," #'+default/newline-above
      "C-." #'+default/newline-below)


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

(map! "C-c k" #'copy-line
      "C-c K" #'avy-copy-line)


;; Completions
;;
;; > Context
;;
;; Exploring the optimal Emacs Completions with
;;   Vertigo
;;   vs helm
;;   vs counsel (used in Doom)
;;   vs consult (improvement over counsel? - https://github.com/minad/consult)
;;
;;   Using Emacs Episode 80 - Vertico, Marginalia, Consult, and Embark
;;   https://www.youtube.com/watch?v=5ffb2at2d7w
;;
;;   Streamline Your Emacs Completions with Vertico
;;   https://www.youtube.com/watch?v=J0OaRy85MOo
;;   https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
;;
;;   https://github.com/minad/vertico
;;
;;
;; > Research
;;
;; Emacs Completion Explained
;; https://www.youtube.com/watch?v=fnE0lXoe7Y0
;;
;; Using Emacs Episode 80 - Vertico, Marginalia, Consult, and Embark
;; https://www.youtube.com/watch?v=5ffb2at2d7w
;; https://cestlaz.github.io/post/using-emacs-80-vertico/
;;
;; Emacs: completion framework (Embark,Consult,Orderless,etc.)
;; https://www.youtube.com/watch?v=43Dg5zYPHTU
;; https://protesilaos.com/codelog/2021-01-06-emacs-default-completion/
;;
;;
;; "One important feature is that it plugs in directly to Emacs' own completion engine unlike Helm and Ivy which have their own layer on top."
;; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
;;
;; [no] Helm, Ivy, have been superceded
;;
;; [ok] Annotations exist on command execution and help menus. Addition not needed.
;;   Marginalia - https://github.com/minad/marginalia
;;
;;
;; > Outcome
;;
;; A. These are the packages I settled on.
;;
;; ```
;; Vertico - https://github.com/minad/vertico
;; Consult - https://github.com/minad/consult
;; Embark - https://github.com/oantolin/embark
;;
;; Orderless (vs Prescient, Selectrum)
;;   https://github.com/oantolin/orderless
;;   https://github.com/radian-software/prescient.el
;;   https://github.com/radian-software/selectrum
;;
;; Corfu - https://github.com/minad/corfu
;;   Replaces Company for Completions
;; ```
;;
;; B. Lo and behold, these are most of the choices that Doom makes in its `completion/vertico/` module.
;; So all I had to do was enable it: `(doom! :completion  vertico)`.
;;
;; https://github.com/doomemacs/doomemacs/tree/master/modules/completion/vertico
;;
;; ```
;; Vertico, which provides the vertical completion user interface
;; Consult, which provides a suite of useful commands using completing-read
;; Embark, which provides a set of minibuffer actions
;; Marginalia, which provides annotations to completion candidates
;; Orderless, which provides better filtering methods
;; ```
(after! vertico

  (vertico-buffer-mode))


;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
