;;; ../.dotfiles/.doom.d/sunra.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.notes\\'" . org-mode))

(map! :map general-override-mode-map
      "M-<backspace>" #'sp-backward-kill-word
      "C-c C-k" #'eval-buffer
      "C-c M-c" #'upcase-word
      "C-x M-x" #'isearch-forward-symbol-at-point
      "C-x RET" #'magit-status
      "M-W" #'delete-trailing-whitespace)

(map! :after consult
      "M-m s s" #'consult-line
      "M-m s S" #'consult-line-multi
      "M-y" #'consult-yank-from-kill-ring)


(map! :map general-override-mode-map
      "M-m p p" #'projectile-switch-project
      "M-m p f" #'projectile-find-file
      "M-m p r" #'projectile-replace
      "M-m p R" #'projectile-replace-regexp
      "M-m p S" #'projectile-save-project-buffers)

(setq avy-all-windows 'all-frames)
(map! "C-c g c" #'avy-goto-char-2)

(fset 'buf-move-up "\C-u10\C-p")
(fset 'buf-move-down "\C-u10\C-n")
(map! "M-U" #'buf-move-up
      "M-D" #'buf-move-down
      "C-d" #'sp-kill-sexp)

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

(map! "C-o" #'hs-toggle-hiding
      "C-c @ C-M-h" #'hs-hide-all
      "C-c @ C-M-s" #'hs-show-all)

(after! cider

  ;; DONT open new window on cider-connect, et al
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-auto-select-error-buffer nil))

(map! :after clojure-mode
      :map clojure-mode-map
      "C-c M-c" #'cider-connect-clj
      "C-c C-k" #'cider-eval-buffer
      "," #'cider-eval-last-sexp)

(defun delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))

(map! "C-M-SPC" #'delete-whitespace-except-one
      "C-," #'+default/newline-above
      "C-." #'+default/newline-below)

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

(after! vertico

  (vertico-buffer-mode)
  (setq completion-styles '(orderless basic)))

(use-package! corfu

  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(setq org-roam-directory "~/roam")
