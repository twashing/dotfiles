;;; ../.dotfiles/.doom.d/sunra.el -*- lexical-binding: t; -*-

(setq

 ;; Set *scratch* buffer to lisp-interaction-mode
 ;; https://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
 ;; https://emacs.stackexchange.com/questions/3830/why-does-lisp-interaction-mode-exist-and-do-we-ever-need-it
 initial-major-mode 'lisp-interaction-mode


 ;; If you use `org' and don't want your org files in the default location below,
 ;; change `org-directory'. It must be set before org loads!
 org-directory "~/org/"

 ;; This determines the style of line numbers in effect. If set to `nil', line
 ;; numbers are disabled. For relative line numbers, set this to `relative'.
 display-line-numbers-type t

 ;; "confirm-kill-emacs is non-nil by default. The doom-quit module only adds silly confirmation messages to it. Do this to completely disable it."
 ;; https://github.com/doomemacs/doomemacs/issues/2688#issuecomment-596684817
 confirm-kill-emacs nil

 ;; Disable auto-comment on `newline-and-indent`
 ;; https://discord.com/channels/406534637242810369/1038583508140048425
 +default-want-RET-continue-comments nil
 +evil-want-o/O-to-continue-comments nil

 ;; When minibuffer offers tab completion, make that case-insensitive
 ;; https://emacs.stackexchange.com/a/32408/10528
 completion-ignore-case t)


(add-to-list 'auto-mode-alist '("\\.notes\\'" . org-mode))


;; TODO
;; "M-:" show minibuffer to eval expressions
;; set *scratch* buffer to lisp-interaction-mode
;; "M-m s s" 'consult-line should be case insensitive

(global-set-key (kbd "C-c C-s") 'save-buffer)

;; (let ((map global-map))
;;   (define-key map (kbd "M-<backspace>") #'sp-backward-kill-word)
;;   (define-key map (kbd "C-c C-k") #'eval-buffer)
;;   (define-key map (kbd "C-c M-c") #'upcase-word)
;;   (define-key map (kbd "C-x M-x") #'isearch-forward-symbol-at-point)
;;   (define-key map (kbd "C-x RET") #'magit-status)
;;   (define-key map (kbd "M-W") #'delete-trailing-whitespace))

(map! :map global-map
      "M-<backspace>" #'sp-backward-kill-word
      "C-c C-k" #'eval-buffer
      "C-c M-c" #'upcase-word
      "C-x M-x" #'isearch-forward-symbol-at-point
      "C-x RET" #'magit-status
      "M-W" #'delete-trailing-whitespace)

(map! :after consult
      :map general-override-mode-map
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
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p))
  (sp-pair "{" nil :unless '(:rem sp-point-before-word-p))
  (sp-pair "[" nil :unless '(:rem sp-point-before-word-p)))

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


(map! :after cider
      :map cider-mode-map
      "C-c M-c" #'cider-connect-clj
      "C-c C-k" #'cider-eval-buffer)

(map! :after clojure
      :map clojure-mode-map
      "C-c M-c" #'cider-connect-clj)

(with-eval-after-load 'general
  (define-key general-override-mode-map (kbd "C-c M-c") nil)
  ;; (define-key clojure-mode-map (kbd "C-c M-c") 'my-clojure-command)
  )

;; (after! clojure
;;   (define-key general-override-mode-map (kbd "C-c M-c") nil))

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

(use-package! cape
  :defer t
  :init
  (map! [remap dabbrev-expand] 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))


;; (use-package! corfu-history
;;   :after corfu
;;   :hook (corfu-mode . (lambda ()
;;                         (corfu-history-mode 1)
;;                         (savehist-mode 1)
;;                         (add-to-list 'savehist-additional-variables 'corfu-history))))

(use-package! corfu-quick
  ;;  corfu
  :bind (:map corfu-map
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert)))

(after! org-roam

  (setq org-roam-directory (file-truename "~/roam"))

  ;; add markdown extension to org-roam-file-extensions list
  (setq org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
  (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))

  (add-to-list 'load-path (file-truename "~/.emacs.d/.local/straight/repos/md-roam"))

  ;; Configs taken from the home repo
  ;; https://github.com/org-roam/org-roam#configuration
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package! flymake
  :config
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t))

(use-package! flymake-kondor
  :ensure t
  :hook (clojure-mode . flymake-kondor-setup))

(use-package! md-roam
  :config

  ;; (setq md-roam-file-extension-single "md")
  (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
  (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
  (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

  (add-to-list 'org-roam-capture-templates
               '("m" "Markdown" plain "" :target
                 (file+head "${slug}.md"
                            "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \n---\n")
                 :unnarrowed t))

  (with-eval-after-load 'markdown-mode
    (advice-add #'markdown-indent-line :before-until #'completion-at-point)))

(use-package! substitute
  :config

  ;; If you want a message reporting the matches that changed in the
  ;; given context.  We don't do it by default.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation)

  ;; We do not bind any keys.  This is just an idea.  The mnemonic is
  ;; that M-# (or M-S-3) is close to M-% (or M-S-5).
  (let ((map global-map))
    (define-key map (kbd "M-# s") #'substitute-target-below-point)
    (define-key map (kbd "M-# r") #'substitute-target-above-point)
    (define-key map (kbd "M-# d") #'substitute-target-in-defun)
    (define-key map (kbd "M-# b") #'substitute-target-in-buffer)))
