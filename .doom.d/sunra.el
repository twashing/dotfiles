(remove-hook 'after-change-major-mode-hook
             #'doom-highlight-non-default-indentation-h)

(remove-hook 'doom-first-buffer-hook
             #'global-whitespace-mode)

(defun sunra/do-before-after-init ()
  "Function to run before anything during Doom initialization."

  (global-whitespace-mode -1)
  (flycheck-mode -1))

(add-hook 'doom-before-modules-init-hook #'sunra/do-before-after-init)
(add-hook 'doom-after-modules-init-hook #'sunra/do-before-after-init)

(map! :map general-override-mode-map
      "M-m s o" #'consult-outline)

(desktop-save-mode 1)

(add-to-list 'desktop-globals-to-save 'log-edit-comment-ring)
(add-to-list 'desktop-globals-to-save 'kmacro-ring)
(add-to-list 'desktop-globals-to-save 'kill-ring)

;; (use-package! org-make-toc
;;   :ensure t)

(map! :map general-override-mode-map
      "C-c l e m" #'pp-macro-expand-last-expression
      "C-c l e D" #'eval-defun-at-point)

;; (map! :map general-override-mode-map
;;       "C-M a" #'embark-act
;;       "C-M e" #'embark-export
;;       "C-M c" #'embark-collect)

(map! :map general-override-mode-map
      "C-x <up>" #'pop-global-mark
      "C-x <down>" #'consult-global-mark)

;; (map! :map general-override-mode-map
;;       "C-c o e" #'+eshell/here
;;       "C-c o E" #'+eshell/toggle)

(map! :map general-override-mode-map
      "C-M-<" #'append-to-buffer)

(map! :map general-override-mode-map
      "C->" #'avy-goto-char-timer
      "C-M->" #'avy-goto-char-2)

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

 ;; Start from 1 when inserting numbers
 mc/insert-numbers-default 1

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

(flycheck-mode -1)

(add-to-list 'auto-mode-alist '("\\.notes\\'" . org-mode))

(global-set-key (kbd "C-c C-s") 'save-buffer)

(map! :map global-map
      "M-<backspace>" #'sp-backward-kill-word
      "C-c C-k" #'eval-buffer
      "C-c M-c" #'upcase-word
      "C-x M-x" #'isearch-forward-symbol-at-point
      "C-x RET" #'magit-status
      "M-W" #'delete-trailing-whitespace
      "C-/" #'org-cycle-global)

(map! :map general-override-mode-map
      "M-m p p" #'projectile-switch-project
      "M-m p f" #'projectile-find-file
      "M-m p r" #'projectile-replace
      "M-m p R" #'projectile-replace-regexp
      "M-m p S" #'projectile-save-project-buffers)

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

(use-package! free-keys)

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
      "M-r" #'sp-raise-sexp
      "DEL" #'sp-backward-delete-char)

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
      "C-M-l" #'transpose-lines)

;; (map! "C-c m N l" #'mc/mark-next-lines
;;       "C-c m N t" #'mc/mark-next-like-this
;;       "C-c m N w" #'mc/mark-next-like-this-word
;;       "C-c m N W" #'mc/mark-next-word-like-this
;;       "C-c m N s" #'mc/mark-next-like-this-symbol
;;       "C-c m N S" #'mc/mark-next-symbol-like-this
;;       "C-c m P l" #'mc/mark-previous-lines
;;
;;       "C-c s n" #'mc/skip-to-next-like-this
;;       "C-c s p" #'mc/skip-to-previous-like-this
;;       "C-c m i n" #'mc/insert-numbers
;;
;;       "C-c m a t" #'mc/mark-all-like-this
;;       "C-c m a w" #'mc/mark-all-words-like-this
;;       "C-c m a s" #'mc/mark-all-symbols-like-this
;;       "C-c m a r" #'mc/mark-all-in-region
;;       "C-c m a x" #'mc/mark-all-in-region-regexp
;;       "C-c m a d" #'mc/mark-all-like-this-dwim
;;       "C-c m a D" #'mc/mark-all-dwim
;;
;;       "C-c m e l" #'mc/edit-lines
;;       "C-c m e b" #'mc/edit-beginnings-of-lines
;;       "C-c m e e" #'mc/edit-ends-of-lines)
;;
;; (map! "C-z" #'repeat)
;;
;; (map! "C-o" #'hs-toggle-hiding
;;       "C-c @ C-M-h" #'hs-hide-all
;;       "C-c @ C-M-s" #'hs-show-all
;;       "C-c @ C-M-l" #'hs-hide-level
;;       "C-M-," #'hs-hide-all
;;       "C-M-." #'hs-show-all
;;       "C-M-/" #'hs-hide-level)
;;
;; (after! cider
;;
;;   ;; DONT open new window on cider-connect, et al
;;   (setq cider-repl-pop-to-buffer-on-connect nil)
;;   (setq cider-auto-select-test-report-buffer nil)
;;   (setq cider-auto-select-error-buffer nil)
;;   (setq cider-show-error-buffer nil))
;;
;; (map! :after cider
;;       :map general-override-mode-map
;;       "C-c M-c" #'cider-connect-clj
;;       "C-c C-k" #'cider-eval-buffer)
;;
;; (map! :after clojure
;;       :map general-override-mode-map
;;       "C-c M-c" #'cider-connect-clj)
;;
;; (with-eval-after-load 'general
;;   (define-key general-override-mode-map (kbd "C-c M-c") nil))
;;
;; (defun delete-whitespace-except-one ()
;;   (interactive)
;;   (just-one-space -1))
;;
;; (map! "C-M-SPC" #'delete-whitespace-except-one
;;       "C-," #'+default/newline-above
;;       "C-." #'+default/newline-below)
;;
;; (defun copy-line (&optional arg)
;;   "Do a kill-line but copy rather than kill.  This function directly calls
;;   kill-line, so see documentation of kill-line for how to use it including prefix
;;   argument and relevant variables.  This function works by temporarily making the
;;   buffer read-only."
;;   (interactive "P")
;;   (let ((buffer-read-only t)
;;         (kill-read-only-ok t))
;;     (kill-line arg)))
;;
;; (map! "C-c k" #'copy-line
;;       "C-c K" #'avy-copy-line)
;;
;; (require 'cl-lib)
;;
;; (defun zipmap (keys values)
;;   (cl-pairlis keys values))
;;
;; (defun sunra/avy-read-process-window-in-list (list)
;;   (mapcar
;;    (lambda (triplet)
;;      (let ((first (nth 0 triplet))
;;            (last (nth 2 triplet))
;;            line-number
;;            substring
;;            buffer
;;            selection-candidate)
;;
;;        (save-window-excursion
;;          (select-window last)
;;          (goto-char first)
;;
;;          (setq line-number (line-number-at-pos))
;;          (setq substring (buffer-substring-no-properties first (nth 1 triplet)))
;;          (setq buffer (window-buffer last))
;;          (setq selection-candidate (format "%d %s %s" line-number substring buffer)))
;;
;;        (list selection-candidate line-number substring buffer)))
;;    list))
;;
;; (defun sunra/avy-read-candidates-prompt (candidates)
;;
;;   (let* ((cans (sunra/avy-read-process-window-in-list candidates))
;;          (hashes (mapcar (lambda (c)
;;                            (secure-hash 'sha1 (car c)))
;;                          cans))
;;          (cadidates-selections-hash (zipmap hashes candidates))
;;
;;          ;; Take selection, get hash, compare
;;          (the-selection (completing-read "Select a match: " (mapcar #'car cans)))
;;          (the-selection-hash (secure-hash 'sha1 the-selection)))
;;
;;     (alist-get the-selection-hash cadidates-selections-hash nil nil #'string=)))
;;
;; (defun sunra/avy-read-candidates-return ()
;;
;;   ;; Read candidates from User prompt
;;   (let* ((candidates (avy--read-candidates))
;;          (flat-cands (mapcar #'flatten-list candidates)))
;;
;;     ;; Conditionally narrow candidates if many, or select the one
;;     (if (> (length flat-cands) 1)
;;        (sunra/avy-read-candidates-prompt flat-cands)
;;       (car flat-cands))))
;;
;; (defun sunra/copy-remote-region ()
;;   (interactive)
;;
;;   ;; Make avy wait a (practically) infinate amount of time
;;   (let ((avy-timeout-seconds most-positive-fixnum))
;;
;;     (let* ((triplet-start (sunra/avy-read-candidates-return))
;;            (candidate-start-position-start (nth 0 triplet-start))
;;            (window (nth 2 triplet-start))
;;
;;            (triplet-end (sunra/avy-read-candidates-return))
;;            (candidate-end-position-end (nth 1 triplet-end)))
;;
;;       (save-window-excursion
;;
;;         (select-window window)
;;
;;         (kill-new
;;          (buffer-substring-no-properties
;;           candidate-start-position-start
;;           candidate-end-position-end))))))
;;
;; (after! vertico
;;
;;   (vertico-buffer-mode)
;;   (setq completion-styles '(orderless basic)))
;;
;; ;; (use-package! corfu
;; ;;
;; ;;   ;; Optional customizations
;; ;;   :custom
;; ;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;; ;;   (corfu-auto t)                 ;; Enable auto completion
;; ;;   (corfu-separator ?\s)          ;; Orderless field separator
;; ;;   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; ;;   (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;; ;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;; ;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;; ;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; ;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;; ;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;; ;;
;; ;;   ;; Enable Corfu only for certain modes.
;; ;;   :hook ((prog-mode . corfu-mode)
;; ;;          (org-mode . corfu-mode)
;; ;;          (shell-mode . corfu-mode)
;; ;;          (eshell-mode . corfu-mode))
;; ;;
;; ;;   ;; Recommended: Enable Corfu globally.
;; ;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;; ;;   ;; See also `corfu-excluded-modes'.
;; ;;   :init
;; ;;   (global-corfu-mode))
;;
;; ;; A few more useful configurations...
;; (use-package! emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   (setq completion-cycle-threshold 3)
;;
;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)
;;
;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (setq tab-always-indent 'complete))
;;
;; (use-package! cape
;;   :defer t
;;   :init
;;   (map! [remap dabbrev-expand] 'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
;;
;;
;; ;; (use-package! corfu-history
;; ;;   :after corfu
;; ;;   :hook (corfu-mode . (lambda ()
;; ;;                         (corfu-history-mode 1)
;; ;;                         (savehist-mode 1)
;; ;;                         (add-to-list 'savehist-additional-variables 'corfu-history))))
;;
;; (use-package! corfu-quick
;;   ;; :after corfu
;;   :bind (:map corfu-map
;;          ("M-q" . corfu-quick-complete)
;;          ("C-q" . corfu-quick-insert)))
;;
;; ;; (use-package! flymake
;; ;;   :config
;; ;;   (setq flymake-start-on-flymake-mode t)
;; ;;   (setq flymake-no-changes-timeout nil)
;; ;;   (setq flymake-start-on-save-buffer t))
;; ;;
;; ;; (use-package! flymake-kondor
;; ;;   :hook (clojure-mode . flymake-kondor-setup))
;;
;; (map! :map general-override-mode-map
;;       "C-x b" #'consult-buffer
;;       "M-m s s" #'consult-line
;;       "M-m s S" #'consult-line-multi
;;       "M-y" #'consult-yank-from-kill-ring)
;;
;; (use-package! embark-consult)
;;
;; (after! denote
;;
;;   (use-package! denote
;;     :config
;;     (setq denote-directory (expand-file-name "~/Projects/notes/")
;;           denote-known-keywords '("emacs" "clojure" "guix")
;;           denote-infer-keywords t
;;           denote-sort-keywords t
;;           denote-file-type 'markdown-yaml ; Org is the default, set others here
;;           denote-prompts '(title keywords)
;;           denote-excluded-directories-regexp nil
;;           denote-excluded-keywords-regexp nil)))
;;
;; ;; (after! org-roam
;; ;;
;; ;;   (setq org-roam-directory (file-truename "~/roam"))
;; ;;
;; ;;   ;; add markdown extension to org-roam-file-extensions list
;; ;;   (setq org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
;; ;;   (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
;; ;;
;; ;;   (add-to-list 'load-path (file-truename "~/.emacs.d/.local/straight/repos/md-roam"))
;; ;;
;; ;;   ;; Configs taken from the home repo
;; ;;   ;; https://github.com/org-roam/org-roam#configuration
;; ;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; ;;   )
;;
;; ;; (use-package! md-roam
;; ;;  :config
;; ;;
;; ;;  ;; (setq md-roam-file-extension-single "md")
;; ;;  (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
;; ;;  (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
;; ;;  (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active
;; ;;
;; ;;  (add-to-list 'org-roam-capture-templates
;; ;;               '("m" "Markdown" plain "" :target
;; ;;                 (file+head "${slug}.md"
;; ;;                            "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \n---\n")
;; ;;                 :unnarrowed t))
;; ;;
;; ;;  (with-eval-after-load 'markdown-mode
;; ;;   (advice-add #'markdown-indent-line :before-until #'completion-at-point)))
;;
;; (defun apply-template (template-file output-file &optional context)
;;   "Apply a template file TEMPLATE-FILE and write the result to OUTPUT-FILE.
;;    Replaces placeholders of the form:
;;    - {file:FILENAME} with the contents of FILENAME (resolved relative to TEMPLATE-FILE's directory)
;;    - {var:VARNAME} with the value associated with VARNAME in CONTEXT (an alist), or an empty string if CONTEXT is nil.
;;
;;    Example usage:
;;    Assuming your project directory contains:
;;    - \"Content.md\" with contents: \"Bar\"
;;    - \"A.md.tmpl\" with contents:
;;         Foo {file:Content.md}
;;         {var:thing}
;;
;;    And you want to create \"A.md\" with the inserted text.
;;    You can call with or without a context of variables
;;
;;    (apply-template \"path/to/project-directory/A.md.tmpl\"
;;                    \"path/to/project-directory/A.md\"
;;                    '((\"thing\" . \"querty\")))
;;
;;    (apply-template \"path/to/project-directory/A.md.tmpl\"
;;                    \"path/to/project-directory/A.md\")"
;;
;;   (let ((project-dir (file-name-directory template-file)))
;;
;;     (with-temp-buffer
;;
;;       ;; Read template into a string.
;;       (insert-file-contents template-file)
;;       (let ((template (buffer-string)))
;;
;;         ;; Replace {file:...} placeholders.
;;         (setq template
;;               (replace-regexp-in-string
;;                "{file:\\([^}]+\\)}"
;;                (lambda (match)
;;                  ;; Obtain the file name from the match.
;;                  (let* ((raw-filename (match-string 1 match))
;;                         ;; Remove any extraneous escape characters if needed.
;;                         (filename (replace-regexp-in-string "\\\\" "" raw-filename))
;;                         (full-path (expand-file-name filename project-dir)))
;;                    (with-temp-buffer
;;                      (condition-case err
;;                          (progn
;;                            (insert-file-contents full-path)
;;                            (buffer-string))
;;                        (error (format "[Error reading file: %s]" full-path))))))
;;                template t t))
;;
;;         ;; Replace {var:...} placeholders.
;;         (setq template
;;               (replace-regexp-in-string
;;                "{var:\\([^}]+\\)}"
;;                (lambda (match)
;;                  (if (string-match "{var:\\([^}]+\\)}" match)
;;                      (if context
;;                          (or (cdr (assoc (match-string 1 match) context)) "")
;;                        "")
;;                    match))
;;                template t t))
;;
;;         ;; Write the resulting string to OUTPUT-FILE.
;;         (with-temp-file output-file
;;           (insert template))))))
;;
;; (defun apply-templates (dir &optional context)
;;   "Process all .tmpl files in DIR.
;;    For each file with a .tmpl suffix, create an output file by removing the .tmpl suffix.
;;    If CONTEXT (an alist) is provided, it is passed to `apply-template` for variable substitutions.
;;
;;    Example usage:
;;    Suppose you have a directory \"path/to/project-directory\" with:
;;      - \"A.md.tmpl\"
;;      - \"main.c.tmpl\"
;;    And you want to process these templates with a context:
;;
;;    (apply-templates \"gptel/directives\"
;;                     '((\"thing\" . \"querty\")))"
;;
;;   (dolist (tmpl-file (directory-files dir t "\\.tmpl$"))
;;
;;     (when (file-regular-p tmpl-file)
;;       (let* ((output-file (replace-regexp-in-string "\\.tmpl$" "" tmpl-file)))
;;         (apply-template tmpl-file output-file context)
;;         (message "Processed template: %s -> %s" tmpl-file output-file)))))
;;
;; (defun load-gptel-directives (dir)
;;   "Load all directive files from DIR into gptel-directives.
;;    Newer directives override existing ones with the same key."
;;   (let* ((files (directory-files dir t "\\.md$"))
;;          (new-pairs (mapcar (lambda (file)
;;                               (cons
;;                                (intern (file-name-base file))
;;                                (with-temp-buffer
;;                                  (insert-file-contents file)
;;                                  (buffer-string))))
;;                             files))
;;          (existing-keys (mapcar #'car gptel-directives))
;;          (filtered-old (cl-remove-if (lambda (pair)
;;                                        (member (car pair) (mapcar #'car new-pairs)))
;;                                      gptel-directives)))
;;     (setq gptel-directives
;;           (append new-pairs filtered-old))))
;;
;; (defun load-all! (dir)
;;   "Load all .el files from DIR"
;;   (dolist (file (directory-files dir t "\\.el$"))
;;     (load! file)))
;;
;; (use-package! gptel
;;
;;   :bind ("C-M-'" . gptel-send)
;;   :config
;;
;;   (load! "openapi-key.el")
;;   (load! "gemini-key.el")
;;   (load! "anthropic-key.el")
;;   (load! "linkup-api-key.el")
;;
;;   (apply-templates (file-name-concat (dir!) "gptel/directives"))
;;   (load-gptel-directives (file-name-concat (dir!) "gptel/directives"))
;;   (load-all! (file-name-concat (dir!) "gptel/tools/"))
;;
;;   (setq! gptel-api-key openapi-key
;;          gptel-expert-commands t
;;          gptel-prompt-prefix-alist '((markdown-mode . "*Prompt* ")
;;                                      (org-mode . "*Prompt* ")
;;                                      (text-mode . "*Prompt*  "))
;;          gptel-response-prefix-alist '((markdown-mode . "*Response* ")
;;                                        (org-mode . "*Response* ")
;;                                        (text-mode . "*Response* ")))
;;
;;   ;; :key can be a function that returns the API key.
;;   ;; Any name you want
;;   ;; Streaming responses
;;   (gptel-make-gemini "Gemini"
;;     :key gemini-key
;;     :stream t)
;;   (gptel-make-anthropic "Claude"
;;     :key anthropic-key
;;     :stream t)
;;
;;   ;; NOTE keep this until moving back to `main' branch
;;   (setq gptel--anthropic-models
;;         (cons '(claude-3-7-sonnet-20250219
;;                 :description "Hybrid model capable of standard thinking and extended thinking modes"
;;                 :capabilities (media tool-use cache)
;;                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
;;                 :context-window 200
;;                 :input-cost 3
;;                 :output-cost 15
;;                 :cutoff-date "2025-02")
;;               gptel--anthropic-models)))
;;
;; (use-package! gptel-quick
;;
;;   :bind (:map embark-general-map
;;               ("?" . #'gptel-quick)))
;;
;; (defun sunra/goto-emacs-dir ()
;;   "Open my ~/.emacs.d directory."
;;   (interactive)
;;   (dired doom-emacs-dir))
;;
;; (defun sunra/goto-private-config-sunra-el ()
;;   "Open my private config.el file."
;;   (interactive)
;;   (find-file (expand-file-name "sunra.el" doom-user-dir)))
;;
;; (defun sunra/goto-private-config-sunra-org ()
;;   "Open my private Config.org file."
;;   (interactive)
;;   (find-file (expand-file-name "SUNRA.org" doom-user-dir)))
;;
;; (let ((map global-map))
;;   (define-key map (kbd "C-h d e") #'sunra/goto-emacs-dir)
;;   (define-key map (kbd "C-h d r") #'sunra/goto-private-config-sunra-el)
;;   (define-key map (kbd "C-h d R") #'sunra/goto-private-config-sunra-org))
;;
;; (package! transient
;;       :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
;;       :recipe (:host github :repo "magit/transient"))
;;
;; (package! with-editor
;;           :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
;;           :recipe (:host github :repo "magit/with-editor"))
;;
;; (require 'cl-lib)
;;
;; (defun screenshot (&optional emacs-frame-only)
;;   "Take a screenshot and save it to a file.
;; With prefix argument ARG, capture the current active Emacs frame only.
;; Otherwise, capture the entire screen."
;;   (interactive "P")
;;   (let* ((default-directory (expand-file-name "~/Pictures/"))
;;          (timestamp (format-time-string "%Y%m%d-%H%M%S"))
;;          (file-name (concat "screenshot-" timestamp ".png"))
;;          (full-path (expand-file-name file-name))
;;          (command
;;           (cond
;;            ((eq system-type 'darwin)
;;             (if emacs-frame-only
;;                 (format "screencapture -l$(osascript -e 'tell app \"Emacs\" to id of window 1') %s"
;;                         (shell-quote-argument full-path))
;;               (format "screencapture %s"
;;                       (shell-quote-argument full-path))))
;;            ((eq system-type 'gnu/linux)
;;             (if emacs-frame-only
;;                 (format "ffmpeg -y -f x11grab -i 0x%s -vframes 1 %s"
;;                         (frame-parameter nil 'outer-window-id)
;;                         (shell-quote-argument full-path))
;;               (format "ffmpeg -y -f x11grab -i :0 -vframes 1 %s"
;;                       (shell-quote-argument full-path))))
;;            (t (error "No suitable screenshot command found for this system")))))
;;     (make-directory (file-name-directory full-path) t)
;;     (if (zerop (shell-command command))
;;         (progn
;;           (message "Screenshot saved to %s" full-path)
;;           (when (y-or-n-p "Open screenshot? ")
;;             (find-file full-path)))
;;       (error "Failed to take screenshot"))))
;;
;;
;; ;; `screenshot` is an Emacs lisp function that can
;; ;; i. take a shot of desktop screen or ii. shot of an active Emacs window
;; ;; using the CLI =Image Magick=, or ~screencapture~ on MacOS
;; ;;
;; ;; Usage
;; ;; ;; (setq debug-on-error t)
;; ;; ;; (toggle-debug-on-error)
;;
;; ;; (screenshot)
;; ;; (screenshot t)
;;
;; (defun screencapture (screen-id &optional duration)
;;   "Record the screen with SCREEN-ID for DURATION seconds using ffmpeg’s avfoundation input.
;;    SCREEN-ID is the device index shown when running:
;;    ffmpeg -f avfoundation -list_devices true -i \"\"
;;
;; By default, DURATION is 5 seconds.  The resulting file is placed in ~/Pictures/ with a timestamp.
;;
;; On non-macOS systems, this function currently signals an error."
;;   (interactive "nEnter screen device index: \nnDuration in seconds (default 5): ")
;;   (unless duration
;;     (setq duration 5))
;;
;;   (let* ((default-directory (expand-file-name "~/Pictures/"))
;;          (timestamp (format-time-string "%Y%m%d-%H%M%S"))
;;          (file-name (concat "screencapture-" timestamp ".mp4"))
;;          (full-path (expand-file-name file-name))
;;          ;; Construct the ffmpeg command for avfoundation
;;          (command (format "ffmpeg -y -f avfoundation -i %s:0 -t %s %s"
;;                           screen-id
;;                           duration
;;                           (shell-quote-argument full-path))))
;;     (make-directory (file-name-directory full-path) t)
;;     (if (zerop (shell-command command))
;;         (progn
;;           (message "Screen recording saved to %s" full-path))
;;       (error "Failed to record from screen ID %s" screen-id))))
;;
;; ;; Usage
;;
;; ;; Record the desktop from screen 3 for 10 seconds
;; ;; (screencapture 3 10)
;; ;;
;; ;; Record the desktop from screen 2 for 5 seconds
;; ;; (screencapture 2 5)
;;
;; ;; # ffmpeg can list out the available desktop screen devices
;; ;; ffmpeg -f avfoundation -list_devices true -i ""
;; ;; ...
;; ;; [AVFoundation indev @ 0x12a804080] AVFoundation video devices:
;; ;; [AVFoundation indev @ 0x12a804080] [0] LG UltraFine Display Camera
;; ;; [AVFoundation indev @ 0x12a804080] [1] FaceTime HD Camera
;; ;; [AVFoundation indev @ 0x12a804080] [2] Capture screen 0
;; ;; [AVFoundation indev @ 0x12a804080] [3] Capture screen 1
;; ;; [AVFoundation indev @ 0x12a804080] AVFoundation audio devices:
;; ;; [AVFoundation indev @ 0x12a804080] [0] LG UltraFine Display Audio
;; ;; [AVFoundation indev @ 0x12a804080] [1] MacBook Pro Microphone
;; ;; [AVFoundation indev @ 0x12a804080] [2] Bose QC45
;;
;; (use-package! plz)
;;
;; (defun websearch (query &optional api-key &optional depth)
;;   "Search Linkup API with QUERY using the corresponding API-KEY.
;;    Optional DEPTH parameter can be 'deep or 'shallow (defaults to deep).
;;    Returns response as parsed JSON or signals an error."
;;   (condition-case err
;;       (let* ((depth-value (or depth "deep"))
;;              (linkup-api-key (or api-key linkup-api-key))
;;              (json-object (json-encode
;;                            `(("q" . ,query)
;;                              ("depth" . ,depth-value)
;;                              ("outputType" . "sourcedAnswer")
;;                              ("includeImages" . "false"))))
;;              (url-request-method "POST")
;;              (url-request-extra-headers
;;               `(("Authorization" . ,(concat "Bearer " linkup-api-key))
;;                 ("Content-Type" . "application/json")))
;;              (url-request-data json-object)
;;              (buffer (url-retrieve-synchronously
;;                       "https://api.linkup.so/v1/search" t)))
;;         (if buffer
;;             (with-current-buffer buffer
;;               (goto-char url-http-end-of-headers)
;;               (let ((json-response (json-read)))
;;                 (kill-buffer buffer)
;;                 (if (and (listp json-response) json-response)
;;                     json-response
;;                   (error "Invalid response format from Linkup API"))))
;;           (error "No response from Linkup API")))
;;
;;     (json-error
;;      (error "Failed to parse Linkup API response: %s" (error-message-string err)))
;;
;;     (error
;;      (error "Linkup search error: %s" (error-message-string err)))))
;;
;; ;; ;; Example usage:
;; ;; (websearch "What is Microsoft's 2024 revenue?")
;; ;; (websearch "What is Microsoft's 2024 revenue?"
;; ;;            "7c913f85-698f-487f-a95b-a5dc949f50f3")
;; ;; (websearch "What is Microsoft's 2024 revenue?"
;; ;;            "7c913f85-698f-487f-a95b-a5dc949f50f3"
;; ;;            "deep")
;;
;; (defun prot-org--id-get (&optional pom create prefix)
;;   "Get the CUSTOM_ID property of the entry at point-or-marker POM.
;;
;; If POM is nil, refer to the entry at point.
;; If the entry does not have an CUSTOM_ID, the function returns nil. However, when
;; CREATE is non nil, create a CUSTOM_ID if none is present already.
;; PREFIX will be passed through to `org-id-new'.  In any case, the
;; CUSTOM_ID of the entry is returned."
;;   (org-with-point-at pom
;;     (let ((id (org-entry-get nil "CUSTOM_ID")))
;;       (cond
;;        ((and id (stringp id) (string-match "\\S-" id))
;;         id)
;;        (create
;;         (setq id (org-id-new (concat prefix "h")))
;;         (org-entry-put pom "CUSTOM_ID" id)
;;         (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
;;         id)))))
;;
;; (declare-function org-map-entries "org")
;;
;; ;;;###autoload
;; (defun prot-org-id-headlines ()
;;   "Add missing CUSTOM_ID to all headlines in current file."
;;   (interactive)
;;   (org-map-entries
;;    (lambda () (prot-org--id-get (point) t))))
;;
;; (setq org-structure-template-alist
;;         '(("s" . "src")
;;           ("E" . "src emacs-lisp")
;;           ("e" . "example")
;;           ("q" . "quote")
;;           ("v" . "verse")
;;           ("V" . "verbatim")
;;           ("c" . "center")
;;           ("C" . "comment")))
