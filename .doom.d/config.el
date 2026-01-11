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


;; (setq doom-font (font-spec :family "PragmataPro" :size 16 :weight 'light)
;;       doom-variable-pitch-font (font-spec :family "PragmataPro" :size 14))
(setq doom-font (font-spec :family "PragmataPro Liga" :size 16 :weight 'light)
      doom-variable-pitch-font (font-spec :family "PragmataPro Liga" :size 14))
;; (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 16 :weight 'regular)
;;       doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 14))
;;
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Don't confirm when exiting Emacs
(setq confirm-kill-emacs nil)

;; Associate .notes files with org-mode (must be set before desktop restore)
(add-to-list 'auto-mode-alist '("\\.notes\\'" . org-mode))

;; Desktop save mode - restore previous session on relaunch
(desktop-save-mode 1)
(setq desktop-restore-frames t)           ; Restore frame configuration
(setq desktop-restore-in-current-display t)
(setq desktop-restore-eager 8)            ; Restore first 8 buffers immediately
(setq desktop-load-locked-desktop t)      ; Load desktop even if locked
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'log-edit-comment-ring)

;; Fix .notes files restored by desktop with wrong mode
(defun my/fix-notes-file-mode ()
  "Ensure .notes files are in org-mode after desktop restore."
  (when (and buffer-file-name
             (string-match-p "\\.notes\\'" buffer-file-name)
             (not (derived-mode-p 'org-mode)))
    (org-mode)))
(add-hook 'desktop-after-read-hook
          (lambda ()
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (my/fix-notes-file-mode)))))

;; Show all buffers (including ephemeral ones like *cider-repl*) in vertico buffer switcher
(after! consult
  ;; Include hidden buffers (those starting with space or asterisk) in buffer list
  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          consult--source-recent-file
          consult--source-file-register
          consult--source-bookmark
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden)))

;; Add starred/special buffers and recent files to workspace buffer switcher
;; This makes *doom*, *scratch*, *Messages*, *cider-repl*, etc. visible
(after! (:and vertico consult)
  (defvar +vertico-starred-buffer-source
    `(:name "Starred"
      :narrow ?*
      :category buffer
      :face consult-buffer
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :predicate
                 (lambda (buf)
                   (string-prefix-p "*" (buffer-name buf))))))
    "Consult source for starred buffers like *scratch*, *Messages*, etc.")

  (defvar +vertico-recent-file-source
    `(:name "Recent Files"
      :narrow ?r
      :category file
      :face consult-file
      :history file-name-history
      :state ,#'consult--file-state
      :items ,(lambda ()
                (mapcar #'abbreviate-file-name recentf-list)))
    "Consult source for recent files.")

  (defun +vertico--add-extra-sources (orig-fn)
    "Advice to add starred buffers and recent files to workspace buffer sources."
    (append (funcall orig-fn)
            (list +vertico-starred-buffer-source
                  +vertico-recent-file-source)))

  (advice-add '+vertico--workspace-generate-sources :around
              #'+vertico--add-extra-sources))

;; Fix keyboard input loss on macOS
;; Disable ns-auto-titlebar which causes keyboard focus issues on macOS 15.x
(after! ns-auto-titlebar
  (ns-auto-titlebar-mode -1))

;; macOS-specific keyboard focus fixes
(when (featurep 'ns)
  ;; Ensure Emacs properly handles keyboard focus
  (setq ns-pop-up-frames nil)
  ;; Fix for keyboard input being dropped
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-control-to-system nil)
  ;; Ensure proper event handling
  (setq ns-function-modifier 'hyper))

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
