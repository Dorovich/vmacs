(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package emacs
  :ensure nil
  :custom
  (completion-ignore-case t)
  (echo-keystrokes 0.02)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (eww-search-prefix "https://frogfind.com/?q=")
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (load-prefer-newer t)
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (ring-bell-function 'ignore)
  (show-paren-delay 0)
  (tab-always-indent 'complete)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (backward-delete-char-untabify-method 'hungry)
  :init
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (file-name-shadow-mode 1)
  (global-auto-revert-mode 1)
  (menu-bar-mode -1)
  (recentf-mode 1)
  (save-place-mode 1)


  (savehist-mode 1)
  (scroll-bar-mode -1)
  (set-default-coding-systems 'utf-8)
  (set-face-attribute 'default nil :height 160)
  (show-paren-mode 1)
  (tool-bar-mode -1)
  :config
  (load-file (expand-file-name "kernel.el" user-emacs-directory))
  :hook
  (text-mode . turn-on-visual-line-mode)
  (text-mode . variable-pitch-mode))

(use-package no-littering
  :ensure t
  :demand t
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  (when (bound-and-true-p recentf-mode)
    (load-file recentf-save-file))
  (when (bound-and-true-p savehist-mode)
    (load-file savehist-file)))

(use-package evil
  :ensure t
  :defer t
  :hook
  (after-init . evil-mode)
  :custom
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)

  (evil-define-key '(normal visual)'global (kbd "ñ") 'evil-ex)

  (evil-define-key 'normal 'global (kbd "gcc")
    (lambda ()
      (interactive)
      (if (not (use-region-p))
	  (comment-or-uncomment-region
	   (line-beginning-position)
	   (line-end-position)))))

  (evil-define-key 'visual 'global (kbd "gc")
    (lambda ()
      (interactive)
      (if (use-region-p)
	  (comment-or-uncomment-region
	   (region-beginning)
	   (region-end))))))

(use-package evil-collection
  :defer t
  :ensure t
  :hook
  (minibuffer-setup . (lambda () (local-set-key (kbd "C-j") 'exit-minibuffer)))
  (evil-mode . evil-collection-init))

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
	undo-tree-visualizer-diff t
	undo-limit 800000
	undo-strong-limit 12000000
	undo-outer-limit 120000000)
  :config
  (setq undo-tree-history-directory-alist
	'(("." . "~/.config/emacs/var/undo-cache"))))

(use-package org
  :defer t
  :ensure nil
  :custom
  (org-ellipsis "") ; "⬎"
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-html-head-include-default-style nil)
  (org-html-htmlize-output-type 'css)
  (org-html-validation-link nil)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-startup-align-all-tables t)
  (org-startup-indented t)
  (image-use-external-converter t))

(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "DEL") 'dired-up-directory))

(use-package magit
  :ensure t
  :commands magit-status)
