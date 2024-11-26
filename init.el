;; -*- no-byte-compile: t; lexical-binding: t; -*-

(package-initialize)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package emacs
  :ensure nil
  :custom
  (ad-redefinition-action 'accept)
  (auto-save-default t)
  (auto-save-include-big-deletions t)
  (backward-delete-char-untabify-method 'hungry)
  (bidi-inhibit-bpa t)
  (compile-command "make -j $(nproc)")
  (completion--cycle-threshold 3)
  (completion-ignore-case t)
  (create-lockfiles nil)
  (echo-keystrokes 0.02)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (eww-search-prefix "https://frogfind.com/?q=")
  (ffap-machine-p-known 'reject)
  (find-file-visit-truename t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (global-text-scale-adjust-resizes-frames nil)
  (history-length 25)
  (hscroll-margin 2)
  (hscroll-step 1)
  (idle-update-delay 1.0)
  (indent-tabs-mode t)
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (lazy-highlight-initial-delay 0)
  (load-prefer-newer t)
  (make-backup-files nil)
  (mouse-yank-at-point t)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (ring-bell-function 'ignore)
  (scroll-conservatively 10)
  (scroll-margin 0)
  (scroll-step 1)
  (show-paren-delay 0.1)
  (tab-always-indent 'complete)
  (tab-width 8)
  (use-dialog-box nil)
  (use-file-dialog 0)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (warning-supress-types '((lexical-binding)))
  (window-resize-pixelwise nil)
  :init
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (file-name-shadow-mode 1)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (set-default-coding-systems 'utf-8)
  (show-paren-mode 1)
  (advice-add 'display-startup-echo-area-message :override 'ignore)
  :config
  (load (expand-file-name "kernel.el" user-emacs-directory) :noerror :nomessage)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  ; (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'variable-pitch nil :family "DejaVu Serif")
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :hook
  (text-mode . turn-on-visual-line-mode)
  (text-mode . variable-pitch-mode)
  (prog-mode . toggle-truncate-lines))

(use-package no-littering
  :ensure t
  :demand t
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  (when (bound-and-true-p recentf-mode)
    (load recentf-save-file :noerror :nomessage))
  (when (bound-and-true-p savehist-mode)
    (load savehist-file :noerror :nomessage)))

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
  (evil-set-leader '(normal visual) (kbd ","))

  (evil-define-key '(insert visual) 'global (kbd "C-c") 'evil-force-normal-state)
  (evil-define-key '(normal visual) 'global (kbd "ñ") 'evil-ex)
  (evil-define-key 'normal dired-mode-map (kbd "DEL") 'dired-up-directory)
  (evil-define-key 'normal 'global
    (kbd "<leader> g") 'magit-status
    (kbd "<leader> r") 'query-replace
    (kbd "<leader> t") 'eshell
    (kbd "U") 'evil-redo
    (kbd "g b") 'ibuffer)

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
  :custom
  (evil-collection-want-find-usages-bindings t)
  :hook
  (minibuffer-setup . (lambda ()
			(local-set-key (kbd "C-j") 'exit-minibuffer)))
  (evil-mode . evil-collection-init))

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-limit 800000)
  (undo-outer-limit 120000000)
  (undo-strong-limit 12000000)
  (undo-tree-history-directory-alist
   '(("." . "~/.config/emacs/var/undo-cache")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package which-key
  :defer t
  :ensure t
  :hook
  (after-init . which-key-mode))

(use-package org
  :defer t
  :ensure nil
  :custom
  (org-ellipsis "")
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
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-AlhG --time-style=iso --color=auto")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package magit
  :defer t
  :ensure t
  :commands magit-status)

(use-package corfu
  :defer t
  :ensure t
  :hook
  (after-init . global-corfu-mode))

(use-package minions
  :defer t
  :ensure t
  :hook
  (after-init . minions-mode)
  :custom
  (minions-mode-line-lighter "∑m")
  :config
  (minions-mode 1))

(use-package standard-themes
  :if (display-graphic-p)
  :ensure t
  :custom
  (standard-dark-palette-overrides '((bg-main "#151515")))
  :config
  (standard-themes-load-dark)
  (global-set-key [f6] 'standard-themes-toggle))

(use-package markdown-mode
  :ensure t
  :defer t
  :custom
  (markdown-command "pandoc -s"))
