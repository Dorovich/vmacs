;; -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package emacs
  :ensure nil
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (completion--cycle-threshold 3)
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
  :config
  (load-file (expand-file-name "kernel.el" user-emacs-directory))
  ;;(set-face-attribute 'default nil :height 160)
  (set-face-attribute 'variable-pitch nil :family "Noto Serif")
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
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
  (evil-set-leader '(normal visual) (kbd ","))

  (evil-define-key '(normal visual) 'global (kbd "ñ") 'evil-ex)
  (evil-define-key 'normal 'global (kbd "<leader> g") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader> r") 'query-replace)
  (evil-define-key 'normal 'global (kbd "<leader> t") 'eat)
  (evil-define-key 'normal 'global (kbd "U") 'evil-redo)
  (evil-define-key 'normal 'global (kbd "g b") 'list-buffers)

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
  (undo-limit 800000)
  (undo-outer-limit 120000000)
  (undo-strong-limit 12000000)
  (undo-tree-history-directory-alist
   '(("." . "~/.config/emacs/var/undo-cache")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

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
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package erc
  :ensure nil
  :defer t
  :commands (erc erc-tls erc-ssl)
  :custom
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-nick "vido25")
  (erc-system-name "vidonet")
  (erc-user-full-name "Vido")
  :config
  (add-to-list 'erc-dcc-auto-masks "TNW!.*@.*"))

(use-package magit
  :defer t
  :ensure t
  :commands magit-status)

(use-package corfu
  :defer t
  :ensure t
  :hook
  (after-init . global-corfu-mode))

(use-package eshell
  :defer t
  :ensure nil
  :custom
  (eshell-highlight-prompt t)
  :config
  (defun my-eshell-prompt-fn ()
    "Change the eshell prompt."
    (concat
     (if (string= (eshell/pwd) (getenv "HOME"))
	 "~"
       (eshell/basename (eshell/pwd)))
     " $ "))
  (setq eshell-prompt-function 'my-eshell-prompt-fn)
  
  (defun eshell/0file (file)
    "Obtain an url to a ‘file’, after uploading it to envs.sh."
    (shell-command (concat "curl -F\"file=@" file "\" https://envs.sh"))))

(use-package eat
  :defer t
  :ensure t
  :commands eat
  :hook
  ((eshell-load . eat-eshell-visual-command-mode)
   (eshell-load . eat-eshell-mode))
  :custom
  (eat-enable-mouse t)
  (eat-kill-buffer-on-exit t))

(use-package standard-themes
  :ensure t
  :custom
  (standard-dark-palette-overrides '((bg-main "#151515")))
  (standard-light-palette-overrides '((bg-main "#f7f7f7")
				      (fg-main "#191c27")))
  (standard-themes-bold-constructs t)
  (standard-themes-disable-other-themes t)
  (standard-themes-headings '((t . (1.4))))
  (standard-themes-italic-constructs t)
  (standard-themes-mixed-fonts t)
  :config
  (keymap-global-set "<f6>" 'standard-themes-toggle)
  (standard-themes-load-dark))
