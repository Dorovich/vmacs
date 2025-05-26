;; -*- no-byte-compile: t; lexical-binding: t; -*-

(defconst v/gui-p (display-graphic-p)
  "Whether Emacs is running in graphical mode or not.")

(defconst v/evil-p t
  "Whether Emacs should use Evil mode.")

(load (expand-file-name "default.el" user-emacs-directory) t t)
(load (expand-file-name "kernel.el" user-emacs-directory) t t)

(eval-when-compile
  (package-initialize)
  (require 'use-package))

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (auto-save-include-big-deletions t)
  (backward-delete-char-untabify-method 'hungry)
  (comment-multi-line t)
  (compile-command "make -j $(nproc)")
  (completion-cycle-threshold 3)
  (create-lockfiles nil)
  (find-file-visit-truename t)
  (frame-resize-pixelwise v/gui-p)
  (ibuffer-expert t)
  (inhibit-startup-message t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (mouse-yank-at-point t)
  (pixel-scroll-precision-mode v/gui-p)
  (pixel-scroll-precision-use-momentum nil)
  (tab-always-indent 'complete)
  (warning-minimum-level :emergency)
  (warning-supress-types '((lexical-binding)))
  :init
  (advice-add 'display-startup-echo-area-message :override 'ignore)
  (recentf-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  :config
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (file-name-shadow-mode 1)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; (set-face-attribute 'variable-pitch nil :family "DejaVu Serif")
  (show-paren-mode 1)
  (unless v/gui-p
    (xterm-mouse-mode 1))
  :hook
  (after-init . global-auto-revert-mode)
  (text-mode . turn-on-visual-line-mode)
  ;; (text-mode . variable-pitch-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . toggle-truncate-lines))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-AlhG --time-style=iso --color=auto")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package org
  :defer t
  :ensure nil
  :custom
  (image-use-external-converter t)
  (org-ellipsis "")
  (org-export-allow-bind-keywords t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)
  (org-html-head-include-default-style nil)
  (org-html-htmlize-output-type 'css)
  (org-html-validation-link nil)
  (org-latex-caption-above nil)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-startup-align-all-tables t)
  (org-startup-indented nil))

(use-package no-littering
  :ensure t
  :demand t
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  (when (bound-and-true-p recentf-mode)
    (load recentf-save-file t t))
  (when (bound-and-true-p savehist-mode)
    (load savehist-file t t)))

(use-package magit
  :defer t
  :ensure t
  :commands magit-status)

(use-package evil
  :if v/evil-p
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
  :after evil
  :defer t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  :hook
  (minibuffer-setup . (lambda () (local-set-key (kbd "C-j") 'exit-minibuffer)))
  (evil-mode . evil-collection-init))

(use-package evil-surround
  :after evil
  :defer t
  :ensure t
  :hook
  (evil-mode . global-evil-surround-mode))

(use-package evil-numbers
  :after evil
  :ensure t
  :config
  (evil-define-key '(normal visual) 'global (kbd "g +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g -") 'evil-numbers/dec-at-pt))

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist '(("." . "~/.config/emacs/var/undo-cache")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package corfu
  :if v/gui-p
  :defer t
  :ensure t
  :hook
  (after-init . global-corfu-mode))
