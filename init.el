;; -*- lexical-binding: t -*-

;; Detectar si estoy en mi portátil
(defconst is-laptop (string= (system-name) "colmena"))

;; Recordar archivos más recientes
(recentf-mode 1)

;; Guardar la entrada del minibúfer
(setq history-length 25)
(savehist-mode 1)

;; Guardar la posición del cursor
(save-place-mode 1)

;; Guardar variables personalizadas en otro archivo
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Desactivar ventanas de diálogo gráficas
(setq use-dialog-box nil)

;; Recargar búfers cuando su contenido ha cambiado
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Abrir archivos de sólo lectura en view-mode
(setq view-read-only t)

;; Cargar versiones nuevas de los archivos
(setq load-prefer-newer t)

;; Desactivar copias de seguridad
(setq make-backup-files nil)

;; Mostrar teclas y pares rápido
(setq echo-keystrokes 0.02
      show-paren-delay 0)

;; Sobreescribir la región seleccionada
(delete-selection-mode 1)

;; Respetar tabulaciones
(setq backward-delete-char-untabify-method 'hungry)

;; Mostrar parejas
(show-paren-mode 1)

;; Autocompletar parejas
(electric-pair-mode 1)
(electric-quote-mode 1)

;; Completar mejor
(setq completion-styles '(substring basic emacs22))

;; Cambiar ventana con alt+flechas
(windmove-default-keybindings 'meta)

;; Hacer cosas interactivamente en el minibúfer
;; (fido-mode 1)

;; Esconder detalles en dired (se pueden mostrar con "(")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Eliminar búfer anterior al moverse entre carpetas
(setq dired-kill-when-opening-new-dired-buffer t)

;; Usar Ibuffer en vez de buffer-list
(defalias 'list-buffers 'ibuffer)

;; Confirmaciones abreviadas
(if (version<= emacs-version "28")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; Ignorar mayúsculas al buscar
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Desactivar avisos de algunos comandos
(progn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

;; Desactivar pitidos
(setq ring-bell-function 'ignore)

;; No usar tamaño de los carácteres
(setq frame-resize-pixelwise t)

;; Fuentes de prosa
(set-face-attribute 'variable-pitch nil :family "Noto Serif")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))

;; Abrir ediff en el mismo frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Símbolos bonitos
(defconst lisp--prettify-symbols-alist '(("lambda" . ?λ)))
(global-prettify-symbols-mode 1)

;; Agrupar mejor y borrar sin problema
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-display-summary nil
      ibuffer-saved-filter-groups `(("default"
                                     ("Dired" (mode . dired-mode))
                                     ("ERC" (mode . erc-mode))
                                     ("Emacs" (or (name . ,(rx bol "*scratch*" eol))
						  (name . ,(rx bol "*Messages*" eol))
						  (name . ,(rx bol "*GNU Emacs*" eol))
						  (name . ,(rx bol "*Async-native-compile-log*" eol)))))))

;; Usar grupo personalizado en Ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Usar utf-8
(set-default-coding-systems 'utf-8)

;; Combinaciones de teclas
(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-=") 'indent-region)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

;; Mostrar batería y hora en mi portátil
(when is-laptop
  (display-battery-mode 1)
  (display-time-mode 1)
  (setq display-time-format "%H:%M"))

;; Añadir MELPA e inicializar
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Preparar use-package y seguir compilando el fichero
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-compute-statistics t
	use-package-expand-minimally t))

;; Mantener el directorio de configuración limpio
(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  (when (bound-and-true-p recentf-mode) (load-file recentf-save-file))
  (when (bound-and-true-p savehist-mode) (load-file savehist-file)))

;; Cargar archivo personalizado
(load custom-file 'noerror 'nomessage)

;; Definiciones de funciones
(load-file (expand-file-name "my-lib.el" user-emacs-directory))

;; Colores rebuenos
(use-package modus-themes
  :defer t
  :commands load-theme
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-disable-other-themes t
        modus-themes-headings '((0 . (1.4))
                                (1 . (2.0))
                                (2 . (1.8))
                                (3 . (1.4))
                                (t . (1.2)))))

;; Otros colores chulos
(use-package standard-themes
  :defer t
  :commands load-theme
  :config
  (setq standard-themes-mixed-fonts t
        standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-disable-other-themes t
        standard-themes-headings '((0 . (1.4))
                                   (1 . (2.0))
                                   (2 . (1.8))
                                   (3 . (1.4))
                                   (t . (1.2)))))

(use-themes standard nil [f5])

;; Deshacer puro y duro
(use-package undo-fu
  :init
  (global-unset-key (kbd "C-z"))
  :bind (("C-z" . 'undo-fu-only-undo)
	 ("C-S-z" . 'undo-fu-only-redo)))

;; Ventanita de autocompletado
(use-package corfu
  :demand t
  :config
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3))

;; Ver combinaciones de teclas
(use-package which-key
  :config
  (which-key-mode 1))

;; Marcar saltos
(use-package pulsar
  :config
  (pulsar-global-mode 1))

;; Mostrar aciertos
(use-package anzu
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; Emular un terminal
(use-package eat
  :commands eat
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :config
  (setq eat-kill-buffer-on-exit t
	eat-enable-mouse t))

;; Cliente de IRC
(use-package erc
  :commands (erc erc-tls erc-ssl)
  :config
  (setq erc-kill-server-buffer-on-quit t
	erc-kill-buffer-on-part t
	erc-kill-queries-on-quit t)
  (require 'erc-dcc)
  (add-to-list 'erc-dcc-auto-masks "TNW!.*@.*"))

;; Cliente de git
(use-package magit
  :commands magit-status)

;; Interfaz para pass
(use-package pass
  :commands pass)

;; Complecion vertical
(use-package vertico
  :demand t
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-unobtrusive-mode 1)
  :bind (:map vertico-map
	      ("C-SPC" . vertico-my-toggle)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Información sobre funciones y variables
(use-package marginalia
  :after (:any vertico icomplete-vertical fido-vertical)
  :config
  (marginalia-mode 1))

;; Modo mejorado para PDFs
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq pdf-view-display-size 'fit-page))

;; Preparar org-mode
(use-package org
  :defer t
  :ensure nil
  :config
  (setq org-ellipsis "⬎"
	org-fold-catch-invisible-edits 'show-and-error
	org-fontify-todo-headline t
	org-fontify-whole-heading-line t
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-html-validation-link nil
	org-pretty-entities t
	org-return-follows-link t
	org-special-ctrl-a/e t
	org-startup-align-all-tables t
	org-startup-indented t
	image-use-external-converter t))

;; Retoques a Org mode
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda))
  :config
  (set-face-attribute 'org-modern-label nil :height 1.0)
  (setq org-modern-list '((43 . "‣") (45 . "–") (42 . "•"))
	org-modern-table nil))

;; Estilo C del kernel
(load-file (expand-file-name "kernel.el" user-emacs-directory))

;; tests
;; (setq inhibit-splash-screen t)
;; (list-bookmarks)
;; (switch-to-buffer "*Bookmark List*")
