;; -*- lexical-binding: t -*-

;; Definiciones de funciones
(load-file (expand-file-name "my-lib.el" user-emacs-directory))

;; Identifíquese
(setq user-full-name "Dorovich"
      user-mail-address "silverdovi25@gmail.com")

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

;; Buscador para eww
(setq eww-search-prefix "https://frogfind.com/?q=")

;; Hacer cosas interactivamente en el minibúfer
;; (fido-mode 1)

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

;; Fuente más grande para el portátil
(when in-bsc-laptop-p
  (set-face-attribute 'default nil :height 160))

;; Desactivar la barra de desplazamiento en el minibúfer
;; (set-window-scroll-bars (minibuffer-window) nil nil)

;; Fuentes de prosa
(set-face-attribute 'variable-pitch nil :family "Noto Serif")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'variable-pitch-mode)

;; Abrir ediff en el mismo frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No mostrar en M-x comandos que dependan del modo si sirve de nada
(setq read-extended-command-predicate #'command-completion-default-include-p)

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
                                     ("Magit" (or (name . ,(rx bol "magit:"))
						  (name . ,(rx bol "magit-"))))
                                     ("Emacs" (or (name . ,(rx bol "*scratch*" eol))
						  (name . ,(rx bol "*Messages*" eol))
						  (name . ,(rx bol "*Bookmark List*" eol))
						  (name . ,(rx bol "*GNU Emacs*" eol))
						  (name . ,(rx bol "*Async-native-compile-log*" eol)))))))

;; Un poco de eshell
(setq eshell-prompt-function 'my-eshell-prompt-fn
      eshell-highlight-prompt t)

;; Usar grupo personalizado en Ibuffer
(add-hook 'ibuffer-mode-hook 'switch-to-my-ibuffer-groups)

;; Usar utf-8
(set-default-coding-systems 'utf-8)

;; Combinaciones de teclas
(keymap-global-set "C-x c" 'comment-or-uncomment-region)
(keymap-global-set "C-x C-r" 'recentf-open-files)
(keymap-global-set "C-x C-m" 'execute-extended-command)
(keymap-global-set "C-=" 'indent-region)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "<mouse-8>" 'previous-buffer)
(keymap-global-set "<mouse-9>" 'next-buffer)
(keymap-global-set "<f5>" 'revert-buffer)

;; Mostrar batería y hora en mi portátil
(when in-laptop-p
  (display-battery-mode 1)
  (display-time-mode 1)
  (setq display-time-day-and-date t
	display-time-default-load-average nil
	display-time-format "%R [%a %-e]"))

;; Indentación para Java
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

;; Añadir MELPA y NonGNU (si hace falta) e inicializar
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (null (assoc "nongnu" package-archives))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Preparar use-package y seguir compilando el fichero
(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (setq use-package-always-ensure t
	use-package-compute-statistics t
	use-package-expand-minimally t))

;; Mantener el directorio de configuración limpio
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  (when (bound-and-true-p recentf-mode) (load-file recentf-save-file))
  (when (bound-and-true-p savehist-mode) (load-file savehist-file)))

;; Cargar archivo personalizado
(load custom-file 'noerror 'nomessage)

;; Colores rebuenos
(use-package modus-themes
  :if (display-graphic-p)
  :defer t
  :commands load-theme
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-disable-other-themes t
        modus-themes-headings '((t . (1.4)))))

;; Otros colores chulos
(use-package standard-themes
  :if (display-graphic-p)
  :defer t
  :commands load-theme
  :config
  (setq standard-themes-mixed-fonts t
        standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-disable-other-themes t
	standard-light-palette-overrides '((fg-main "#2e3446")
					   (bg-main "#ededed")
					   (bg-completion bg-hl-line))
	standard-dark-palette-overrides '((bg-main "#151515"))
	standard-themes-headings '((t . (1.4)))))

;; Solarizando
(use-package solarized-theme
  :if (display-graphic-p)
  :defer t
  :commands load-theme)

(when (display-graphic-p)
  (use-themes standard t "<f6>"))

;; Deshacer puro y duro
(use-package undo-fu
  :init
  (global-unset-key (kbd "C-z"))
  :bind (("<remap> <undo>" . 'undo-fu-only-undo)
	 ("<remap> <undo-redo>" . 'undo-fu-only-redo)
	 ("C-z" . 'undo-fu-only-undo)
	 ("C-S-z" . 'undo-fu-only-redo)))

;; Ventanita de autocompletado
(use-package corfu
  :if (display-graphic-p)
  :config
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3))

;; Cliente de git
(use-package magit
  :commands magit-status)

;; Ver combinaciones de teclas
(use-package which-key
  :config
  (which-key-mode 1))

;; Emular un terminal
(use-package eat
  :commands eat
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :config
  (setq eat-kill-buffer-on-exit t
	eat-enable-mouse t))

;; Complecion vertical
(use-package vertico
  :if (display-graphic-p)
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
  :if (display-graphic-p)
  :after (:any vertico icomplete-vertical fido-vertical)
  :config
  (marginalia-mode 1))

;; Preparar org-mode
(use-package org
  :defer t
  :ensure nil
  :config
  (setq org-ellipsis "" ; "⬎"
	org-fold-catch-invisible-edits 'show-and-error
	org-fontify-todo-headline t
	org-fontify-whole-heading-line t
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-html-head-include-default-style nil
	org-html-htmlize-output-type 'css
	org-html-validation-link nil
	org-pretty-entities t
	org-return-follows-link t
	org-special-ctrl-a/e t
	org-startup-align-all-tables t
	org-startup-indented t
	image-use-external-converter t))

;; Retoques a Org mode
;; (use-package org-modern
;;   :hook ((org-mode . org-modern-mode)
;; 	 (org-agenda-finalize . org-modern-agenda))
;;   :config
;;   (set-face-attribute 'org-modern-label nil :height 1.0)
;;   (setq org-modern-list '((43 . "‣") (45 . "–") (42 . "•"))
;; 	org-modern-table nil))

;; Mostrar aciertos
(use-package anzu
  :defer 1
  :config
  (global-anzu-mode 1)
  (keymap-global-set "<remap> <query-replace>" 'anzu-query-replace)
  (keymap-global-set "<remap> <query-replace-regexp>" 'anzu-query-replace-regexp))

;; Preparar dired
(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "DEL") 'dired-up-directory))

;; Marcar saltos
(use-package pulsar
  :config
  (pulsar-global-mode 1)
  (keymap-global-set "<f8>" 'pulsar-pulse-line))

;; Abreviaciones dinámicas
(use-package dabbrev
  :commands (dabbrev-completion dabbrev-expand)
  :bind (("C-<tab>" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Moverse de ventana con las flechitas
(use-package windmove
  :ensure nil
  :bind (("<f2> <right>" . windmove-right)
	 ("<f2> <left>" . windmove-left)
	 ("<f2> <up>" . windmove-up)
	 ("<f2> <down>" . windmove-down)))

;; Modo mejorado para PDFs
(use-package pdf-tools
  :if (display-graphic-p)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-loader-install)
  (setq pdf-view-display-size 'fit-page))

;; Cliente de IRC
(use-package erc
  :ensure nil
  :commands (erc erc-tls erc-ssl)
  :config
  (setq erc-nick "vido25"
	erc-user-full-name "Vido"
	erc-email-userid "vido"
	erc-system-name "vidonet"
	rc-kill-server-buffer-on-quit t
	erc-kill-buffer-on-part t
	erc-kill-queries-on-quit t)
  (require 'erc-dcc)
  (add-to-list 'erc-dcc-auto-masks "TNW!.*@.*"))

;; Margenes de archivo
(use-package writeroom-mode
  :hook (org-mode . writeroom-mode)
  :init
  (setq writeroom-maximize-window nil
	writeroom-fullscreen-effect 'maximized
	writeroom-mode-line t
	writeroom-bottom-divider-width 0))

;; Estilo C del kernel
(load-file (expand-file-name "kernel.el" user-emacs-directory))
