;; Recordar archivos más recientes
(recentf-mode 1)

;; Guardar la entrada del minibúfer
(setq history-length 25)
(savehist-mode 1)

;; Guardar la posición del cursor
(save-place-mode 1)

;; Guardar variables personalizadas en otro archivo
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

;; Desactivar ventanas de diálogo gráficas
(setq use-dialog-box nil)

;; Recargar búfers cuando su contenido ha cambiado
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Desactivar copias de seguridad
(setq make-backup-files nil)

;; Mostrar teclas y pares rápido
(setq echo-keystrokes 0.02
      show-paren-delay 0)

;; Sobreescribir la región seleccionada
(delete-selection-mode 1)

;; Mostrar parejas
(show-paren-mode 1)

;; Autocompletar parejas
(electric-pair-mode 1)
(electric-quote-mode 1)

;; Completar mejor
(setq completion-styles '(emacs22 basic initials substring))

;; Hacer cosas interactivamente en el minibúfer
(fido-mode 1)

;; Usar Ibuffer y confirmaciones abreviadas
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignorar mayúsculas al buscar
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Desactivar pitidos
(setq ring-bell-function 'ignore)

;; No usar tamaño de los carácteres
(setq frame-resize-pixelwise t)

;; Fuentes de prosa
(set-face-attribute 'variable-pitch nil :family "Noto Serif")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))

;; Preparar org-mode
(setq org-pretty-entities t
      org-hide-leading-stars t
      org-startup-indented t
      org-startup-align-all-tables t
      org-return-follows-link t
      org-html-validation-link nil
      org-fontify-todo-headline t
      org-fontify-whole-heading-line t
      image-use-external-converter t)
;; (with-eval-after-load 'org
;;   (let ((mixed-font 'fixed-pitch))
;;     (set-face-attribute 'org-block nil :inherit mixed-font)
;;     (set-face-attribute 'org-block-begin-line nil :inherit mixed-font)
;;     (set-face-attribute 'org-block-end-line nil :inherit mixed-font)
;;     (set-face-attribute 'org-code nil :inherit mixed-font)
;;     (set-face-attribute 'org-document-info-keyword nil :inherit mixed-font)
;;     (set-face-attribute 'org-meta-line nil :inherit mixed-font)
;;     (set-face-attribute 'org-table nil :inherit mixed-font)
;;     (set-face-attribute 'org-verbatim nil :inherit mixed-font))
;;   (set-face-attribute 'org-level-1 nil :height 1.4)
;;   (set-face-attribute 'org-level-2 nil :height 1.3)
;;   (set-face-attribute 'org-level-3 nil :height 1.2)
;;   (set-face-attribute 'org-level-4 nil :height 1.1)
;;   (set-face-attribute 'org-level-5 nil :height 1.1))

;; Agrupar mejor y borrar sin problema
(setq ibuffer-expert t
      ibuffer-saved-filter-groups '(("default"
                                     ("Dired" (mode . dired-mode))
                                     ("ERC" (mode . erc-mode))
                                     ("Emacs" (or (name . "^\\*scratch\\*$")
                                                  (name . "^\\*Messages\\*$"))))))

;; Usar grupo personalizado en Ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Combinaciones de teclas
(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

;; Recargar configuración
(defun reload-init-file ()
  "Reload the user's init.el twice."
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;; Escribir con privilegios
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Añadir MELPA e inicializar
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Pillar paquetes si no estan
(setq use-package-always-ensure t)

;; Mantener el directorio de configuración limpio
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

;; Cargar archivo personalizado
(load custom-file 'noerror 'nomessage)

;; Deshacer puro y duro
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Ventanita de autocompletado
(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
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

;; Colores rebuenos
(use-package modus-themes
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-headings '((0 . (1.2))
                                (1 . (1.4))
                                (2 . (1.3))
                                (3 . (1.2))
                                (4 . (1.1)))
        modus-vivendi-palette-overrides '((cursor yellow-faint)
                                          (fg-heading-1 yellow-cooler)
                                          (fg-heading-2 maroon)
                                          (fg-heading-3 indigo)
                                          (fg-heading-4 olive))))

;; Otros colores chulos
(use-package standard-themes
  :config
  (setq standard-themes-bold-constructs t
	standard-themes-italic-constructs t
	standard-themes-mixed-fonts t
	standard-themes-disable-other-themes t
	standard-themes-headings '((0 . (1.2))
                                   (1 . (1.4))
                                   (2 . (1.3))
                                   (3 . (1.2))
                                   (4 . (1.1))))
  (define-key global-map (kbd "<f5>") #'standard-themes-toggle)
  (standard-themes-load-dark))
