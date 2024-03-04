;; -*- lexical-binding: t -*-

;; Recargar configuración
(defun reload-init-file ()
  "Reload the user's init.el."
  (interactive)
  (load-file user-init-file))

;; Editar ficheros con privilegios
(defun find-file-as-root (file-name)
  "Like find file, but opens the file as root."
  (interactive "FFind file (root): ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Cambiar entre modos de vertico temporalmente
(defun vertico-my-toggle ()
  "Toggle between unobtrusive and reverse vertico modes."
  (interactive)
  (if vertico-unobtrusive-mode
      (progn
	(vertico-multiform--temporary-mode 'vertico-unobtrusive-mode -1)
	(vertico-multiform--temporary-mode 'vertico-reverse-mode 1))
    (progn
      (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-unobtrusive-mode 1))))
