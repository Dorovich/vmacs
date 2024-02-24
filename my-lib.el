;; Recargar configuración
(defun reload-init-file ()
  "Reload the user's init.el twice."
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;; Escribir con privilegios
(defun sudo-save ()
  "Save buffer with elevated privileges."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Cambiar entre modos de vertico temporalmente
(defun vertico-toggle-u-r ()
  "Toggle between unobtrusive and reverse vertico modes."
  (interactive)
  (if vertico-unobtrusive-mode
      (progn
	(vertico-multiform--temporary-mode 'vertico-unobtrusive-mode -1)
	(vertico-multiform--temporary-mode 'vertico-reverse-mode 1))
    (progn
      (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-unobtrusive-mode 1))))
