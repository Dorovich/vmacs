;; -*- lexical-binding: t -*-

;; ¿estoy en los ordenadores de la uni?
(defconst is-uni (string-match
		   (rx letter num (zero-or-one letter) (= 3 num) "pc" (= 2 num))
		   (system-name)))

;; ¿estoy en el portatil?
(defconst is-laptop (string-match
		      (rx "colmena")
		      (system-name)))

;; ¿estoy en ordenador principal?
(defconst is-desktop (string-match
		       (rx "vidonet")
		       (system-name)))

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

;; Cambiar a mis grupos de ibuffer
(defun switch-to-my-ibuffer-groups ()
  (ibuffer-switch-to-saved-filter-groups "default"))

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

;; Cambiar entre temas de colores
(defmacro use-themes (name &optional light toggle-key)
  (let ((__mds (eq name 'modus))
	(__std (eq name 'standard)))
    `(progn
       (require ,(cond (__mds ''modus-themes)
		       (__std ''standard-themes)))
       (if ,light
	   ,(cond (__mds '(modus-themes-load-theme 'modus-operandi))
		  (__std '(standard-themes-load-light)))
	 ,(cond (__mds '(modus-themes-load-theme 'modus-vivendi))
		(__std '(standard-themes-load-dark))))
       (when ,toggle-key
	 (define-key global-map ,toggle-key
		     ,(cond (__mds ''modus-themes-toggle)
			    (__std ''standard-themes-toggle)))))))
