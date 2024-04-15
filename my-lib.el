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
	(__std (eq name 'standard))
	(__tmr (eq name 'tomorrow)))
    `(progn
       (require ,(cond (__mds ''modus-themes)
		       (__std ''standard-themes)
		       (__tmr ''color-theme-sanityinc-tomorrow)))
       (if ,light
	   ,(cond (__mds '(modus-themes-load-theme 'modus-operandi))
		  (__std '(standard-themes-load-light))
		  (__tmr '(color-theme-sanityinc-tomorrow-day)))
	 ,(cond (__mds '(modus-themes-load-theme 'modus-vivendi))
		(__std '(standard-themes-load-dark))
		(__tmr '(color-theme-sanityinc-tomorrow-night))))
       (when ,toggle-key
	 (keymap-global-set ,toggle-key
			    ,(cond (__mds ''modus-themes-toggle)
				   (__std ''standard-themes-toggle)
				   (__tmr ''color-theme-sanityinc-tomorrow-toggle)))))))

(defun color-theme-sanityinc-tomorrow-toggle ()
  (interactive)
  "Toggle between night and day tomorrow themes."
  (if (member 'sanityinc-tomorrow-night custom-enabled-themes)
      (color-theme-sanityinc-tomorrow-day)
    (color-theme-sanityinc-tomorrow-night)))

;; Arreglar formato org-mode
(defun org-fix-format ()
  (interactive)
  (when window-system
    (dolist (face org-level-faces)
      (set-face-attribute face nil :height 1.4 :weight 'bold))
    (dolist (face '(org-block org-code org-verbatim org-table org-drawer
			      org-table org-formula org-special-keyword org-block
			      org-property-value org-document-info-keyword))
      (set-face-attribute face nil :inherit 'fixed-pitch)))
  (set-face-attribute 'org-table nil :height 1.0)
  (set-face-attribute 'org-formula nil :height 1.0))

;; Cambiar marcador de eshell
(defun my-eshell-prompt-fn ()
  (concat
   (if (string= (eshell/pwd) (getenv "HOME"))
       "~"
     (eshell/basename (eshell/pwd)))
   " $ "))

;; Otras cosas que he hecho por diversión
(defmacro ++ (&rest args)
  "Returns the sum of ‘args’ plus 1."
  `(+ 1 ,@args))

(defun roll-i (n f v)
  (if (= n 1)
      (++ v (random f))
    (roll-i (1- n) f (++ v (random f)))))

(defun roll (n f)
  "Roll the dice!"
  (interactive "NNumber of dice: \nnFaces of dice: ")
  (message (number-to-string (roll-i n f 0))))

