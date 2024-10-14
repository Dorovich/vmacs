;; -*- lexical-binding: t -*-

;; ¿estoy en los ordenadores de la uni?
(defconst in-uni-p (string-match
		    (rx letter num (zero-or-one letter) (= 3 num) "pc" (= 2 num))
		    (system-name)))

;; ¿estoy en el portatil?
(defconst in-laptop-p (string-match
		       (rx "colmena")
		       (system-name)))

;; ¿estoy en el portatil del trabajo (BSC)?
(defconst in-bsc-laptop-p (string-match
			   (rx "BSC-8488191165")
			   (system-name)))

;; ¿estoy en ordenador principal?
(defconst in-desktop-p (string-match
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
	(__sol (eq name 'solarized))
	(__alt (eq name 'alt)))
    `(progn
       (require ,@(cond (__mds '('modus-themes))
		       (__std '('standard-themes))
		       (__sol '('solarized-theme))
		       (__alt '(nil nil t))))
       (if ,light
	   ,(cond (__mds '(modus-themes-load-theme 'modus-operandi))
		  (__std '(standard-themes-load-light))
		  (__sol '(load-theme 'solarized-selenized-light t))
		  (__alt '(progn (load-theme 'adwaita t) (set-cursor-color "#151515"))))
	 ,(cond (__mds '(modus-themes-load-theme 'modus-vivendi))
		(__std '(standard-themes-load-dark))
		(__sol '(load-theme 'solarized-selenized-dark t))
		(__alt '(progn (load-theme 'wombat t) (set-cursor-color "#fefefe")))))
       (when ,toggle-key
	 (keymap-global-set ,toggle-key
			    ,(cond (__mds ''modus-themes-toggle)
				   (__std ''standard-themes-toggle)
				   (__sol ''color-theme-solarized-toggle)
				   (__alt ''color-theme-alt-toggle)))))))

(defun theme-switch (a b)
  "Toggle between themes `a' and `b'. All other themes are disabled."
  (let ((theme (if (member a custom-enabled-themes) b a)))
    (disable-all-themes)
    (load-theme theme t)
    theme))

(defun theme-switch-with-cursor-color (a b ca cb)
  "Toggle between themes ‘a’ and ‘b’. All other themes are disabled. Also toggle between cursor colors ‘ca’ for ‘a’ and ‘cb’ for ‘b’."
  (if (eq (theme-switch a b) a)
      (set-cursor-color ca)
    (set-cursor-color cb)))

(defun color-theme-solarized-toggle ()
  "Toggle between light and dark solarized themes."
  (interactive)
  (require 'solarized-theme)
  (theme-switch 'solarized-selenized-dark 'solarized-selenized-light))

(defun color-theme-alt-toggle ()
  "Toggle between light and dark custom themes."
  (interactive)
  (theme-switch-with-cursor-color 'wombat 'adwaita "#fefefe" "#151515"))

;; Desactivar todos los temas
(defun disable-all-themes ()
  (interactive)
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; Cambiar fuentes
(defun use-font (f &optional s)
  (interactive "sFont name: \nnFont size: ")
  (when (member f (font-family-list))
    (set-frame-font (if s
			(concat f "-" (number-to-string s))
		      f)
		    t t)))

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
  "Change the eshell prompt."
  (concat
   (if (string= (eshell/pwd) (getenv "HOME"))
       "~"
     (eshell/basename (eshell/pwd)))
   " $ "))

;; ¡Redactar y desredactar!
;; github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org#try-redacting
(defun redact (beg end &optional func)
  "Redact from ‘beg’ to ‘end’ of region."
  (interactive "r")
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'redact t)
    (overlay-put overlay 'display
		 (cond
		  ((functionp func)
		   (funcall func))
		  ((stringp func)
		   func)
		  (t (make-string (- end beg) ?x))))))

(defun unredact ()
  "Delete the redaction overlay of the selected region."
  (interactive)
  (mapc 'delete-overlay
	(seq-filter (lambda (overlay) (overlay-get overlay 'redact))
		    (overlays-in (point-min) (point-max)))))

;; Para tirar dados (tail-r)
(defun roll-i (n f v)
  "Roll the dice, recursively!"
  (if (= n 1)
      (+ 1 v (random f))
    (roll-i (1- n) f (+ 1 v (random f)))))

(defun roll (n f)
  "Roll the dice! ‘n’ dice of ‘f’ number of faces."
  (interactive "NNumber of dice: \nnFaces of dice: ")
  (message (number-to-string (roll-i n f 0))))

;; Tirar dados, en eshell
(defun eshell/roll (&optional expr &rest args)
  "Roll dice. ‘expr’ should be a dice expression such as 2d6."
  (cond
   ((null expr) "Specify a roll, such as ’2d6’.")
   (args "Too many arguments.")
   (t (let* ((expr-list (split-string expr "d"))
	     (num (string-to-number (car expr-list)))
	     (faces (string-to-number (cadr expr-list))))
	(if (> (length expr-list) 2)
	    "One dice roll only!"
	  (format "%s => %d" expr (roll-i num faces 0)))))))

;; Subir archivos a envs.sh
(defun eshell/0file (file)
  "Obtain an url to a ‘file’, after uploading it to envs.sh."
  (shell-command (concat "curl -F\"file=@" file "\" https://envs.sh")) nil nil)
