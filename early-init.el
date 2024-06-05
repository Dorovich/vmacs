;; Calcular tiempos
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Margen de 1GB para el recolector de basura
(setq gc-cons-threshold #x40000000)

;; Recoger basura cuando se está inactivo durante 15s
(defvar k-gc-timer
  (run-with-idle-timer 15 t 'garbage-collect))
;; (lambda ()
;;   (message "Garbage Collector has run for %.06fsec"
;;            (k-time (garbage-collect))))))

;; Colocar caché en un lugar mejor
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Desactivar la barra de herramientas y el menú
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; No mostrar pantalla de inicio si se está abriendo algo
(when (cdr command-line-args)
  (setq inhibit-startup-screen t))

;; Parámetros por defecto de la ventana
(setq default-frame-alist '((alpha . 100)
                            (vertical-scroll-bars . t)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (left-fringe . 10)
                            (right-fringe . 10)))
