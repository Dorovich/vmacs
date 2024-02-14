;; Colocar caché en un lugar mejor
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Dejar un margen de 50MB
(setq gc-cons-threshold (* 50 1000 1000))

;; Desactivar la barra de herramientas
(tool-bar-mode -1)

;; Parámetros por defecto de la ventana
(setq default-frame-alist '((alpha . 100)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (left-fringe . 10)
                            (right-fringe . 10)))
