;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold #x40000000)

(defvar k-gc-timer
  (run-with-idle-timer 15 t 'garbage-collect))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
