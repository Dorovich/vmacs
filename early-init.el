;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; Redirect native comp cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
     (expand-file-name "var/eln-cache/" user-emacs-directory))))


;; Startup garbage collection threshold
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defun startup/revert-gc ()
  (setq gc-cons-threshold (* 16 1024 1024)
	gc-cons-percentage 0.1)
(add-hook 'emacs-startup-hook #'startup/revert-gc)

;; Startup file handler alist
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook #'startup/revert-file-name-handler-alist)

;; Setup UI
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha . 100) default-frame-alist)
(push '(width . 98) default-frame-alist)
(push '(height . 48) default-frame-alist)

(setq-default menu-bar-mode nil
	      tool-bar-mode nil
	      scroll-bar-mode nil)

;; Inhibit display until stuff finishes
(unless noninteractive
  (setq inhibit-redisplay t) ; Can cause artifacts
  (setq inhibit-message t)
  (defun startup/reset-inhibited-vars ()
    (setq inhibit-redisplay nil) ; Can cause artifacts
    (setq inhibit-message nil)
    (remove-hook 'post-command-hook #'startup/reset-inhibited-vars))
  (add-hook 'post-command-hook #'startup/reset-inhibited-vars -100))

;; Set utf-8
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Setup package information
(setq package-enable-at-startup t
      package-quickstart nil ; recordar usar ’package-quickstart-refresh’ si es true
      package-native-compile t
      package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
						      ("nongnu" . 80)
						      ("melpa-stable" . 70)
						      ("melpa"  . 0)))
