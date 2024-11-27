;; -*- no-byte-compile: t; lexical-binding: t; -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha . 100) default-frame-alist)
(push '(width . 98) default-frame-alist)
(push '(height . 48) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

(unless noninteractive
  (setq-default inhibit-redisplay t) ; Can cause artifacts
  (setq-default inhibit-message t)

  (defun v/reset-inhibited-vars ()
    (setq-default inhibit-redisplay nil) ; Can cause artifacts
    (setq-default inhibit-message nil)
    (remove-hook 'post-command-hook #'v/reset-inhibited-vars))

  (add-hook 'post-command-hook #'v/reset-inhibited-vars -100))

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq package-enable-at-startup nil
      package-quickstart nil
      package-native-compile t
      package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
						      ("nongnu" . 80)
						      ("melpa-stable" . 70)
						      ("melpa"  . 0)))
