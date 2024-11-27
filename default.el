;; -*- no-byte-compile: t; lexical-binding: t; -*-

(setq-default ad-redefinition-action 'accept
	      bidi-inhibit-bpa t
	      bidi-paragraph-direction 'left-to-right
	      completion-ignore-case t
	      delete-pair-blink-delay 0.02
	      echo-keystrokes 0.02
	      ediff-window-setup-function 'ediff-setup-windows-plain
	      ffap-machine-p-known 'reject
	      frame-inhibit-implied-resize t
	      global-auto-revert-non-file-buffers t
	      global-text-scale-adjust-resizes-frames nil
	      highlight-nonselected-windows nil
	      hscroll-margin 2
	      hscroll-step 1
	      idle-update-delay 1.0
	      indent-tabs-mode t
	      indicate-buffer-boundaries nil
	      indicate-empty-lines nil
	      inhibit-startup-buffer-menu t
	      lazy-highlight-initial-delay 0
	      load-prefer-newer t
	      make-backup-files nil
	      read-buffer-completion-ignore-case t
	      read-file-name-completion-ignore-case t
	      ring-bell-function 'ignore
	      scroll-conservatively 10
	      scroll-margin 0
	      scroll-step 1
	      setq-default bidi-display-reordering 'left-to-right
	      show-paren-delay 0.02
	      tab-width 8
	      use-dialog-box nil
	      use-file-dialog 0
	      use-short-answers t
	      window-resize-pixelwise nil
	      word-wrap t
	      )
