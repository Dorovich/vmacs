;; -*- lexical-binding: t -*-

(defun my-org-backend-config (backend)
  "Set specific settings depending on which org-mode backend is used."
  (pcase backend
    ((or 'latex 'beamer)
     (setq org-export-with-tags nil)
     (setq my-remove-headlines-tag "latex")
     (setq my-remove-sections-tags '("html")))
    ('html
     (setq org-export-with-tags nil)
     (setq my-remove-headlines-tag "html")
     (setq my-remove-sections-tags '("latex")))
    (_
     (setq org-export-with-tags nil)
     (setq my-remove-headlines-tag nil)
     (setq my-remove-sections-tags '("latex" "html"))))
  ;; Remove sections marked by 'my-remove-sections-re'.
  (setq match-expr (mapconcat #'identity my-remove-sections-tags "|"))
  (org-map-entries
   (lambda ()
     (let ((beg (point))
           (end (save-excursion (org-end-of-subtree t t))))
       (delete-region (point) end)
       (setq org-map-continue-from beg)))
   match-expr)
  ;; Remove headlines marked by 'my-remove-headlines-re'.
  (when my-remove-headlines-tag
    (org-map-entries
     (lambda ()
       (delete-region (point) (line-end-position)))
     my-remove-headlines-tag)))

(add-hook 'org-export-before-parsing-hook #'my-org-backend-config)
