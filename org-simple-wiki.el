;;; org-simple-wiki.el --- simple wiki extension for org-mode
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'helm-projectile)
(require 'helm-ag)

(defgroup org-simple-wiki nil
  "Settings for the simple org-mode-based wiki"
  :group 'tools)

(defcustom org-simple-wiki-location "~/org/wiki"
  "Org-wiki directory where all wiki pages files *.org are stored.
Default value ~/org/wiki."
  :type 'directory
  :group 'org-simple-wiki)

(defun org-simple-wiki-find-file ()
  "Open files in the default wiki"
  (interactive)
  (if (file-accessible-directory-p org-simple-wiki-location)
      (let ((cwd default-directory))
        (cd org-simple-wiki-location)
        (helm-projectile-find-file)
        (cd cwd))
      (message "`%s' is not accessible as a directory" org-simple-wiki-location)))

(defun org-simple-wiki-search-ag ()
  "Search pages"
  (interactive)
  (helm-do-ag org-simple-wiki-location))

(defun org-simple-wiki-search-keyword-ag ()
  "Search pages by WIKI_KW"
  (interactive)
  (let ((word (or (symbol-at-point) ""))) ; Insert word at point for keyword
    (with-temp-buffer ; Dirty trick for helm-ag to use customized default input
      (insert (format "^[ \\t]*#\\+wiki_kw %s" word))
      (let ((helm-ag--extra-options "-G\\.org$")
            (helm-ag-insert-at-point 'paragraph))
        (helm-do-ag org-simple-wiki-location)))))

(provide 'org-simple-wiki)
