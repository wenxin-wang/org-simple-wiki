;;; Based largely on https://caiorss.github.io/org-wiki/
;;; Thank you caiorss!

(require 'helm-core)
(require 'projectile)

(defgroup org-simple-wiki nil
  "Settings for the simple org-mode-based wiki"
  :group 'tools)

(defcustom org-simple-wiki-location "~/org/wiki"
  "Org-wiki directory where all wiki pages files *.org are stored.
Default value ~/org/wiki."
  :type 'directory
  :group 'org-simple-wiki)

(defun org-simple-wiki--page-categories ()
  "Get the categories of current page"
  (let ((fst-hdr (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^\\*+ +" nil 1)
                   (point)))
        (re "^[ \t]*#\\+WIKI_CATEGORIES:[ \t]*\\(.+\\)")
        (categories '()))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) fst-hdr)
        (if (re-search-forward re fst-hdr 1)
            (dolist (category (split-string (match-string 1)))
              (add-to-list 'categories category))))
      categories)))

(defun org-simple-wiki-default-wiki-files ()
  "Open files in the default wiki"
  (interactive)
  (if (file-accessible-directory-p org-simple-wiki-location)
      (helm-find-files-1 org-simple-wiki-location))
      (message "`%s' is not accessible as a directory" org-simple-wiki-location))

(defun org-simple-wiki-current-wiki-files ()
  "Open files in the default wiki"
  (interactive)
  (helm-find-files-1 (projectile-project-root)))
