;;; org-simple-wiki.el --- simple wiki extension for org-mode
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'cl-lib)
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

(defconst org-simple-wiki--keyword "wiki_kw"
  "The org keyword for wiki keyword, as in #+WIKI_KW: keyword1 keyword2")

;;;###autoload
(defun org-simple-wiki-search-ag ()
  "Search pages"
  (interactive)
  (helm-do-ag org-simple-wiki-location))

;;;###autoload
(defun org-simple-wiki-search-keyword-ag (&optional word)
  "Search pages by keywords"
  (interactive)
  ;; Insert word at point for keyword
  (setq word (if word (downcase word)
               (setq word (or (symbol-at-point) ""))))
  (with-temp-buffer ; Dirty trick for helm-ag to use customized default input
    (insert (format "^[ \\t]*#\\+%s: %s" org-simple-wiki--keyword word))
    (let ((helm-ag--extra-options "-G\\.org$")
          (helm-ag-insert-at-point 'paragraph))
      (helm-do-ag org-simple-wiki-location))))

;;;###autoload
(defun org-simple-wiki-find-file ()
  "Open files in the default wiki"
  (interactive)
  (if (file-accessible-directory-p org-simple-wiki-location)
      (let ((cwd default-directory))
        (cd org-simple-wiki-location)
        (helm-projectile-find-file)
        (cd cwd))
    (message "`%s' is not accessible as a directory" org-simple-wiki-location)))

(defun org-simple-wiki--list-keywords (dir)
  "List all keywords in a wiki"
  (with-temp-buffer
    (call-process "grep" nil t nil "-r" "-h" "-o" "-P"
                  (format "(?<=#\\+%s:).*"
                          (upcase org-simple-wiki--keyword))
                  (expand-file-name dir))
    (remove-duplicates (split-string (buffer-string))
                       :test 'string=)))

;;;###autoload
(defun org-simple-wiki-find-file-by-keyword ()
  "Find page by existing keywords"
  (interactive)
  (helm :sources
        `((name . "A list of wiki keywords")
          (candidates . ,(org-simple-wiki--list-keywords
                          org-simple-wiki-location))
          (action . org-simple-wiki-search-keyword-ag))))

(provide 'org-simple-wiki)
