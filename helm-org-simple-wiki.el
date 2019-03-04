;;; org-simple-wiki.el --- simple wiki extension for org-mode
;; Package-Version: 20170512.2229
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017 - 2019.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'org-simple-wiki)
(require 'helm-projectile)
(require 'helm-ag)

;;;###autoload
(defun helm-org-simple-wiki-search-ag ()
  "Search pages"
  (interactive)
  (let ((helm-ag-insert-at-point 'symbol))
    (helm-do-ag org-simple-wiki-location)))

;;;###autoload
(defun helm-org-simple-wiki-search-keyword-ag (&optional word)
  "Search pages by keywords"
  (interactive)
  ;; Insert word at point for keyword
  (setq word (if word (downcase word)
               (setq word (or (symbol-at-point) ""))))
  (with-temp-buffer ; Dirty trick for helm-ag to use customized default input
    (insert (format "^[ \\t]*#\\+%s: %s" org-simple-wiki-keyword word))
    (let ((helm-ag--extra-options "-G\\.org$")
          (helm-ag-insert-at-point 'paragraph))
      (helm-do-ag org-simple-wiki-location))))

;;;###autoload
(defun helm-org-simple-wiki-projectile-find-file ()
  "Open files in the default wiki"
  (interactive)
  (if (file-accessible-directory-p org-simple-wiki-location)
      (with-temp-buffer ; Prevent changing of current buffer's working directory
        (cd org-simple-wiki-location)
        (let ((projectile-sort-order 'default))
          (helm-projectile-find-file)))
    (message "`%s' is not accessible as a directory" org-simple-wiki-location)))

;;;###autoload
(defun helm-org-simple-wiki-find-file ()
  "Open files in the default wiki"
  (interactive)
  (if (file-accessible-directory-p org-simple-wiki-location)
      (helm-find-files-1 (file-name-as-directory
                          (expand-file-name org-simple-wiki-location)))
    (message "`%s' is not accessible as a directory" org-simple-wiki-location)))

;;;###autoload
(defun helm-org-simple-wiki-find-file-by-keyword ()
  "Find page by existing keywords"
  (interactive)
  (helm :sources
        `((name . "A list of wiki keywords")
          (candidates . ,(org-simple-wiki--list-keywords
                          org-simple-wiki-location))
          (action . org-simple-wiki-search-keyword-ag))))

;;;###autoload
(defun helm-org-simple-wiki-insert-keyword ()
  "Insert (selected) keywords after current point"
  (interactive)
  (lexical-let ((buffer (current-buffer)))
    (defun insert-keywords (_)
      (with-current-buffer buffer
        (dolist (kw (helm-marked-candidates))
          (insert " " kw)))))
  (helm :sources
        `((name . "A list of wiki keywords")
          (candidates . ,(org-simple-wiki--list-keywords
                          org-simple-wiki-location))
          (action . insert-keywords))))

;;;###autoload
(defun helm-org-simple-wiki-insert-link ()
  "Insert wiki link at current point."
  (interactive)
  (lexical-let ((buffer (current-buffer)))
    (defun insert-link (name)
      (with-current-buffer buffer
        (insert (format "[[wiki:%s][%s]]" name name)))))
  (let* ((links
          (mapcar #'file-name-sans-extension
                  (org-simple-wiki--projectile-file-list)))
         (actions (helm-make-actions "Insert" #'insert-link))
         (pages `((name . "A list of wiki pages")
                  (candidates . links)
                  (action . actions)))
         (new-page (helm-build-dummy-source
                       "New page" :action actions)))
    (helm :sources '(pages new-page))))

(provide 'helm-org-simple-wiki)
