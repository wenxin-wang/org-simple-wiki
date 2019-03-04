;;; org-simple-wiki.el --- simple wiki extension for org-mode
;; Package-Version: 20170512.2229
;; Inspired by https://caiorss.github.io/org-wiki/
;; Thank you caiorss!

;; I never really understand all these licenses.. Hope this would not cause much trouble.

;; Copyright (C) Wenxin Wang 2017 - 2019.
;; Distributed under the MIT License ( license terms are at https://opensource.org/licenses/MIT ).

;; This seems not a package but more like a bunch of helper functions.
;; I do not think it suits general usage, but feel free to try and change it.

(require 'cl-lib)
(require 'org)
(require 'projectile)

(defgroup org-simple-wiki nil
  "Settings for the simple org-mode-based wiki"
  :group 'tools)

(defcustom org-simple-wiki-location "~/org/wiki"
  "Org-wiki directory where all wiki pages files *.org are stored.
Default value ~/org/wiki."
  :type 'directory
  :group 'org-simple-wiki)

(defcustom org-simple-wiki-keyword "wiki"
  "The org keyword for wiki keyword, as in #+WIKI: keyword1 keyword2"
  :type 'directory
  :group 'org-simple-wiki)

;;;###autoload
(defun org-simple-wiki--list-keywords (dir)
  "List all keywords in a wiki"
  (with-temp-buffer
    (call-process "ag" nil t nil "-o" "--nocolor" "--nofilename"
                  (format "(?<=#\\+%s:).*"
                          (upcase org-simple-wiki-keyword))
                  (expand-file-name dir))
    (cl-remove-duplicates (split-string (buffer-string) "[ \n,]+")
                          :test 'string=)))

(defun org-simple-wiki--path-to-keywords ()
  "Generate keywords from path"
  (let ((offset (length (expand-file-name
                         org-simple-wiki-location))))
    (delete
     ""
     (split-string
      (file-name-directory
       (substring (buffer-file-name) offset))
      "/"))))

;;;###autoload
(defun org-simple-wiki-insert-header ()
  "Insert wiki header at the top of the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert (format "#+TITLE: %s\n#+%s:"
                    (file-name-base (buffer-file-name))
                    (upcase org-simple-wiki-keyword)))
    (dolist (kw (org-simple-wiki--path-to-keywords))
      (insert " " (downcase kw)))
    (insert "\n")))

(defun org-simple-wiki--projectile-file-list ()
  "List files using projectile"
  (with-temp-buffer
    (cd org-simple-wiki-location)
    (let ((projectile-sort-order 'default))
      (projectile-current-project-files))))

;;===== wiki protocol for org-simple-wiki =====
(defun org-simple-wiki--open-page (page)
  "Open page in wiki"
  (let ((file (concat (file-name-as-directory org-simple-wiki-location) (concat page ".org"))))
    (make-directory (file-name-directory file) t)
    (let ((buffer (get-file-buffer file)))
      (if buffer (switch-to-buffer buffer)
        (if (not (file-exists-p file))
            (progn (find-file file)
                   (org-simple-wiki-insert-header))
          (find-file file))))))

;;;###autoload
(defun org-simple-wiki-org-mode-init ()
  "Run when org-mode is loaded"
  (org-link-set-parameters
   "wiki"
   :follow #'org-simple-wiki--open-page))

(provide 'org-simple-wiki)

;;; org-simple-wiki.el ends here
