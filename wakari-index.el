;;; wakari-index.el --- Card indexing functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for finding and indexing SRS cards in org files

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup wakari-index nil
  "Card indexing for Wakari."
  :group 'wakari)

(defcustom wakari-index-tag "srs-item"
  "Tag used to identify SRS items in org files."
  :type 'string
  :group 'wakari-index)

(defun wakari-index-find-cards-in-directory (directory)
  "Find all SRS cards in org files under DIRECTORY.
Returns a list of (file . position) pairs for each card found."
  (let ((default-directory directory)
        cards)
    (with-temp-buffer
      (call-process "rg" nil t nil
                    "--no-heading"
                    "--line-number"
                    "--follow"
                    (concat ":" wakari-index-tag ":")
                    "*.org")
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "\\(.+\\):\\([0-9]+\\):")
          (push (cons (expand-file-name (match-string 1))
                     (string-to-number (match-string 2)))
                cards))
        (forward-line 1)))
    (nreverse cards)))

(defun wakari-index-find-cards (&rest directories)
  "Find all SRS cards in org files under DIRECTORIES.
If no directories are provided, use `default-directory'.
Returns a list of (file . position) pairs for each card found."
  (let* ((dirs (or directories (list default-directory)))
         (cards (cl-loop for dir in dirs
                        append (wakari-index-find-cards-in-directory dir))))
    ;; Remove duplicates by comparing both file and position
    (cl-remove-duplicates cards :test #'equal)))

(provide 'wakari-index)
;;; wakari-index.el ends here
