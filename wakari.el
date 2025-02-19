;;; wakari.el --- Extensible Spaced Repetition System -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lander M Kerbey

;; Author: Lander M Kerbey
;; Keywords: learning, memory, spaced repetition
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (rec-mode "1.9.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Wakari provides a highly customizable spaced repetition system for
;; knowledge retention and skill cultivation.  It aims to address the
;; limitations of existing SR systems for complex skill development by
;; rejecting the strict flashcard paradigm and allowing the user to
;; integrate arbitrary tools into their practice.

;;; Code:

(require 'org)
(require 'rec-mode)

(defgroup wakari nil
  "Extensible spaced repetition system."
  :group 'applications
  :prefix "wakari-")

(defcustom wakari-data-directory (expand-file-name "wakari" user-emacs-directory)
  "Directory where Wakari stores its data files."
  :type 'directory
  :group 'wakari)

(defcustom wakari-review-file (expand-file-name "review.rec" wakari-data-directory)
  "File storing review data in recutils format."
  :type 'file
  :group 'wakari)

(provide 'wakari)
;;; wakari.el ends here
