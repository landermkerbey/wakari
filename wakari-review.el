;;; wakari-review.el --- Review session management -*- lexical-binding: t; -*-

;;; Commentary:

;; This module handles the review session lifecycle, including:
;; - Starting/ending review sessions
;; - Displaying items for review
;; - Handling user responses
;; - Managing the review buffer and UI

;;; Code:

(require 'org)

(defgroup wakari-review nil
  "Review session management for Wakari."
  :group 'wakari)

(defcustom wakari-review-buffer-name "*Wakari Review*"
  "Name of the buffer used for review sessions."
  :type 'string
  :group 'wakari-review)

(defvar wakari-review--current-session nil
  "Holds data for the current review session.")

(defvar wakari-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wakari-review-show-answer)
    (define-key map (kbd "q") #'wakari-review-quit)
    map)
  "Keymap for `wakari-review-mode'.")

(define-derived-mode wakari-review-mode special-mode "Wakari Review"
  "Major mode for Wakari review sessions."
  :group 'wakari
  (buffer-disable-undo)
  (setq truncate-lines t))

(defun wakari-review-start ()
  "Start a new review session."
  (interactive)
  (let ((review-buffer (get-buffer-create wakari-review-buffer-name)))
    (with-current-buffer review-buffer
      (wakari-review-mode)
      (setq wakari-review--current-session (wakari-review--create-session))
      (wakari-review--display-next-item))
    (switch-to-buffer review-buffer)))

(defun wakari-review--create-session ()
  "Create a new review session structure."
  ;; TODO: Implement session creation logic
  nil)

(defun wakari-review--display-next-item ()
  "Display the next item in the review session."
  ;; TODO: Implement item display logic
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Review session started.\n")))

(defun wakari-review-show-answer ()
  "Show the answer for the current review item."
  (interactive)
  ;; TODO: Implement answer display logic
  )

(defun wakari-review-quit ()
  "End the current review session."
  (interactive)
  (when (y-or-n-p "End review session? ")
    (kill-buffer wakari-review-buffer-name)))

(provide 'wakari-review)
;;; wakari-review.el ends here
