;;; wakari-queue.el --- Queue management for wakari -*- lexical-binding: t -*-

;;; Commentary:
;; Handles the queueing of cards for review using recutils for storage

;;; Code:
(require 'rec-mode)

(defun wakari-queue-add (queue-file card-path)
  "Add CARD-PATH to the queue stored in QUEUE-FILE."
  (with-temp-buffer
    (when (file-exists-p queue-file)
      (insert-file-contents queue-file))
    (goto-char (point-max))
    (insert "\n%rec: Card\npath: " card-path "\n")))

(defun wakari-queue-peek (queue-file)
  "Return the next card path from QUEUE-FILE without removing it."
  (when (file-exists-p queue-file)
    (with-temp-buffer
      (insert-file-contents queue-file)
      (goto-char (point-min))
      (when (re-search-forward "^path: \\(.+\\)$" nil t)
        (match-string 1)))))

(defun wakari-queue-next (queue-file)
  "Get and remove the next card path from QUEUE-FILE."
  (let ((next-card (wakari-queue-peek queue-file)))
    (when next-card
      (with-temp-buffer
        (insert-file-contents queue-file)
        (goto-char (point-min))
        (when (re-search-forward "^%rec: Card$" nil t)
          (let ((start (match-beginning 0)))
            (forward-paragraph)
            (delete-region start (point))
            (write-region (point-min) (point-max) queue-file))))
      next-card)))

(provide 'wakari-queue)
;;; wakari-queue.el ends here
