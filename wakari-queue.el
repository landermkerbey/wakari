;;; wakari-queue.el --- Queue management for wakari -*- lexical-binding: t -*-

;;; Commentary:
;; Handles the queueing of cards for review using recutils for storage

;;; Code:
(require 'rec-mode)

(defun wakari-queue--ensure-descriptor (queue-file)
  "Ensure QUEUE-FILE has proper rec descriptor."
  (unless (file-exists-p queue-file)
    (with-temp-file queue-file
      (insert "%rec: Card\n%doc: Queue of cards for review\n%mandatory: path\n"))))

(defun wakari-queue-add (queue-file card-path)
  "Add CARD-PATH to the queue stored in QUEUE-FILE."
  (wakari-queue--ensure-descriptor queue-file)
  (with-temp-buffer
    (rec-mode)
    (insert-file-contents queue-file)
    (goto-char (point-max))
    (rec-insert-record "Card" `(("path" ,card-path)))
    (write-region (point-min) (point-max) queue-file)))

(defun wakari-queue-peek (queue-file)
  "Return the next card path from QUEUE-FILE without removing it."
  (when (file-exists-p queue-file)
    (with-temp-buffer
      (rec-mode)
      (insert-file-contents queue-file)
      (message "Buffer contents: %S" (buffer-string))
      (let ((found (rec-cmd-goto-next-rec)))
        (message "Found record: %S" found)
        (when found
          (rec-get-field "path"))))))

(defun wakari-queue-next (queue-file)
  "Get and remove the next card path from QUEUE-FILE."
  (let ((next-card (wakari-queue-peek queue-file)))
    (when next-card
      (with-temp-buffer
        (rec-mode)
        (insert-file-contents queue-file)
        (when (rec-cmd-goto-next-rec)
          (rec-delete-record)
          (write-region (point-min) (point-max) queue-file)))
      next-card)))

(provide 'wakari-queue)
;;; wakari-queue.el ends here
