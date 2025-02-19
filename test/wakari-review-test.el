;;; wakari-review-test.el --- Tests for wakari-review.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for wakari review functionality

;;; Code:

(require 'buttercup)
(require 'wakari-review)

(describe "Review session management"
  (before-each
    (when (get-buffer wakari-review-buffer-name)
      (kill-buffer wakari-review-buffer-name)))
  
  (after-each
    (when (get-buffer wakari-review-buffer-name)
      (kill-buffer wakari-review-buffer-name)))

  (it "creates a review buffer with proper mode"
    (wakari-review-start)
    (with-current-buffer wakari-review-buffer-name
      (expect major-mode) :to-be 'wakari-review-mode
      (expect (buffer-local-value 'truncate-lines (current-buffer)) :to-be t))))

(provide 'wakari-review-test)
;;; wakari-review-test.el ends here
