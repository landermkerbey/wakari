;;; wakari-index-test.el --- Tests for wakari-index.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for card indexing functionality

;;; Code:

(require 'buttercup)
(require 'wakari-index)

(defvar wakari-test-dir nil
  "Temporary directory for test files.")

(describe "Card indexing"
  (before-all
    ;; Create temporary directory
    (setq wakari-test-dir (make-temp-file "wakari-test-" t))
    
    ;; Create test files
    (with-temp-file (expand-file-name "test1.org" wakari-test-dir)
      (insert "* Regular heading
* Card 1 :srs-item:
* Another heading
** Card 2 :srs-item:
*** Deep heading"))
    
    (with-temp-file (expand-file-name "test2.org" wakari-test-dir)
      (insert "* Top level
** Card 3 :srs-item:
** Not a card
*** Card 4 :srs-item:")))
  
  (after-all
    ;; Clean up test files
    (delete-directory wakari-test-dir t))
  
  (it "finds all cards in org files"
    (let ((cards (wakari-index-find-cards wakari-test-dir)))
      (expect (length cards) :to-equal 4)
      ;; Verify each card is found
      (expect (--any? (string-match-p "test1.org$" (car it)) cards)
              :to-be t)
      (expect (--any? (string-match-p "test2.org$" (car it)) cards)
              :to-be t))))

(provide 'wakari-index-test)
;;; wakari-index-test.el ends here
