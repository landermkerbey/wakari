;;; wakari-index-test.el --- Tests for wakari-index.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for card indexing functionality

;;; Code:

(require 'buttercup)
(require 'dash)
(require 'wakari-index)

(defvar wakari-test-dir nil
  "Temporary directory for test files.")

(describe "Card indexing"
  (before-all
    ;; Create temporary directories
    (setq wakari-test-dir (make-temp-file "wakari-test-" t))
    (message "Created test dir: %s" wakari-test-dir)
    (make-directory (expand-file-name "subdir" wakari-test-dir))
    (message "Created subdir: %s" (expand-file-name "subdir" wakari-test-dir))
    
    ;; Create test files
    (with-temp-file (expand-file-name "test1.org" wakari-test-dir)
      (insert "* Regular heading
* Card 1 :srs-item:
* Another heading
** Card 2 :srs-item:
*** Deep heading")
      (message "Created test1.org with content"))
    
    (with-temp-file (expand-file-name "test2.org" wakari-test-dir)
      (insert "* Top level
** Card 3 :srs-item:
** Not a card
*** Card 4 :srs-item:"))

    (with-temp-file (expand-file-name "subdir/test3.org" wakari-test-dir)
      (insert "* Nested file
** Card 5 :srs-item:
*** Card 6 :srs-item:")))
  
  (after-all
    ;; Clean up test files
    (delete-directory wakari-test-dir t))
  
  (it "finds all cards in org files recursively"
    (let ((cards (wakari-index-find-cards wakari-test-dir)))
      (message "Found cards: %S" cards)
      (expect (length cards) :to-equal 6)
      ;; Verify each file is found
      (expect (--count (string-match-p "test1.org$" (car it)) cards) :to-equal 2)
      (expect (--count (string-match-p "test2.org$" (car it)) cards) :to-equal 2)
      (expect (--count (string-match-p "test3.org$" (car it)) cards) :to-equal 2)))
  
  (it "finds cards in multiple directories"
    (let* ((subdir (expand-file-name "subdir" wakari-test-dir))
           (cards (wakari-index-find-cards wakari-test-dir subdir)))
      (message "Found cards in multiple dirs: %S" cards)
      (expect (length cards) :to-equal 6)  ; Only unique cards should be returned
      (expect (--count (string-match-p "test3.org$" (car it)) cards) :to-equal 2))))

(provide 'wakari-index-test)
;;; wakari-index-test.el ends here
