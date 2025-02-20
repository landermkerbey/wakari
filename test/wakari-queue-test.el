;;; wakari-queue-test.el --- Tests for wakari queue functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for wakari-queue.el

;;; Code:
(require 'test-helper)

(describe "Module availability"
  (it "has queue functions defined"
    (expect (fboundp 'wakari-queue-add) :to-be t)
    (expect (fboundp 'wakari-queue-peek) :to-be t)
    (expect (fboundp 'wakari-queue-next) :to-be t)))

(describe "Queue operations"
  (let (queue-file)
    (before-each
      (setq queue-file (make-temp-file "wakari-queue-test")))
    
    (after-each
      (delete-file queue-file))

    (describe "with empty queue"
      (it "returns nil for next operation"
        (expect (wakari-queue-next queue-file) :to-be nil))
      
      (it "returns nil for peek operation"
        (expect (wakari-queue-peek queue-file) :to-be nil)))

    (describe "with populated queue"
      (before-each
        (wakari-queue-add queue-file "card1.org")
        (wakari-queue-add queue-file "card2.org"))

      (it "maintains FIFO order"
        (expect (wakari-queue-next queue-file) :to-equal "card1.org")
        (expect (wakari-queue-next queue-file) :to-equal "card2.org")
        (expect (wakari-queue-next queue-file) :to-be nil))

      (it "peek shows next item without removing it"
        (expect (wakari-queue-peek queue-file) :to-equal "card1.org")
        (expect (wakari-queue-peek queue-file) :to-equal "card1.org")))))

(provide 'wakari-queue-test)
;;; wakari-queue-test.el ends here
