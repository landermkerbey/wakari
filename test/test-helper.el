;;; test-helper.el --- Test helper for wakari -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper functions and setup for wakari tests

;;; Code:

(require 'buttercup)
(require 'dash)
(require 'rec-mode)

;; Add project root to load path
(let ((project-root (locate-dominating-file default-directory "Cask")))
  (add-to-list 'load-path project-root))

(require 'wakari-index)
(require 'wakari-queue)

(provide 'test-helper)
;;; test-helper.el ends here
