;;; aider-comint-tests.el --- Tests for aider-comint.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Package-Requires: ((ert "1.0"))
;; Keywords: test aider comint
;; URL: https://github.com/your/repo

;;; Commentary:
;; This file contains tests for aider-comint.el functionality.

;;; Code:

(require 'ert)
(require 'aider-comint)

;;; Test helper functions

(defmacro with-mock-process (&rest body)
  "Create a temporary process for testing."
  `(with-temp-buffer
     (let ((proc (start-process "test" (current-buffer) "echo")))
       ,@body)))

;;; Unit tests

(ert-deftest aider-comint-format-output-test ()
  "Test output formatting function."
  (should (equal (aider-comint-format-output "") ""))
  (should (equal (aider-comint-format-output "test") "test"))
  (should (equal (aider-comint-format-output "multi\nline") "multi\nline")))

(ert-deftest aider-comint-send-command-test ()
  "Test command sending functionality."
  (with-mock-process
    (should-not (aider-comint-send-command ""))
    (should (aider-comint-send-command "test"))))

(ert-deftest aider-comint--select-directory-test ()
  "Test directory selection logic."
  (let ((aider-comint-last-directory nil))
    (with-mock
      (mock (read-directory-name * * * t) => "/valid/path")
      (should (equal (aider-comint--select-directory) "/valid/path"))
      (should (equal aider-comint-last-directory "/valid/path")))))

;;; Integration tests

(ert-deftest aider-comint-integration-test ()
  "Test complete command execution flow."
  (with-mock-process
    (aider-comint-send-command "test")
    (should (string-match "test" (buffer-string)))))

;;; End of tests

(provide 'aider-comint-tests)
;;; aider-comint-tests.el ends here
