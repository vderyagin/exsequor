;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'exsequor)

(defvar exsequor-test-fixtures-dir
  (expand-file-name "fixtures" (file-name-directory load-file-name)))

(defun exsequor-test-read-fixture (path)
  "Read fixture file at PATH relative to fixtures directory."
  (with-temp-buffer
    (insert-file-contents (expand-file-name path exsequor-test-fixtures-dir))
    (buffer-string)))

(defun exsequor-test-find-by-name (items name)
  "Find item with :name matching NAME in ITEMS list."
  (seq-find (lambda (item) (equal (plist-get item :name) name)) items))

(provide 'test/test-helper)
