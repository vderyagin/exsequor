default: build

# compile elisp
build: _ensure-eldev-installed
    eldev compile \
      --force-all \
      --warnings-as-errors \
      --keep-going

# install dependencies
deps: _ensure-eldev-installed
    eldev prepare

# update dependencies
deps-update: _ensure-eldev-installed
    eldev upgrade

# clean up bytecode
clean: _ensure-eldev-installed
    eldev clean

# run tests
test: _ensure-eldev-installed
    eldev test

# format all elisp files
@format: _ensure-eldev-installed
  eldev exec "\
  (progn \
    (require 'rx) \
    (require 'seq) \
    (require 'subr-x) \
    (require 'buttercup) \
    \
    (defun format--hidden-path-p (path) \
      (string-match-p (rx string-start \".\" (+ (not \"/\"))) \
                      (file-relative-name path default-directory))) \
    \
    (defun format--el-files () \
      (seq-remove \
       #'format--hidden-path-p \
       (directory-files-recursively default-directory (rx \".el\" string-end) nil))) \
    \
    (let ((inhibit-message t) \
          (make-backup-files nil) \
          (files (format--el-files))) \
      (seq-do #'load-file files) \
      (seq-do \
       (lambda (file) \
         (let ((enable-local-variables nil) \
               (enable-local-eval nil)) \
           (with-current-buffer (find-file-noselect file) \
             (let ((indent-tabs-mode nil)) \
               (indent-region (point-min) (point-max))) \
             (save-buffer) \
             (kill-buffer (current-buffer))))) \
       files) \
      (kill-emacs 0)))"

@_ensure-eldev-installed:
    if ! command -v "eldev" &> /dev/null; then \
        echo "Eldev (https://github.com/emacs-eldev/eldev) is not installed"; \
        exit 1; \
    fi
