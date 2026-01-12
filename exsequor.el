;;; exsequor.el --- Run tasks in project and outside of it -*- lexical-binding: t -*-

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 16 Dec 2023
;; Version: 0.2.0

;; Package-Requires: ((consult))

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.
;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;;; Code:

(require 'map)
(require 'project)
(require 'consult)

(eval-when-compile
  (require 'subr-x))

(defun exsequor--just-format-param (param)
  (let* ((name (map-elt param "name"))
         (default (map-elt param "default"))
         (kind (map-elt param "kind")))
    (concat
     (pcase kind
       ("star" "*")
       ("plus" "+")
       (_ ""))
     name
     (when (stringp default)
       (format "=%s" (if (string-empty-p default) "''" (prin1-to-string default)))))))

(defun exsequor--just-format-recipe-name (recipe)
  (let ((namepath (map-elt recipe "namepath"))
        (params (map-elt recipe "parameters")))
    (if (seq-empty-p params)
        namepath
      (concat namepath " " (string-join (seq-map #'exsequor--just-format-param params) " ")))))

(defun exsequor--just-collect-recipes (data flags)
  (let ((recipes (map-elt data "recipes"))
        (modules (map-elt data "modules"))
        (result nil))
    (map-do
     (lambda (_name recipe)
       (let* ((namepath (map-elt recipe "namepath"))
              (doc (map-elt recipe "doc"))
              (private (eq (map-elt recipe "private") t))
              (display-name (exsequor--just-format-recipe-name recipe)))
         (push (list :name display-name
                     :description (and (stringp doc) doc)
                     :action (format "just%s %s" flags namepath)
                     :hidden private)
               result)))
     recipes)
    (map-do
     (lambda (_name submodule)
       (setq result (nconc (exsequor--just-collect-recipes submodule flags) result)))
     modules)
    result))

(defun exsequor--parse-just-json (json-string &optional flag-str)
  "Parse just recipes from JSON-STRING (output of just --dump --dump-format json).
FLAG-STR is an optional string of flags to include in action commands."
  (let ((data (json-parse-string json-string)))
    (seq-sort-by (lambda (item) (plist-get item :name))
                 #'string<
                 (exsequor--just-collect-recipes data (or flag-str "")))))

(defun exsequor--just-parse-recipes (&rest flags)
  (let* ((cmd (string-join (append '("just" "--dump" "--dump-format" "json") flags) " "))
         (json-str (shell-command-to-string cmd))
         (flag-str (if flags (concat " " (string-join flags " ")) "")))
    (exsequor--parse-just-json json-str flag-str)))

(defvar exsequor-cache (make-hash-table :test #'equal))

(defvar-local exsequor--show-hidden nil
  "When non-nil, show hidden tasks in completion.")

(defun exsequor--candidate-visible-p (cand)
  (or exsequor--show-hidden
      (not (get-text-property 0 'exsequor-hidden cand))))

(defun exsequor--multi-predicate (sources)
  (let ((sources-vector (vconcat sources)))
    (lambda (cand)
      (and (consult--multi-predicate sources-vector cand)
           (exsequor--candidate-visible-p cand)))))

(defun exsequor-toggle-show-hidden ()
  (interactive)
  (setq exsequor--show-hidden (not exsequor--show-hidden))
  (message (if exsequor--show-hidden
               "Showing all tasks"
             "Showing only public tasks"))
  (run-hooks 'consult--completion-refresh-hook))

(defvar exsequor-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'exsequor-toggle-show-hidden)
    (define-key map (kbd "M-.") #'exsequor-jump-to-definition)
    map))

(cl-defun exsequor-add-command-set (name &key items items-fn predicate global narrow)
  (cl-assert (and (xor (and items (listp items))
                       (functionp items-fn))
                  (functionp predicate)
                  (stringp name)
                  (or (null narrow) (characterp narrow))))
  (map-put! exsequor-cache name
            (list
             :items (or items-fn (lambda () items))
             :predicate predicate
             :global global
             :narrow narrow)))

(defun exsequor-lookup-command (candidates name)
  (seq-find
   (lambda (candidate) (string= (plist-get candidate :name) name))
   candidates))

(defun exsequor-run-command (command)
  (let ((action (plist-get command :action)))
    (funcall (pcase-exhaustive action
               ((pred stringp) #'compile)
               ((pred functionp) #'funcall))
             action)))

(defun exsequor-annotate (command)
  (let ((desc (plist-get command :description))
        (action (plist-get command :action)))
    (or desc
        (and (stringp action) (format "(%s)" action)))))

(defun exsequor--source-location-marker (cand)
  (when-let* ((file (get-text-property 0 'exsequor-source-file cand))
              (line (get-text-property 0 'exsequor-source-line cand))
              ((file-exists-p file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (point-marker)))))

(defun exsequor--source-preview ()
  (let ((preview (consult--jump-preview)))
    (lambda (action cand)
      (funcall preview action (and cand (exsequor--source-location-marker (car cand)))))))

(defun exsequor--get-current-candidate ()
  (run-hook-with-args-until-success 'consult--completion-candidate-hook))

(defun exsequor-jump-to-definition ()
  (interactive)
  (if-let* ((cand (exsequor--get-current-candidate))
            (file (get-text-property 0 'exsequor-source-file cand))
            (line (get-text-property 0 'exsequor-source-line cand)))
      (progn
        (run-at-time 0 nil
                     (lambda ()
                       (find-file file)
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (recenter)
                       (pulse-momentary-highlight-one-line (point))))
        (abort-recursive-edit))
    (message "No source location available for this command")))

(defun exsequor-make-source (name command-set)
  (when-let* (((funcall (plist-get command-set :predicate)))
              (items (funcall (plist-get command-set :items)))
              (root default-directory))
    (list
     :items (seq-map
             (lambda (command)
               (let ((item-name (plist-get command :name))
                     (source-file (plist-get command :source-file))
                     (source-line (plist-get command :source-line)))
                 (when (plist-get command :hidden)
                   (put-text-property 0 (length item-name) 'exsequor-hidden t item-name))
                 (when source-file
                   (put-text-property 0 (length item-name) 'exsequor-source-file source-file item-name))
                 (when source-line
                   (put-text-property 0 (length item-name) 'exsequor-source-line source-line item-name))
                 item-name))
             items)
     :name name
     :narrow (plist-get command-set :narrow)
     :annotate (lambda (name)
                 (exsequor-annotate (exsequor-lookup-command items name)))
     :action (lambda (name)
               (let ((default-directory root))
                 (exsequor-run-command (exsequor-lookup-command items name)))))))

(defun exsequor-sources (root)
  (let ((default-directory root))
    (thread-last
      exsequor-cache
      (map-apply #'exsequor-make-source)
      (seq-filter #'identity))))

(defun exsequor-sources-global ()
  (thread-last
    exsequor-cache
    (map-filter (lambda (_name command-set) (plist-get command-set :global)))
    (map-apply #'exsequor-make-source)
    (seq-filter #'identity)))

;;;###autoload
(defun exsequor-run-in-project ()
  (interactive)
  (let* ((root (if-let* ((project (project-current)))
                   (project-root project)
                 default-directory))
         (sources (exsequor-sources root)))
    (consult--multi sources
                    :sort nil
                    :keymap exsequor-minibuffer-map
                    :predicate (exsequor--multi-predicate sources)
                    :state (exsequor--source-preview)
                    :preview-key "M-o")))

;;;###autoload
(defun exsequor-run-global ()
  (interactive)
  (let ((sources (exsequor-sources-global)))
    (consult--multi sources
                    :sort nil
                    :keymap exsequor-minibuffer-map
                    :predicate (exsequor--multi-predicate sources)
                    :state (exsequor--source-preview)
                    :preview-key "M-o")))

(exsequor-add-command-set
 "Gentoo overlay"
 :items '((:name "scan for QA issues" :action "pkgcheck scan")
          (:name "update manifests" :action "pkgdev manifest"))
 :predicate
 (lambda ()
   (and (or (executable-find "pkgcheck")
            (executable-find "pkgdev"))
        (file-regular-p "profiles/repo_name"))))

(defun exsequor--parse-package-json-scripts (json-string)
  "Parse npm scripts from JSON-STRING of package.json contents."
  (map-elt (json-parse-string json-string) "scripts"))

(defun exsequor--package-json-scripts ()
  (when (file-regular-p "package.json")
    (exsequor--parse-package-json-scripts
     (with-temp-buffer
       (insert-file-contents "package.json")
       (buffer-string)))))

(exsequor-add-command-set
 "Node scripts"
 :items-fn
 (lambda ()
   (when-let* ((scripts (exsequor--package-json-scripts)))
     (thread-last
       scripts
       map-pairs
       (seq-map
        (pcase-lambda (`(,name . ,cmd))
          (list
           :name name
           :description (format "(%s)" cmd)
           :action (format "npm run %s" name)))))))
 :narrow ?s
 :predicate
 (lambda ()
   (and (executable-find "npm")
        (file-regular-p "package.json"))))

(exsequor-add-command-set
 "Bun"
 :items-fn
 (lambda ()
   (append
    '((:name "install dependencies" :action "bun install")
      (:name "update dependencies" :action "bun update")
      (:name "list outdated dependencies" :action "bun outdated")
      (:name "audit dependencies" :action "bun audit")
      (:name "run tests" :action "bun test"))
    (when-let* ((scripts (exsequor--package-json-scripts)))
      (thread-last
        scripts
        map-pairs
        (seq-map
         (pcase-lambda (`(,name . ,cmd))
           (list
            :name name
            :description (format "(%s)" cmd)
            :action (format "bun run %s" name))))))))
 :narrow ?b
 :predicate
 (lambda ()
   (and (executable-find "bun")
        (file-regular-p "bun.lock"))))

(exsequor-add-command-set
 "Just"
 :items-fn #'exsequor--just-parse-recipes
 :narrow ?j
 :predicate
 (lambda ()
   (and (executable-find "just")
        (not (file-equal-p default-directory "~/"))
        (or (file-regular-p "Justfile")
            (file-regular-p ".justfile")
            (file-regular-p "justfile")))))

(exsequor-add-command-set
 "Just (global)"
 :items-fn (lambda () (exsequor--just-parse-recipes "--global-justfile"))
 :narrow ?J
 :global t
 :predicate
 (lambda ()
   (and (executable-find "just")
        (seq-some
         #'file-regular-p
         (list
          (expand-file-name "just/justfile" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
          "~/.config/just/justfile"
          "~/justfile"
          "~/.justfile")))))

(exsequor-add-command-set
 "Cargo"
 :items '((:name "build" :action "cargo build")
          (:name "test" :action "cargo test")
          (:name "release build" :action "cargo build --release")
          (:name "run benchmarks" :action "cargo bench")
          (:name "update dependencies" :action "cargo update")
          (:name "show outdated dependencies" :action "cargo outdated")
          (:name "run" :action "cargo run")
          (:name "check & report errors" :action "cargo check")
          (:name "remove 'target' directory" :action "cargo clean"))
 :narrow ?c
 :predicate
 (lambda ()
   (and (executable-find "cargo")
        (file-regular-p "Cargo.toml"))))

(exsequor-add-command-set
 "Mix"
 :items '((:name "build" :action "mix compile")
          (:name "test" :action "mix test")
          (:name "get dependencies" :action "mix deps.get")
          (:name "delete generated application files" :action "mix clean")
          (:name "list outdated dependencies" :action "mix hex.outdated --all")
          (:name "run dialyzer" :action "mix dialyzer")
          (:name "run Elixir formatter" :action "mix format")
          (:name "run arbitrary mix command"
                 :action (lambda ()
                           (thread-last
                             "Command: "
                             read-string
                             (format "mix %s")
                             compile))))
 :narrow ?m
 :predicate
 (lambda ()
   (and (executable-find "mix")
        (file-regular-p "mix.exs"))))

(exsequor-add-command-set
 "Eldev"
 :items '((:name "compile elisp"
                 :action "eldev compile --force-all --warnings-as-errors --keep-going")
          (:name "install dependencies" :action "eldev prepare")
          (:name "update dependencies" :action "eldev upgrade")
          (:name "clean bytecode" :action "eldev clean"))
 :narrow ?e
 :predicate
 (lambda ()
   (and (executable-find "eldev")
        (file-regular-p "Eldev"))))

(exsequor-add-command-set
 "Cask"
 :items '((:name "build" :action "cask build")
          (:name "install depencencies" :action "cask install")
          (:name "update dependencies" :action "cask update")
          (:name "cleanup bytecode" :action "cask clean-elc")
          (:name "list dependencies" :action "cask list")
          (:name "eval expression"
                 :action (lambda ()
                           (thread-last
                             "Expression: "
                             read--expression
                             prin1-to-string
                             (format "cask eval '(message \"%%s\" %s)'")
                             compile))))
 :predicate
 (lambda ()
   (and (executable-find "cask")
        (file-regular-p "Cask"))))

(exsequor-add-command-set
 "Bundle"
 :items '((:name "install dependencies" :action "bundle install")
          (:name "update dependencies" :action "bundle update")
          (:name "list outdated dependencies" :action "bundle outdated"))
 :predicate
 (lambda ()
   (and (executable-find "bundle")
        (file-regular-p "Gemfile"))) )

(exsequor-add-command-set
 "NPM"
 :items '((:name "test" :action "npm test")
          (:name "update depencencies" :action "npm update")
          (:name "list outdated packages" :action "npm outdated")
          (:name "install dependencies" :action "npm install")
          (:name "add runtime dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "npm install --save %s")
                             compile)))
          (:name "add development dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "npm install --save-dev %s")
                             compile))))
 :narrow ?n
 :predicate
 (lambda ()
   (and (executable-find "npm")
        (file-regular-p "package.json"))))

(exsequor-add-command-set
 "Yarn"
 :items '((:name "install dependencies" :action "yarn --no-emoji --no-progress")
          (:name "update depencencies" :action "yarn upgrade --no-emoji --no-progress")
          (:name "list outdated dependencies" :action "yarn outdated")
          (:name "add runtime dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "yarn add --no-emoji --no-progress %s")
                             compile)))
          (:name "add development dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "yarn add --dev --no-emoji --no-progress  %s")
                             compile))))
 :narrow ?y
 :predicate
 (lambda ()
   (and (executable-find "yarn")
        (file-regular-p "yarn.lock"))))

(exsequor-add-command-set
 "Sorbet"
 :items '((:name "check types" :action "srb typecheck"))
 :predicate
 (lambda ()
   (and (executable-find "srb")
        (file-regular-p "sorbet/config"))))

(defun exsequor--rake-strip-args (task-name)
  (car (split-string task-name "\\[" t)))

(defun exsequor--parse-rake-where (output-string)
  "Parse rake task locations from OUTPUT-STRING (output of rake --where --all).
Returns alist of (task-name . (file . line))."
  (let ((rx (rx "rake " (group (+ (not space))) (+ space)
                (group (+? nonl)) ":" (group (+ digit)) ":in")))
    (thread-last
      output-string
      string-lines
      (seq-keep
       (lambda (line)
         (when (string-match rx line)
           (let ((raw-task (match-string 1 line))
                 (file (match-string 2 line))
                 (line-num (string-to-number (match-string 3 line))))
             (cons (exsequor--rake-strip-args raw-task)
                   (cons file line-num)))))))))

(defun exsequor--rake-parse-where (&rest flags)
  (let ((cmd (string-join (append '("rake" "--where" "--all") flags) " ")))
    (exsequor--parse-rake-where (shell-command-to-string cmd))))

(defun exsequor--parse-rake-tasks (tasks-output locations &optional flag-str)
  "Parse rake tasks from TASKS-OUTPUT with pre-parsed LOCATIONS alist.
FLAG-STR is an optional string of flags to include in action commands."
  (thread-last
    tasks-output
    string-lines
    (seq-filter (lambda (line) (string-prefix-p "rake " line)))
    (seq-map
     (lambda (line)
       (let* ((parts (split-string line "#" t (rx (+ space))))
              (task (string-trim (string-remove-prefix "rake " (car parts))))
              (task-name (exsequor--rake-strip-args task))
              (desc (cadr parts))
              (loc (cdr (assoc task-name locations))))
         (list
          :name task
          :description (and desc (format "(%s)" desc))
          :action (format "rake%s %s" (or flag-str "") task-name)
          :hidden (not desc)
          :source-file (car loc)
          :source-line (cdr loc)))))))

(defun exsequor--rake-parse-tasks (&rest flags)
  (let* ((cmd (string-join (append '("rake" "--all" "--tasks") flags) " "))
         (flag-str (if flags (concat " " (string-join flags " ")) ""))
         (locations (apply #'exsequor--rake-parse-where flags)))
    (exsequor--parse-rake-tasks (shell-command-to-string cmd) locations flag-str)))

(exsequor-add-command-set
 "Rake"
 :items-fn #'exsequor--rake-parse-tasks
 :narrow ?r
 :predicate
 (lambda ()
   (and (executable-find "rake")
        (or (file-regular-p "Rakefile")
            (file-regular-p "rakefile")
            (file-regular-p "Rakefile.rb")
            (file-regular-p "rakefile.rb")))))

(exsequor-add-command-set
 "Rake (global)"
 :items-fn (lambda () (exsequor--rake-parse-tasks "--system"))
 :narrow ?R
 :global t
 :predicate
 (lambda ()
   (and (executable-find "rake")
        (file-expand-wildcards "~/.rake/*.rake"))))

(provide 'exsequor)

;;; exsequor.el ends here
