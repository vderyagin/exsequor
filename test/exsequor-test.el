;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "exsequor--parse-package-json-scripts"
  (it "parses scripts from package.json"
    (let* ((json (exsequor-test-read-fixture "package.json"))
           (scripts (exsequor--parse-package-json-scripts json)))
      (expect (map-elt scripts "build") :to-equal "tsc")
      (expect (map-elt scripts "test") :to-equal "jest")
      (expect (map-elt scripts "lint") :to-equal "eslint ."))))

(describe "exsequor--parse-just-json"
  (let (json-str recipes)
    (before-all
      (let ((default-directory exsequor-test-fixtures-dir))
        (setq json-str (shell-command-to-string "just --unstable --dump --dump-format json"))
        (setq recipes (exsequor--parse-just-json json-str))))

    (it "parses basic recipe"
      (let ((build (exsequor-test-find-by-name recipes "build")))
        (expect build :not :to-be nil)
        (expect (plist-get build :description) :to-equal "Build the project")
        (expect (plist-get build :action) :to-equal "just build")
        (expect (plist-get build :hidden) :to-be nil)))

    (it "parses recipe with star parameter"
      (let ((test-recipe (exsequor-test-find-by-name recipes "test *args")))
        (expect test-recipe :not :to-be nil)
        (expect (plist-get test-recipe :description) :to-equal "Run tests")
        (expect (plist-get test-recipe :action) :to-equal "just test")))

    (it "parses recipe with default parameter"
      (let ((deploy (exsequor-test-find-by-name recipes "deploy env=\"staging\"")))
        (expect deploy :not :to-be nil)
        (expect (plist-get deploy :description) :to-equal "Deploy with environment")
        (expect (plist-get deploy :action) :to-equal "just deploy")))

    (it "parses private recipe with hidden flag"
      (let ((helper (exsequor-test-find-by-name recipes "_helper")))
        (expect helper :not :to-be nil)
        (expect (plist-get helper :hidden) :to-be t)))

    (it "parses submodule recipe"
      (let ((subtask (exsequor-test-find-by-name recipes "submodule::subtask")))
        (expect subtask :not :to-be nil)
        (expect (plist-get subtask :description) :to-equal "Submodule task")
        (expect (plist-get subtask :action) :to-equal "just submodule::subtask")))))

(describe "exsequor--parse-rake-where"
  (let (output locations)
    (before-all
      (let ((default-directory exsequor-test-fixtures-dir))
        (setq output (shell-command-to-string "rake --where --all"))
        (setq locations (exsequor--parse-rake-where output))))

    (it "parses task location"
      (let ((build-loc (cdr (assoc "build" locations))))
        (expect build-loc :not :to-be nil)
        (expect (string-suffix-p "Rakefile" (car build-loc)) :to-be t)
        (expect (cdr build-loc) :to-equal 2)))

    (it "parses namespaced task location"
      (let ((migrate-loc (cdr (assoc "db:migrate" locations))))
        (expect migrate-loc :not :to-be nil)
        (expect (string-suffix-p "Rakefile" (car migrate-loc)) :to-be t)
        (expect (cdr migrate-loc) :to-equal 12)))

    (it "parses task with args location"
      (let ((greet-loc (cdr (assoc "greet" locations))))
        (expect greet-loc :not :to-be nil)
        (expect (string-suffix-p "Rakefile" (car greet-loc)) :to-be t)))))

(describe "exsequor--parse-rake-tasks"
  (let (tasks-output where-output locations tasks)
    (before-all
      (let ((default-directory exsequor-test-fixtures-dir))
        (setq tasks-output (shell-command-to-string "rake --all --tasks"))
        (setq where-output (shell-command-to-string "rake --where --all"))
        (setq locations (exsequor--parse-rake-where where-output))
        (setq tasks (exsequor--parse-rake-tasks tasks-output locations))))

    (it "parses task with description"
      (let ((build (exsequor-test-find-by-name tasks "build")))
        (expect build :not :to-be nil)
        (expect (plist-get build :description) :to-equal "(Build the project)")
        (expect (plist-get build :action) :to-equal "rake build")
        (expect (plist-get build :hidden) :to-be nil)))

    (it "parses task without description as hidden"
      (let ((no-desc (exsequor-test-find-by-name tasks "no_desc")))
        (expect no-desc :not :to-be nil)
        (expect (plist-get no-desc :hidden) :to-be t)))

    (it "parses namespaced task"
      (let ((migrate (exsequor-test-find-by-name tasks "db:migrate")))
        (expect migrate :not :to-be nil)
        (expect (plist-get migrate :description) :to-equal "(Run migrations)")
        (expect (plist-get migrate :action) :to-equal "rake db:migrate")))

    (it "parses task with args"
      (let ((greet (exsequor-test-find-by-name tasks "greet[name]")))
        (expect greet :not :to-be nil)
        (expect (plist-get greet :description) :to-equal "(Greet someone)")
        (expect (plist-get greet :action) :to-equal "rake greet")))

    (it "includes source file location"
      (let ((build (exsequor-test-find-by-name tasks "build")))
        (expect (string-suffix-p "Rakefile" (plist-get build :source-file)) :to-be t)
        (expect (plist-get build :source-line) :to-equal 2)))))
