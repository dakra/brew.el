;;; brew-tests.el --- Tests for brew.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the pure functions of brew.el, run against captured
;; brew JSON output in test/fixtures/.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'brew)

(defconst brew-tests--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun brew-tests--fixture-json (file)
  "Return the parsed JSON contents of fixture FILE."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat "fixtures/" file) brew-tests--directory))
    (brew--parse-json)))

(defconst brew-tests--installed (brew-tests--fixture-json "installed.json")
  "Parsed installed.json fixture.")

(defun brew-tests--formula (name)
  "Return the formula alist for NAME from the installed fixture."
  (seq-find (lambda (formula) (equal (alist-get 'name formula) name))
            (alist-get 'formulae brew-tests--installed)))

(defun brew-tests--cask (token)
  "Return the cask alist for TOKEN from the installed fixture."
  (seq-find (lambda (cask) (equal (alist-get 'token cask) token))
            (alist-get 'casks brew-tests--installed)))

;;;; Formula entries

(ert-deftest brew-tests-formula-entry-installed ()
  (let ((entry (brew--formula-entry (brew-tests--formula "actionlint"))))
    (should (equal (car entry) '("actionlint" . formula)))
    (let ((vec (cadr entry)))
      (should (equal (aref vec 0) "actionlint"))
      (should (equal (aref vec 1) "formula"))
      (should (equal (aref vec 2) "1.7.12"))
      (should (equal (aref vec 3) "1.7.12"))
      (should (equal (brew--entry-status entry) "installed"))
      (should (equal (aref vec 5)
                     "Static checker for GitHub Actions workflow files")))))

(ert-deftest brew-tests-formula-entry-dependency ()
  (let ((entry (brew--formula-entry (brew-tests--formula "ada-url"))))
    (should (equal (brew--entry-status entry) "dependency"))))

(ert-deftest brew-tests-formula-entry-outdated ()
  (let* ((entry (brew--formula-entry (brew-tests--formula "enchant")))
         (vec (cadr entry)))
    (should (equal (brew--entry-status entry) "outdated"))
    (should (equal (aref vec 2) "2.8.18"))
    (should (equal (aref vec 3) "2.8.19"))))

(ert-deftest brew-tests-formula-entry-deprecated ()
  (let ((entry (brew--formula-entry (brew-tests--formula "gemini-cli"))))
    (should (equal (brew--entry-status entry) "deprecated"))))

(ert-deftest brew-tests-formula-entry-deprecated-not-installed ()
  "A deprecated formula that is not installed is markable as available."
  (let* ((formula (copy-alist (brew-tests--formula "gemini-cli")))
         (formula (cons '(installed . ())
                        (assq-delete-all 'installed formula)))
         (entry (brew--formula-entry formula)))
    (should (equal (brew--entry-status entry) "available"))))

(ert-deftest brew-tests-formula-entry-pinned-wins-over-outdated ()
  (let* ((formula (copy-alist (brew-tests--formula "enchant")))
         (formula (cons '(pinned . t)
                        (assq-delete-all 'pinned formula)))
         (entry (brew--formula-entry formula)))
    (should (equal (brew--entry-status entry) "pinned"))))

(ert-deftest brew-tests-formula-entry-available ()
  (let* ((formula '((name . "ripgrep")
                    (desc . "Search tool")
                    (versions . ((stable . "14.1.1")))
                    (installed . ())
                    (pinned . nil)
                    (outdated . nil)
                    (deprecated . nil)))
         (entry (brew--formula-entry formula))
         (vec (cadr entry)))
    (should (equal (brew--entry-status entry) "available"))
    (should (equal (aref vec 2) ""))
    (should (equal (aref vec 3) "14.1.1"))))

;;;; Cask entries

(ert-deftest brew-tests-cask-entry-installed ()
  (let* ((entry (brew--cask-entry (brew-tests--cask "agentsview")))
         (vec (cadr entry)))
    (should (equal (car entry) '("agentsview" . cask)))
    (should (equal (aref vec 1) "cask"))
    (should (equal (aref vec 2) "0.36.1"))
    (should (equal (aref vec 3) "0.36.1"))
    (should (equal (brew--entry-status entry) "installed"))))

(ert-deftest brew-tests-cask-entry-auto-updates-not-outdated ()
  "A cask with auto-updates and an older installed version is not outdated."
  (let ((entry (brew--cask-entry (brew-tests--cask "antigravity-cli"))))
    (should (equal (brew--entry-status entry) "installed"))))

(ert-deftest brew-tests-version-column-truncation ()
  (should (equal (brew--version-column nil) ""))
  (should (equal (brew--version-column "2.8.18") "2.8.18"))
  (should (equal (brew--version-column "123456789012") "123456789012"))
  (let ((truncated (brew--version-column "1.0.16,4893150192467968")))
    (should (<= (string-width truncated) 12))
    (should (string-suffix-p "…" truncated))
    (should (string-prefix-p "1.0.16," truncated))))

(ert-deftest brew-tests-cask-entry-version-columns-fit ()
  (let ((vec (cadr (brew--cask-entry (brew-tests--cask "antigravity-cli")))))
    (should (<= (string-width (aref vec 2)) 12))
    (should (<= (string-width (aref vec 3)) 12))))

(ert-deftest brew-tests-cask-entry-available ()
  (let* ((cask '((token . "some-app")
                 (desc . "Some app")
                 (version . "1.0")
                 (installed . nil)))
         (entry (brew--cask-entry cask)))
    (should (equal (brew--entry-status entry) "available"))))

(ert-deftest brew-tests-cask-entry-deprecated-not-installed ()
  "A deprecated cask that is not installed is markable as available."
  (let ((entry (brew--cask-entry '((token . "old-app")
                                   (version . "1.0")
                                   (installed . nil)
                                   (deprecated . t)))))
    (should (equal (brew--entry-status entry) "available"))))

(ert-deftest brew-tests-cask-entry-outdated ()
  (let* ((cask '((token . "some-app")
                 (version . "2.0")
                 (installed . "1.0")
                 (outdated . t)))
         (entry (brew--cask-entry cask))
         (vec (cadr entry)))
    (should (equal (brew--entry-status entry) "outdated"))
    (should (equal (aref vec 2) "1.0"))
    (should (equal (aref vec 3) "2.0"))))

;;;; Entry sets

(ert-deftest brew-tests-info-entries ()
  (let ((entries (brew--info-entries brew-tests--installed)))
    (should (= (length entries) 6))
    (should (equal (mapcar #'car entries)
                   '(("actionlint" . formula)
                     ("ada-url" . formula)
                     ("enchant" . formula)
                     ("gemini-cli" . formula)
                     ("agentsview" . cask)
                     ("antigravity-cli" . cask))))))

;;;; Status sorting

(ert-deftest brew-tests-status-sort-order ()
  (let ((outdated (brew--formula-entry (brew-tests--formula "enchant")))
        (installed (brew--formula-entry (brew-tests--formula "actionlint")))
        (dependency (brew--formula-entry (brew-tests--formula "ada-url"))))
    (should (brew--status-sort-p outdated installed))
    (should (brew--status-sort-p installed dependency))
    (should-not (brew--status-sort-p dependency outdated))
    (should-not (brew--status-sort-p installed installed))))

;;;; Filters

(ert-deftest brew-tests-entry-visible-p ()
  (let ((entry (brew--formula-entry (brew-tests--formula "enchant"))))
    (should (brew--entry-visible-p entry nil))
    (should (brew--entry-visible-p entry '((name . "^ench"))))
    (should-not (brew--entry-visible-p entry '((name . "^xyz"))))
    (should (brew--entry-visible-p entry '((status . "outdated"))))
    (should-not (brew--entry-visible-p entry '((status . "installed"))))
    (should (brew--entry-visible-p entry '((type . "formula"))))
    (should-not (brew--entry-visible-p entry '((type . "cask"))))
    (should (brew--entry-visible-p entry '((name . "chant")
                                           (status . "outdated")
                                           (type . "formula"))))))

;;;; Execution plans

(ert-deftest brew-tests-plan-commands-basic ()
  (should (equal (brew--plan-commands
                  '((install . (("foo" . formula) ("bar" . cask)))
                    (upgrade . (("enchant" . formula)))
                    (uninstall . (("baz" . cask))))
                  nil)
                 '(("install" "--formula" "foo")
                   ("install" "--cask" "bar")
                   ("upgrade" "--formula" "enchant")
                   ("uninstall" "--cask" "baz")))))

(ert-deftest brew-tests-plan-commands-flag-routing ()
  "Each flag is only passed to the commands that support it."
  (should (equal (brew--plan-commands
                  '((upgrade . (("a" . formula) ("b" . cask)))
                    (uninstall . (("c" . formula) ("d" . cask))))
                  '("--greedy" "--zap" "--dry-run" "--force"))
                 '(("upgrade" "--formula" "--dry-run" "--force" "a")
                   ("upgrade" "--cask" "--greedy" "--dry-run" "--force" "b")
                   ("uninstall" "--formula" "--force" "c")
                   ("uninstall" "--cask" "--zap" "--force" "d")))))

(ert-deftest brew-tests-plan-commands-groups-names ()
  "Multiple packages of the same type and action share one command."
  (should (equal (brew--plan-commands
                  '((upgrade . (("a" . formula) ("b" . formula))))
                  nil)
                 '(("upgrade" "--formula" "a" "b")))))

(ert-deftest brew-tests-plan-description ()
  (should (equal (brew--plan-description
                  '((install . (("foo" . formula)))
                    (upgrade . (("enchant" . formula) ("sqlite" . formula)))
                    (uninstall . (("bar" . cask)))))
                 "Install: foo · Upgrade: enchant, sqlite · Uninstall: bar"))
  (should (equal (brew--plan-description '((upgrade . (("a" . formula)))))
                 "Upgrade: a")))

;;;; Confirmation prompts

(ert-deftest brew-tests-run-answer-prompt ()
  (let* ((buffer (generate-new-buffer " *brew-tests-prompt*"))
         (proc (make-process :name "brew-tests-prompt" :buffer buffer
                             :command '("cat") :noquery t))
         asked sent)
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc string) (setq sent string))))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "==> Upgrading 2 outdated packages\n")
            (set-marker (process-mark proc) (point)))
          (process-put proc 'brew-prompt-pos (point-min))
          ;; No prompt in the output yet.
          (brew--run-answer-prompt proc)
          (should-not asked)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "==> Do you want to proceed with the upgrade? [y/n]\n")
            (set-marker (process-mark proc) (point)))
          (brew--run-answer-prompt proc)
          (should (equal asked "Proceed with the brew upgrade?"))
          (should (equal sent "y"))
          ;; The same prompt is not answered twice.
          (setq asked nil sent nil)
          (brew--run-answer-prompt proc)
          (should-not asked)
          (should-not sent))
      (delete-process proc)
      (kill-buffer buffer))))

;;;; Taps

(ert-deftest brew-tests-tap-package-names ()
  (let ((json (brew-tests--fixture-json "tap-info.json")))
    (should (equal (brew--tap-package-names json)
                   '("d12frosted/emacs-plus/emacs-plus@29"
                     "d12frosted/emacs-plus/emacs-plus@30"
                     "d12frosted/emacs-plus/emacs-plus@31"
                     "d12frosted/emacs-plus/emacs-app")))))

(ert-deftest brew-tests-tap-package-names-empty ()
  (should-not (brew--tap-package-names nil))
  (should-not (brew--tap-package-names '(((formula_names . ())
                                          (cask_tokens . ()))))))

;;;; Services

(ert-deftest brew-tests-service-entry ()
  (let* ((services (brew-tests--fixture-json "services.json"))
         (started (seq-find (lambda (service)
                              (equal (alist-get 'status service) "started"))
                            services))
         (entry (brew--service-entry started))
         (vec (cadr entry)))
    (should (equal (car entry) (alist-get 'name started)))
    (should (equal (substring-no-properties (aref vec 1)) "started"))
    (should (equal (aref vec 2) "daniel"))
    (should (string-suffix-p ".plist" (aref vec 3)))))

(ert-deftest brew-tests-service-entry-none ()
  (let* ((service '((name . "foo") (status . "none")
                    (user . nil) (file . nil) (exit_code . nil)))
         (entry (brew--service-entry service))
         (vec (cadr entry)))
    (should (equal (substring-no-properties (aref vec 1)) "none"))
    (should (equal (aref vec 2) ""))
    (should (equal (aref vec 3) ""))))

(ert-deftest brew-tests-service-entry-error-exit-code ()
  (let* ((service '((name . "foo") (status . "error")
                    (user . "daniel") (file . "/tmp/foo.plist")
                    (exit_code . 78)))
         (entry (brew--service-entry service))
         (vec (cadr entry)))
    (should (equal (substring-no-properties (aref vec 1)) "error (78)"))))

(provide 'brew-tests)
;;; brew-tests.el ends here
