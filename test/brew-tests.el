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

(ert-deftest brew-tests-run-answer-password ()
  (let* ((buffer (generate-new-buffer " *brew-tests-password*"))
         (proc (make-process :name "brew-tests-password" :buffer buffer
                             :command '("cat") :noquery t))
         asked sent)
    (unwind-protect
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (prompt &rest _)
                     (setq asked prompt)
                     (copy-sequence "secret")))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc string) (setq sent string))))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "==> Running installer for spotify\n")
            (set-marker (process-mark proc) (point)))
          ;; No password prompt in the output yet.
          (brew--run-answer-password proc)
          (should-not asked)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "Password:")
            (set-marker (process-mark proc) (point)))
          (brew--run-answer-password proc)
          (should (equal asked "Password: "))
          (should (equal sent "secret\n"))
          ;; The same prompt is not answered twice.
          (setq asked nil sent nil)
          (brew--run-answer-password proc)
          (should-not asked)
          (should-not sent)
          ;; A renewed prompt after a failed attempt is answered again.
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "\nSorry, try again.\nPassword:")
            (set-marker (process-mark proc) (point)))
          (brew--run-answer-password proc)
          (should (equal asked "Password: "))
          (should (equal sent "secret\n")))
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

;;;; Tap trust

(ert-deftest brew-tests-tap-trust ()
  (let ((trust (brew-tests--fixture-json "trust.json")))
    (should (equal (brew--tap-trust "homebrew/core" trust) "official"))
    (should (equal (brew--tap-trust "dakra/tap" trust) "trusted"))
    (should (equal (brew--tap-trust "tanrax/tap" trust) "2 items"))
    (should (equal (brew--tap-trust "oven-sh/bun" trust) ""))
    ;; Without trust data (brew trust failed), only official taps count.
    (should (equal (brew--tap-trust "dakra/tap" nil) ""))
    (should (equal (brew--tap-trust "homebrew/cask" nil) "official"))
    ;; Trusted commands count as items.
    (should (equal (brew--tap-trust "foo/bar" '((commands "foo/bar/cmd")))
                   "1 item"))))

(ert-deftest brew-tests-tap-trust-toggle ()
  (with-temp-buffer
    (brew-taps-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (tap) (brew--tap-entry tap nil))
                  '("dakra/tap" "homebrew/core" "oven-sh/bun")))
    (tabulated-list-print)
    (let (ran)
      (cl-letf (((symbol-function 'brew--run)
                 (lambda (args &optional _callback) (setq ran args)))
                ((symbol-function 'yes-or-no-p) #'always))
        (goto-char (point-min))
        ;; Without trust data the toggle cannot tell which way to go.
        (should-error (brew-taps-trust-toggle) :type 'user-error)
        (setq brew--tap-trust '((taps "dakra/tap")))
        (brew-taps-trust-toggle)
        (should (equal ran '("untrust" "--tap" "dakra/tap")))
        (forward-line 1)
        (should-error (brew-taps-trust-toggle) :type 'user-error)
        (forward-line 1)
        (brew-taps-trust-toggle)
        (should (equal ran '("trust" "--tap" "oven-sh/bun")))))))

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

;;;; Vulnerabilities

(ert-deftest brew-tests-vuln-entries ()
  (let* ((entries (brew--vuln-entries (brew-tests--fixture-json "vulns.json")))
         (libxml2 (assoc '("libxml2" . "OSV-2021-777") entries))
         (openjpeg (assoc '("openjpeg" . "CVE-2026-107080") entries)))
    ;; Patched vulnerabilities are left out.
    (should (= (length entries) 5))
    (should-not (assoc '("openjpeg" . "CVE-2026-6192") entries))
    (should (equal (substring-no-properties (aref (cadr libxml2) 2)) "HIGH"))
    (should (equal (aref (cadr libxml2) 4) "2.15.5"))
    (should (equal (aref (cadr openjpeg) 4) ""))
    (should (equal (aref (cadr openjpeg) 5) ""))))

(ert-deftest brew-tests-vuln-severity-sort ()
  (let* ((entries (brew--vuln-entries (brew-tests--fixture-json "vulns.json")))
         (sorted (sort (copy-sequence entries) #'brew--vuln-severity-sort-p)))
    (should (equal (mapcar (lambda (entry)
                             (substring-no-properties (aref (cadr entry) 2)))
                           sorted)
                   '("HIGH" "HIGH" "UNKNOWN" "UNKNOWN" "UNKNOWN")))))

(ert-deftest brew-tests-vulns-summary ()
  (should (equal (brew--vulns-summary (brew-tests--fixture-json "vulns.json"))
                 (concat "5 vulnerabilities in 3 formulae, 226 checked, 4 not scanned, "
                         "1 scanned at the current formula version"))))

(ert-deftest brew-tests-call-exit-codes ()
  "Listed exit codes count as success only when brew printed output."
  (let ((brew-executable "sh")
        (inhibit-message t))
    (pcase-dolist (`(,script ,expected) '(("echo out; exit 1" ok)
                                          ("exit 1" error)
                                          ("echo out; exit 2" error)))
      (let (done)
        (brew--call (list "-c" script)
                    (lambda (_output) (setq done 'ok))
                    nil
                    (lambda () (setq done 'error))
                    '(1))
        (with-timeout (5 (error "Timed out"))
          (while (not done) (accept-process-output nil 0.05)))
        (should (eq done expected))))))

;;;; Version install

(ert-deftest brew-tests-read-formula-completes-formulae-only ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "wget" 'formula table)
    (puthash "firefox" 'cask table)
    (puthash "docker" 'both table)
    (puthash "python@3.12" 'formula table)
    (cl-letf (((symbol-function 'brew--available-packages) (lambda () table))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) collection)))
      (let ((collection (brew--read-formula "Formula: ")))
        (should (equal (sort (all-completions "" collection) #'string<)
                       '("docker" "wget")))
        (should (test-completion "wget" collection))
        (should-not (test-completion "firefox" collection))))))

(ert-deftest brew-tests-version-install ()
  (let (ran)
    (cl-letf (((symbol-function 'brew--run)
               (lambda (args &optional _callback) (setq ran args))))
      (brew-version-install "wget" "1.21")
      (should (equal ran '("version-install" "wget" "1.21")))
      (should-error (brew-version-install "wget" "") :type 'user-error))))

;;;; Upgrade all

(ert-deftest brew-tests-upgrade-all-severity-implies-vulnerable ()
  (let (ran)
    (cl-letf (((symbol-function 'brew--run)
               (lambda (args &optional _callback) (setq ran args))))
      (brew-upgrade-all-run '("--severity=high"))
      (should (equal ran '("upgrade" "--vulnerable" "--severity=high")))
      (brew-upgrade-all-run '("--vulnerable" "--severity=low"))
      (should (equal ran '("upgrade" "--vulnerable" "--severity=low")))
      (brew-upgrade-all-run '("--greedy"))
      (should (equal ran '("upgrade" "--greedy"))))))

(provide 'brew-tests)
;;; brew-tests.el ends here
