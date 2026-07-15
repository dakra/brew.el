;;; brew.el --- Manage Homebrew packages, services and taps -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/brew.el
;; Keywords: tools processes
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; brew.el is a Homebrew (https://brew.sh) front end for Emacs:
;;
;; - `brew' pops up a transient with all entry points.
;; - `brew-list-packages' lists installed formulae and casks in a
;;   `tabulated-list-mode' buffer modeled after `list-packages':
;;   mark packages with `i' (install), `d' (uninstall) or `U' (mark
;;   all upgrades), then execute the marks with `x'.  `s' searches
;;   all of Homebrew, `RET' shows package details, `p' pins/unpins,
;;   `/' filters by name, status or type.
;; - `brew-services' manages `brew services' daemons (start, stop, restart).
;; - `brew-taps' lists taps and lets you add or remove them.
;; - `brew-install', `brew-upgrade-all', `brew-update',
;;   `brew-cleanup', `brew-autoremove' and `brew-doctor' run the
;;   corresponding maintenance commands.
;;
;; All brew invocations are asynchronous.
;; Mutating commands stream their output into the *brew* buffer.

;;; Code:

(require 'ansi-color)
(require 'let-alist)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

;;;; Customization

(defgroup brew nil
  "Manage Homebrew packages, services and taps."
  :group 'tools
  :prefix "brew-")

(defcustom brew-executable
  (or (executable-find "brew") "/opt/homebrew/bin/brew")
  "Path to the brew executable."
  :type 'string)

(defcustom brew-search-max-results 50
  "Maximum number of search results to fetch details for."
  :type 'integer)

(defcustom brew-tap-max-packages 200
  "Maximum number of tap packages to fetch details for."
  :type 'integer)

(defcustom brew-api-cache-directory
  (expand-file-name
   "api"
   (or (getenv "HOMEBREW_CACHE")
       (if (eq system-type 'darwin)
           "~/Library/Caches/Homebrew"
         (expand-file-name "Homebrew"
                           (or (getenv "XDG_CACHE_HOME") "~/.cache")))))
  "Directory with Homebrew's cached API data.
The files \"formula_names.txt\" and \"cask_names.txt\" in this
directory provide completion candidates for `brew-install'."
  :type 'directory)

;;;; Faces

(defface brew-status-installed
  '((t :inherit success))
  "Face for packages that are installed and up to date.")

(defface brew-status-outdated
  '((t :inherit font-lock-warning-face))
  "Face for installed packages with a newer version available.")

(defface brew-status-pinned
  '((t :inherit font-lock-constant-face))
  "Face for pinned packages.")

(defface brew-status-dependency
  '((t :inherit shadow))
  "Face for packages installed only as a dependency.")

(defface brew-status-available
  '((t :inherit default))
  "Face for packages that are not installed.")

(defface brew-status-deprecated
  '((t :inherit error))
  "Face for deprecated or disabled packages.")

;;;; Process layer

(defvar brew--query-environment
  '("HOMEBREW_NO_ENV_HINTS=1")
  "Extra environment for brew commands whose output stays hidden.")

(defvar brew--run-environment
  '("HOMEBREW_NO_ENV_HINTS=1" "HOMEBREW_COLOR=1")
  "Extra environment for brew commands shown in the *brew* buffer.")

(defun brew--parse-json ()
  "Parse the current buffer as JSON the way brew.el expects.
Objects become alists, arrays become lists and both null and
false become nil."
  (json-parse-buffer :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun brew--parse-lines ()
  "Return the current buffer's contents as a list of non-empty lines."
  (split-string (buffer-string) "\n" t))

(defun brew--call (args callback &optional parser on-error)
  "Run brew with ARGS asynchronously and pass the output to CALLBACK.
The process output is collected in a hidden buffer.  On success,
PARSER (defaulting to `buffer-string') is called in that buffer
and its result is passed to CALLBACK.  On failure, a message with
the stderr output is shown and ON-ERROR, if non-nil, is called
with no arguments."
  (let* ((stdout (generate-new-buffer " *brew-stdout*"))
         (stderr (generate-new-buffer " *brew-stderr*"))
         (stderr-pipe (make-pipe-process
                       :name "brew-stderr" :buffer stderr
                       :sentinel #'ignore :noquery t))
         (process-environment
          (append brew--query-environment process-environment)))
    (make-process
     :name "brew" :buffer stdout :noquery t
     :command (cons brew-executable args)
     :connection-type 'pipe
     :stderr stderr-pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (if (zerop (process-exit-status proc))
                 (let ((result (with-current-buffer stdout
                                 (goto-char (point-min))
                                 (funcall (or parser #'buffer-string)))))
                   (funcall callback result))
               (progn
                 ;; The stderr pipe is a separate process; drain any
                 ;; output it has not delivered yet.
                 (while (accept-process-output stderr-pipe 0.05))
                 (message "brew %s: %s" (string-join args " ")
                          (with-current-buffer stderr
                            (string-trim (buffer-string))))
                 (when on-error (funcall on-error))))
           (delete-process stderr-pipe)
           (kill-buffer stdout)
           (kill-buffer stderr)))))))

(defun brew--call-json (args callback)
  "Run brew with ARGS and pass the parsed JSON output to CALLBACK."
  (brew--call args callback #'brew--parse-json))

(defun brew--call-lines (args callback &optional on-error)
  "Run brew with ARGS and pass the output lines to CALLBACK.
ON-ERROR is called with no arguments when brew fails."
  (brew--call args callback #'brew--parse-lines on-error))

;;;; Mutation runner with visible output

(defvar brew-process-buffer-name "*brew*"
  "Name of the buffer showing output of mutating brew commands.")

(defvar brew--run-queue nil
  "Queued brew commands as (ARGS . CALLBACK) conses.")

(defvar brew--run-active nil
  "Non-nil while a mutating brew command is running.
Only the queue runner sets and clears this; it guards against
starting two brew processes concurrently.")

(defvar brew--update-if-needed-callbacks nil
  "Pending continuations of a running \"brew update-if-needed\".
Non-nil while the update process is in flight; mutating commands
are deferred until it finishes so that they do not contend for
Homebrew's update lock.")

(define-derived-mode brew-process-mode special-mode "Brew-Process"
  "Major mode for the output of mutating brew commands."
  (setq-local window-point-insertion-type t))

(defun brew--process-buffer ()
  "Return the buffer for mutating brew command output."
  (let ((buffer (get-buffer-create brew-process-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'brew-process-mode)
        (brew-process-mode)))
    buffer))

(defun brew--run (args &optional callback)
  "Run brew with ARGS, streaming output into the *brew* buffer.
Commands are executed sequentially; if brew is already running,
the command is queued.  CALLBACK is called with no arguments when
the command finishes, regardless of its exit status."
  (setq brew--run-queue (append brew--run-queue (list (cons args callback))))
  (display-buffer (brew--process-buffer))
  (unless (or brew--run-active brew--update-if-needed-callbacks)
    (brew--run-next)))

(defun brew--run-next ()
  "Start the next queued brew command, if any."
  (if-let* ((job (pop brew--run-queue)))
      (let* ((args (car job))
             (callback (cdr job))
             (buffer (brew--process-buffer))
             (process-environment
              (append brew--run-environment process-environment)))
        (setq brew--run-active t)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (propertize (format "$ brew %s\n" (string-join args " "))
                                'face 'bold))))
        (condition-case err
            (let ((proc (make-process
                         :name "brew" :buffer buffer
                         :command (cons brew-executable args)
                         :connection-type 'pty
                         :filter #'brew--run-filter
                         :sentinel #'brew--run-sentinel)))
              (set-marker (process-mark proc)
                          (with-current-buffer buffer (point-max))
                          buffer)
              (process-put proc 'brew-args args)
              (process-put proc 'brew-callback callback)
              (process-put proc 'brew-prompt-pos
                           (marker-position (process-mark proc))))
          (error
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (propertize (format "✗ %s\n"
                                           (error-message-string err))
                                   'face 'error))))
           (message "brew %s: %s" (string-join args " ")
                    (error-message-string err))
           (when callback (funcall callback))
           (brew--run-next))))
    (setq brew--run-active nil)))

(defun brew--run-filter (proc string)
  "Insert STRING from PROC, handling carriage-return progress redraws.
A carriage return deletes the current output line so that brew's
progress bars are redrawn in place instead of accumulating.  A
carriage return at the end of STRING is deferred to the next call
so that it can be told apart from a \"\\r\\n\" sequence split
across two chunks."
  (when (process-get proc 'brew-pending-cr)
    (process-put proc 'brew-pending-cr nil)
    (setq string (concat "\r" string)))
  (when (string-suffix-p "\r" string)
    (process-put proc 'brew-pending-cr t)
    (setq string (substring string 0 -1)))
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (process-mark proc))
            (setq string (replace-regexp-in-string "\r+\n" "\n" string))
            (let ((parts (split-string string "\r")))
              (insert (ansi-color-apply (car parts)))
              (dolist (part (cdr parts))
                (delete-region (line-beginning-position) (point))
                (insert (ansi-color-apply part))))
            (set-marker (process-mark proc) (point)))))))
  (brew--run-answer-prompt proc))

(defconst brew--run-prompt-regexp
  "^==> Do you want to proceed with the \\([a-z]+\\)\\? \\[y/n\\]$"
  "Regexp matching Homebrew's confirmation prompt.")

(defun brew--run-answer-prompt (proc)
  "Answer a pending Homebrew confirmation prompt of PROC, if any.
Homebrew asks for confirmation before installing, upgrading or
reinstalling packages when the plan includes more than the named
packages.  The question is forwarded to the minibuffer and the
answer is sent to brew, which reads a single character without
waiting for a newline.  Quitting the minibuffer prompt answers
\"n\", making brew abort."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((action (save-excursion
                        (goto-char (process-get proc 'brew-prompt-pos))
                        (when (re-search-forward brew--run-prompt-regexp
                                                 (process-mark proc) t)
                          (process-put proc 'brew-prompt-pos (point))
                          (match-string-no-properties 1)))))
          (when action
            (let ((answer (condition-case nil
                              (if (y-or-n-p (format "Proceed with the brew %s?"
                                                    action))
                                  "y" "n")
                            (quit "n"))))
              (when (process-live-p proc)
                (process-send-string proc answer)))))))))

(defun brew--run-sentinel (proc _event)
  "Handle completion of the mutating brew process PROC."
  (when (memq (process-status proc) '(exit signal))
    (let ((args (process-get proc 'brew-args))
          (callback (process-get proc 'brew-callback))
          (code (process-exit-status proc))
          (buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (if (zerop code)
                        (propertize "✓ finished\n" 'face 'success)
                      (propertize (format "✗ failed with exit code %d\n" code)
                                  'face 'error))))))
      (if (zerop code)
          (message "brew %s: finished" (string-join args " "))
        (message "brew %s: failed with exit code %d (see %s)"
                 (string-join args " ") code brew-process-buffer-name))
      (brew--run-next)
      (when callback (funcall callback)))))

;;;; Data model

(defconst brew--statuses
  '("outdated" "pinned" "deprecated" "installed" "dependency" "available")
  "All package statuses, in sort order (most interesting first).")

(defconst brew--status-faces
  '(("outdated" . brew-status-outdated)
    ("pinned" . brew-status-pinned)
    ("deprecated" . brew-status-deprecated)
    ("installed" . brew-status-installed)
    ("dependency" . brew-status-dependency)
    ("available" . brew-status-available))
  "Alist mapping status strings to faces.")

(defun brew--status-string (status)
  "Return STATUS propertized with its face."
  (propertize status 'face (cdr (assoc status brew--status-faces))))

(defun brew--entry-status (entry)
  "Return the status string of the tabulated-list ENTRY."
  (substring-no-properties (aref (cadr entry) 4)))

(defun brew--status-rank (status)
  "Return the sort rank of STATUS (lower is more interesting)."
  (or (seq-position brew--statuses status) (length brew--statuses)))

(defun brew--status-sort-p (entry1 entry2)
  "Return non-nil if ENTRY1 sorts before ENTRY2 by status."
  (< (brew--status-rank (brew--entry-status entry1))
     (brew--status-rank (brew--entry-status entry2))))

(defun brew--version-column (version)
  "Return VERSION truncated to fit the version columns, or \"\" if nil.
Some cask versions carry a long build id suffix; the full version
is still shown in the package info buffer."
  (if version (truncate-string-to-width version 12 nil nil "…") ""))

(defun brew--formula-entry (formula)
  "Return a tabulated-list entry for the FORMULA info alist."
  (let-alist formula
    (let* ((installed (car .installed))
           (status (cond
                    ((and installed .pinned) "pinned")
                    (.outdated "outdated")
                    ((and installed .deprecated) "deprecated")
                    ((and installed
                          (not (alist-get 'installed_on_request installed)))
                     "dependency")
                    (installed "installed")
                    (t "available"))))
      (list (cons .name 'formula)
            (vector .name
                    "formula"
                    (brew--version-column (alist-get 'version installed))
                    (brew--version-column .versions.stable)
                    (brew--status-string status)
                    (or .desc ""))))))

(defun brew--cask-entry (cask)
  "Return a tabulated-list entry for the CASK info alist."
  (let-alist cask
    (let ((status (cond
                   (.outdated "outdated")
                   ((and .installed .deprecated) "deprecated")
                   (.installed "installed")
                   (t "available"))))
      (list (cons .token 'cask)
            (vector .token
                    "cask"
                    (brew--version-column .installed)
                    (brew--version-column .version)
                    (brew--status-string status)
                    (or .desc ""))))))

(defun brew--info-entries (json)
  "Return tabulated-list entries for all packages in brew info JSON."
  (append (mapcar #'brew--formula-entry (alist-get 'formulae json))
          (mapcar #'brew--cask-entry (alist-get 'casks json))))

;;;; Execution plan

(defun brew--command-flags (action type flags)
  "Return the FLAGS applicable to ACTION on packages of TYPE.
ACTION is one of the symbols `install', `upgrade' and
`uninstall'; TYPE is `formula' or `cask'."
  (let ((allowed (pcase (list action type)
                   ('(install formula) '("--dry-run" "--force"))
                   ('(install cask) '("--dry-run" "--force"))
                   ('(upgrade formula) '("--dry-run" "--force"))
                   ('(upgrade cask) '("--dry-run" "--force" "--greedy"))
                   ('(uninstall formula) '("--force"))
                   ('(uninstall cask) '("--force" "--zap")))))
    (seq-filter (lambda (flag) (member flag allowed)) flags)))

(defun brew--plan-commands (plan flags)
  "Return the list of brew command lines executing PLAN with FLAGS.
PLAN is an alist mapping the symbols `install', `upgrade' and
`uninstall' to lists of package IDs, each a (NAME . TYPE) cons
with TYPE being `formula' or `cask'.  FLAGS is a list of option
strings; each flag is passed only to the commands that support
it.  Each returned command line is a list of brew arguments."
  (let (commands)
    (pcase-dolist (`(,action . ,ids) plan)
      (dolist (type '(formula cask))
        (let ((names (mapcar #'car
                             (seq-filter (lambda (id) (eq (cdr id) type))
                                         ids))))
          (when names
            (push (append (list (symbol-name action)
                                (if (eq type 'formula) "--formula" "--cask"))
                          (brew--command-flags action type flags)
                          names)
                  commands)))))
    (nreverse commands)))

(defun brew--plan-description (plan)
  "Return a short human-readable summary of PLAN."
  (mapconcat #'identity
             (delq nil
                   (mapcar (pcase-lambda (`(,action . ,ids))
                             (when ids
                               (format "%s: %s"
                                       (capitalize (symbol-name action))
                                       (mapconcat #'car ids ", "))))
                           plan))
             " · "))

;;;; Package list mode

(defvar-local brew--entries nil
  "All tabulated-list entries of the current view, before filtering.")

(defvar-local brew--raw nil
  "Hash table mapping package IDs to their raw brew info alists.")

(defvar-local brew--view 'installed
  "Current view: the symbol `installed', a (search . TERM) or (tap . TAP) cons.")

(defvar-local brew--filters nil
  "Active filters as an alist with the keys `name', `status' and `type'.")

(defvar-local brew--marks nil
  "Marked packages as an alist mapping package IDs to action symbols.")

(defvar-local brew--generation 0
  "Counter invalidating stale asynchronous responses.
Incremented whenever a refresh or search is started; responses
carrying an older generation are dropped.")

(defconst brew--mark-tags
  '((install . "I") (upgrade . "U") (uninstall . "D"))
  "Alist mapping mark actions to their line tags.")

(defun brew--print-entry (id cols)
  "Print the package entry ID with COLS and restore its mark tag.
Used as `tabulated-list-printer' so that marks survive any
reprint, including sorting by column."
  (tabulated-list-print-entry id cols)
  (when-let* ((action (cdr (assoc id brew--marks))))
    (save-excursion
      (forward-line -1)
      (tabulated-list-put-tag (cdr (assq action brew--mark-tags))))))

(defvar brew-package-list-buffer-name "*brew-packages*"
  "Name of the Homebrew package list buffer.")

(defvar brew-package-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'brew-mark-install)
    (define-key map (kbd "d") #'brew-mark-uninstall)
    (define-key map (kbd "U") #'brew-mark-upgrades)
    (define-key map (kbd "u") #'brew-unmark)
    (define-key map (kbd "DEL") #'brew-unmark-backward)
    (define-key map (kbd "x") #'brew-execute)
    (define-key map (kbd "P") #'brew-pin-toggle)
    (define-key map (kbd "s") #'brew-search)
    (define-key map (kbd "b") #'brew-browse-homepage)
    (define-key map (kbd "RET") #'brew-package-info)
    (define-key map (kbd "r") #'revert-buffer)
    (define-key map (kbd "/ n") #'brew-filter-by-name)
    (define-key map (kbd "/ s") #'brew-filter-by-status)
    (define-key map (kbd "/ t") #'brew-filter-by-type)
    (define-key map (kbd "/ /") #'brew-filter-clear)
    (define-key map (kbd "?") #'brew-package-list-help)
    map)
  "Keymap for `brew-package-list-mode'.")

(define-derived-mode brew-package-list-mode tabulated-list-mode "Brew-Packages"
  "Major mode for browsing Homebrew packages.

Mark packages with \\[brew-mark-install], \\[brew-mark-uninstall] and
\\[brew-mark-upgrades], then execute the marks with \\[brew-execute].

\\{brew-package-list-mode-map}"
  (setq tabulated-list-format
        (vector '("Name" 28 t)
                '("Type" 8 t)
                '("Installed" 12 t)
                '("Latest" 12 t)
                (list "Status" 11 #'brew--status-sort-p)
                '("Description" 0 nil)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq tabulated-list-printer #'brew--print-entry)
  (setq-local revert-buffer-function #'brew--package-list-revert)
  (tabulated-list-init-header))

(defun brew--package-list-buffer ()
  "Return the package list buffer, creating it if necessary."
  (let ((buffer (get-buffer-create brew-package-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'brew-package-list-mode)
        (brew-package-list-mode)))
    buffer))

;;;###autoload
(defun brew-list-packages ()
  "Display a list of installed Homebrew packages."
  (interactive)
  (let ((buffer (brew--package-list-buffer)))
    (brew--fetch-installed buffer)
    (pop-to-buffer buffer)))

;;;###autoload
(defun brew-list-outdated ()
  "Display the Homebrew package list filtered to outdated packages."
  (interactive)
  (brew-list-packages)
  (with-current-buffer brew-package-list-buffer-name
    (setf (alist-get 'status brew--filters) "outdated")
    (brew--refresh-display)))

(defun brew--package-list-revert (&rest _args)
  "Refresh the package list buffer from brew."
  (brew--fetch-installed (current-buffer)))

(defun brew--update-if-needed-finish (failed)
  "Run the pending update continuations, then any queued brew commands.
Each continuation is called with FAILED."
  (let ((callbacks (nreverse brew--update-if-needed-callbacks)))
    (setq brew--update-if-needed-callbacks nil)
    (dolist (callback callbacks)
      (funcall callback failed))
    (unless brew--run-active
      (brew--run-next))))

(defun brew--update-if-needed (callback)
  "Update Homebrew's package definitions if they are stale.
Runs \"brew update-if-needed\", which is rate-limited by Homebrew
itself, and then calls CALLBACK with one argument: non-nil when
the update attempt failed.  Concurrent calls share one brew
process.  While a mutating brew command is active, the update is
skipped and CALLBACK is called right away."
  (cond
   (brew--run-active
    (funcall callback nil))
   (brew--update-if-needed-callbacks
    (push callback brew--update-if-needed-callbacks))
   (t
    (push callback brew--update-if-needed-callbacks)
    (brew--call '("update-if-needed")
                (lambda (_output) (brew--update-if-needed-finish nil))
                nil
                (lambda () (brew--update-if-needed-finish t))))))

(defun brew--fetch-installed (buffer)
  "Asynchronously fetch all installed packages and display them in BUFFER.
Refreshes the package definitions first (see `brew--update-if-needed')
so that the outdated statuses are current."
  (message "brew: refreshing package list...")
  (let ((generation (with-current-buffer buffer
                      (setq brew--generation (1+ brew--generation)))))
    (brew--update-if-needed
     (lambda (update-failed)
       (brew--call-json
        '("info" "--json=v2" "--installed")
        (lambda (json)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (= generation brew--generation)
                (brew--set-info json 'installed)
                (message "brew: %d packages%s" (length brew--entries)
                         (if update-failed " (update check failed)"
                           "")))))))))))

(defun brew--set-info (json view)
  "Populate the buffer-local package caches from brew info JSON.
VIEW is stored in `brew--view'."
  (let ((entries (brew--info-entries json))
        (alists (append (alist-get 'formulae json)
                        (alist-get 'casks json)))
        (raw (make-hash-table :test #'equal)))
    (seq-mapn (lambda (entry alist) (puthash (car entry) alist raw))
              entries alists)
    (setq brew--entries entries
          brew--raw raw
          brew--view view)
    (setq brew--marks (seq-filter (lambda (mark) (gethash (car mark) raw))
                                  brew--marks))
    (brew--refresh-display)))

(defun brew--entry-visible-p (entry filters)
  "Return non-nil if ENTRY passes all FILTERS."
  (let ((vec (cadr entry)))
    (and (let ((name (alist-get 'name filters)))
           (or (not name) (string-match-p name (aref vec 0))))
         (let ((status (alist-get 'status filters)))
           (or (not status) (equal status (brew--entry-status entry))))
         (let ((type (alist-get 'type filters)))
           (or (not type) (equal type (aref vec 1)))))))

(defun brew--refresh-display ()
  "Recompute `tabulated-list-entries' from the cache and redisplay."
  (setq tabulated-list-entries
        (seq-filter (lambda (entry) (brew--entry-visible-p entry brew--filters))
                    brew--entries))
  (tabulated-list-print t)
  (brew--update-mode-line))

(defun brew--filters-description ()
  "Return a short description of the active filters, or nil."
  (when brew--filters
    (mapconcat (pcase-lambda (`(,key . ,value))
                 (format "%s%s%s" key (if (eq key 'name) "~" "=") value))
               brew--filters ", ")))

(defun brew--update-mode-line ()
  "Update the mode line with package counts and active filters."
  (let ((installed (seq-count
                    (lambda (entry)
                      (not (equal (brew--entry-status entry) "available")))
                    brew--entries))
        (outdated (seq-count
                   (lambda (entry)
                     (equal (brew--entry-status entry) "outdated"))
                   brew--entries))
        (filters (brew--filters-description)))
    (setq mode-line-process
          (concat
           (pcase brew--view
             ('installed (format ": %d installed, %d outdated"
                                 installed outdated))
             (`(search . ,term) (format ": search %S, %d results"
                                        term (length tabulated-list-entries)))
             (`(tap . ,tap) (format ": tap %s, %d packages"
                                    tap (length tabulated-list-entries))))
           (when brew--marks (format ", %d marked" (length brew--marks)))
           (when filters (format " [%s]" filters))))
    (force-mode-line-update)))

;;;; Marking

(defun brew--status-at-point ()
  "Return the status string of the package at point, or nil."
  (when-let* ((entry (tabulated-list-get-entry)))
    (substring-no-properties (aref entry 4))))

(defun brew--set-mark (action)
  "Set the mark of the package at point to ACTION, or clear it if nil."
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No package on this line"))
    (setq brew--marks (assoc-delete-all id brew--marks))
    (when action (push (cons id action) brew--marks))
    (tabulated-list-put-tag
     (if action (cdr (assq action brew--mark-tags)) " "))
    (brew--update-mode-line)))

(defun brew-mark-install ()
  "Mark the package at point for installation."
  (interactive)
  (unless (equal (brew--status-at-point) "available")
    (user-error "Package at point is already installed"))
  (brew--set-mark 'install)
  (forward-line 1))

(defun brew-mark-uninstall ()
  "Mark the package at point for uninstallation."
  (interactive)
  (when (equal (brew--status-at-point) "available")
    (user-error "Package at point is not installed"))
  (brew--set-mark 'uninstall)
  (forward-line 1))

(defun brew-mark-upgrades ()
  "Mark all outdated packages in the buffer for upgrade."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (equal (brew--status-at-point) "outdated")
          (brew--set-mark 'upgrade)
          (setq count (1+ count)))
        (forward-line 1)))
    (message "%d package%s marked for upgrade"
             count (if (= count 1) "" "s"))))

(defun brew-unmark ()
  "Remove the mark of the package at point and move down."
  (interactive)
  (brew--set-mark nil)
  (forward-line 1))

(defun brew-unmark-backward ()
  "Move up one line and remove the mark of the package there."
  (interactive)
  (forward-line -1)
  (brew--set-mark nil))

;;;; Execute

(defvar brew--pending-plan nil
  "The plan the `brew-execute' transient is about to run.")

(defvar brew--pending-buffer nil
  "The package list buffer that invoked the `brew-execute' transient.")

(defun brew--current-plan ()
  "Return the execution plan for the current package list buffer.
The plan is built from the marked packages, or, when nothing is
marked, from the package at point.  It is an alist mapping action
symbols to lists of package IDs; see `brew--plan-commands'."
  (if brew--marks
      (let (install upgrade uninstall)
        (pcase-dolist (`(,id . ,action) brew--marks)
          (pcase action
            ('install (push id install))
            ('upgrade (push id upgrade))
            ('uninstall (push id uninstall))))
        (seq-filter #'cdr
                    (list (cons 'install (nreverse install))
                          (cons 'upgrade (nreverse upgrade))
                          (cons 'uninstall (nreverse uninstall)))))
    (when-let* ((id (tabulated-list-get-id)))
      (if (equal (brew--status-at-point) "available")
          (list (cons 'install (list id)))
        (list (cons 'uninstall (list id)))))))

(transient-define-suffix brew-execute-run (args)
  "Run the brew commands assembled from the pending plan."
  :description (lambda ()
                 (concat "Run — " (brew--plan-description brew--pending-plan)))
  (interactive (list (transient-args 'brew-execute)))
  (let* ((plan brew--pending-plan)
         (buffer brew--pending-buffer)
         (dry-run (member "--dry-run" args))
         (commands (brew--plan-commands plan args))
         (last-command (car (last commands))))
    (unless commands (user-error "Nothing to do"))
    (dolist (command commands)
      (brew--run command
                 (when (eq command last-command)
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (unless dry-run (setq brew--marks nil))
                         (revert-buffer)))))))))

;;;###autoload (autoload 'brew-execute "brew" nil t)
(transient-define-prefix brew-execute ()
  "Execute the marked package actions.
When no package is marked, act on the package at point: install
it when it is not installed, uninstall it otherwise."
  ["Options"
   ("g" "Greedy (also upgrade auto-updating casks)" "--greedy")
   ("z" "Zap (also remove cask config and caches)" "--zap")
   ("f" "Force" "--force")
   ("n" "Dry run" "--dry-run")]
  ["Execute"
   ("x" brew-execute-run)]
  (interactive)
  (unless (derived-mode-p 'brew-package-list-mode)
    (user-error "Not in a brew package list buffer"))
  (let ((plan (brew--current-plan)))
    (unless plan
      (user-error "No packages marked and no package at point"))
    (setq brew--pending-plan plan
          brew--pending-buffer (current-buffer)))
  (transient-setup 'brew-execute))

;;;; Package actions on point

(defun brew--id-at-point ()
  "Return the package ID at point or signal an error."
  (or (tabulated-list-get-id)
      (user-error "No package on this line")))

(defun brew--raw-at-point ()
  "Return the raw info alist of the package at point."
  (gethash (brew--id-at-point) brew--raw))

(defun brew--revert-package-list ()
  "Revert the package list buffer if it exists."
  (when-let* ((buffer (get-buffer brew-package-list-buffer-name)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun brew-pin-toggle ()
  "Pin the formula at point, or unpin it when it is pinned."
  (interactive)
  (let ((id (brew--id-at-point)))
    (unless (eq (cdr id) 'formula)
      (user-error "Only formulae can be pinned"))
    (let ((pinned (alist-get 'pinned (brew--raw-at-point))))
      (brew--run (list (if pinned "unpin" "pin") (car id))
                 #'brew--revert-package-list))))

(defun brew-browse-homepage ()
  "Browse the homepage of the package at point."
  (interactive)
  (let ((homepage (alist-get 'homepage (brew--raw-at-point))))
    (unless homepage
      (user-error "Package at point has no homepage"))
    (browse-url homepage)))

;;;; Package info buffer

(defun brew--info-insert-field (label value)
  "Insert an info line with LABEL and VALUE unless VALUE is empty."
  (when (and value (not (equal value "")))
    (insert (propertize (format "%-14s" (concat label ":")) 'face 'bold)
            value "\n")))

(defun brew--render-info (id raw)
  "Insert details for the package ID from its RAW info alist."
  (let ((type (cdr id)))
    (let-alist raw
      (insert (propertize (car id) 'face 'bold)
              "  (" (symbol-name type) ")\n\n")
      (brew--info-insert-field "Description" .desc)
      (when .name
        (brew--info-insert-field
         "Full name" (if (listp .name) (string-join .name ", ") .name)))
      (when .homepage
        (insert (propertize (format "%-14s" "Homepage:") 'face 'bold))
        (insert-text-button .homepage
                            'action (lambda (_button) (browse-url .homepage))
                            'follow-link t
                            'help-echo "Browse this URL")
        (insert "\n"))
      (brew--info-insert-field "Tap" .tap)
      (pcase type
        ('formula
         (brew--info-insert-field "Version" .versions.stable)
         (when-let* ((installed (car .installed)))
           (brew--info-insert-field
            "Installed"
            (concat (alist-get 'version installed)
                    (unless (alist-get 'installed_on_request installed)
                      " (as dependency)"))))
         (when .pinned (brew--info-insert-field "Pinned" "yes"))
         (brew--info-insert-field "Dependencies"
                                  (when .dependencies
                                    (string-join .dependencies ", "))))
        ('cask
         (brew--info-insert-field "Version" .version)
         (brew--info-insert-field "Installed" .installed)
         (when .auto_updates
           (brew--info-insert-field "Auto-updates" "yes"))))
      (when .outdated (brew--info-insert-field "Outdated" "yes"))
      (when .deprecated
        (brew--info-insert-field
         "Deprecated" (or .deprecation_reason "yes")))
      (when .caveats
        (insert "\n" (propertize "Caveats:" 'face 'bold) "\n"
                (string-trim-right .caveats) "\n")))))

(defun brew-package-info ()
  "Show details for the package at point in a separate buffer."
  (interactive)
  (let* ((id (brew--id-at-point))
         (raw (brew--raw-at-point))
         (buffer (get-buffer-create (format "*brew-info: %s*" (car id)))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (brew--render-info id raw)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;;; Search

;;;###autoload
(defun brew-search (term)
  "Search Homebrew for TERM and show the results in the package list."
  (interactive "sSearch Homebrew: ")
  (unless (derived-mode-p 'brew-package-list-mode)
    (brew-list-packages))
  (let ((buffer (current-buffer))
        (generation (setq brew--generation (1+ brew--generation))))
    (message "brew: searching for %s..." term)
    (brew--call-lines
     (list "search" "--formula" term)
     (lambda (formulae)
       (brew--call-lines
        (list "search" "--cask" term)
        (lambda (casks)
          (brew--search-enrich buffer generation term formulae casks))
        (lambda ()
          (brew--search-enrich buffer generation term formulae nil))))
     (lambda ()
       (brew--call-lines
        (list "search" "--cask" term)
        (lambda (casks)
          (brew--search-enrich buffer generation term nil casks))
        (lambda ()
          (brew--search-enrich buffer generation term nil nil)))))))

(defun brew--search-enrich (buffer generation term formulae casks)
  "Fetch details for the TERM search results and display them in BUFFER.
FORMULAE and CASKS are the lists of matching package names.  The
results are dropped when GENERATION no longer matches the
buffer's `brew--generation'."
  (let* ((names (delete-dups (append formulae casks)))
         (total (length names))
         (names (seq-take names brew-search-max-results)))
    (if (null names)
        (message "brew: no packages found for %s" term)
      (when (> total (length names))
        (message "brew: showing %d of %d results" (length names) total))
      (brew--call-json
       (append '("info" "--json=v2") names)
       (lambda (json)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (= generation brew--generation)
               (brew--set-info json (cons 'search term))
               (message "brew: %d results for %s"
                        (length brew--entries) term)))))))))

;;;; Filters

(defun brew--set-filter (key value)
  "Set the filter KEY to VALUE (clear it when VALUE is nil) and redisplay."
  (setq brew--filters (assq-delete-all key brew--filters))
  (when value (push (cons key value) brew--filters))
  (brew--refresh-display))

(defun brew-filter-by-name (regexp)
  "Only show packages whose name matches REGEXP."
  (interactive "sFilter by name (regexp): ")
  (brew--set-filter 'name (unless (equal regexp "") regexp)))

(defun brew-filter-by-status (status)
  "Only show packages with STATUS."
  (interactive (list (completing-read "Filter by status: "
                                      brew--statuses nil t)))
  (brew--set-filter 'status (unless (equal status "") status)))

(defun brew-filter-by-type (type)
  "Only show packages of TYPE (formula or cask)."
  (interactive (list (completing-read "Filter by type: "
                                      '("formula" "cask") nil t)))
  (brew--set-filter 'type (unless (equal type "") type)))

(defun brew-filter-clear ()
  "Remove all filters."
  (interactive)
  (setq brew--filters nil)
  (brew--refresh-display))

;;;; Install, uninstall and maintenance commands

(defun brew--available-packages ()
  "Return a hash table of all available package names.
The names are read from Homebrew's cached API name lists (see
`brew-api-cache-directory').  Values are the symbols `formula',
`cask' or `both'."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,file . ,type) '(("formula_names.txt" . formula)
                                      ("cask_names.txt" . cask)))
      (let ((path (expand-file-name file brew-api-cache-directory)))
        (when (file-readable-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (dolist (name (split-string (buffer-string) "\n" t))
              (puthash name (if (gethash name table) 'both type) table))))))
    table))

(defun brew--read-available-package ()
  "Read an available package name with completion.
Return a (NAME . TYPE) cons where TYPE is the symbol `formula',
`cask', `both' or nil when the name is not in the cached lists."
  (let* ((table (brew--available-packages))
         (name
          (if (zerop (hash-table-count table))
              (read-string "Install package: ")
            (let ((completion-extra-properties
                   (list :annotation-function
                         (lambda (name)
                           (pcase (gethash name table)
                             ('formula "  formula")
                             ('cask "  cask")
                             ('both "  formula, cask"))))))
              (completing-read "Install package: " table)))))
    (cons name (gethash name table))))

;;;###autoload
(defun brew-install (package)
  "Install the Homebrew PACKAGE.
Interactively, complete over all available formulae and casks
from Homebrew's cached API name lists.  PACKAGE is a name string
or a (NAME . TYPE) cons as returned by
`brew--read-available-package'."
  (interactive (list (brew--read-available-package)))
  (pcase-let ((`(,name . ,type) (if (consp package)
                                    package
                                  (cons package nil))))
    (brew--run (if (eq type 'cask)
                   (list "install" "--cask" name)
                 (list "install" name))
               #'brew--revert-package-list)))

(defun brew--installed-names ()
  "Return the names of all installed formulae and casks.
This calls brew synchronously and may take a second."
  (append (process-lines brew-executable "list" "--formula")
          (process-lines brew-executable "list" "--cask")))

;;;###autoload
(defun brew-uninstall (name)
  "Uninstall the Homebrew package NAME."
  (interactive (list (completing-read "Uninstall package: "
                                      (brew--installed-names) nil t)))
  (brew--run (list "uninstall" name) #'brew--revert-package-list))

;;;###autoload
(defun brew-update ()
  "Update Homebrew itself and all package definitions."
  (interactive)
  (brew--run '("update")))

;;;###autoload
(defun brew-autoremove ()
  "Uninstall formulae that were only installed as dependencies."
  (interactive)
  (brew--run '("autoremove") #'brew--revert-package-list))

;;;###autoload
(defun brew-doctor ()
  "Check the Homebrew installation for potential problems."
  (interactive)
  (brew--run '("doctor")))

(defun brew-upgrade-all-run (args)
  "Upgrade all outdated Homebrew packages with option ARGS."
  (interactive (list (transient-args 'brew-upgrade-all)))
  (brew--run (cons "upgrade" args) #'brew--revert-package-list))

;;;###autoload (autoload 'brew-upgrade-all "brew" nil t)
(transient-define-prefix brew-upgrade-all ()
  "Upgrade all outdated Homebrew packages."
  ["Options"
   ("g" "Greedy (also upgrade auto-updating casks)" "--greedy")
   ("n" "Dry run" "--dry-run")]
  ["Upgrade"
   ("u" "Upgrade all outdated packages" brew-upgrade-all-run)])

(defun brew-cleanup-run (args)
  "Run brew cleanup with option ARGS."
  (interactive (list (transient-args 'brew-cleanup)))
  (brew--run (cons "cleanup" args)))

;;;###autoload (autoload 'brew-cleanup "brew" nil t)
(transient-define-prefix brew-cleanup ()
  "Remove stale lock files, outdated downloads and old versions."
  ["Options"
   ("p" "Remove all cache files" "--prune=all")
   ("s" "Scrub the download cache" "-s")
   ("n" "Dry run" "--dry-run")]
  ["Cleanup"
   ("c" "Run cleanup" brew-cleanup-run)])

;;;; Services

(defvar brew-services-buffer-name "*brew-services*"
  "Name of the Homebrew services buffer.")

(defvar brew-services-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'brew-services-start)
    (define-key map (kbd "o") #'brew-services-stop)
    (define-key map (kbd "r") #'brew-services-restart)
    (define-key map (kbd "RET") #'brew-services-visit-file)
    (define-key map (kbd "?") #'brew-services-help)
    map)
  "Keymap for `brew-services-mode'.")

(defconst brew--service-status-faces
  '(("started" . success)
    ("scheduled" . success)
    ("error" . error)
    ("none" . shadow))
  "Alist mapping service status strings to faces.")

(defun brew--service-entry (service)
  "Return a tabulated-list entry for the SERVICE alist."
  (let-alist service
    (let ((status (or .status "none")))
      (list .name
            (vector .name
                    (propertize
                     (if .exit_code
                         (format "%s (%s)" status .exit_code)
                       status)
                     'face (or (cdr (assoc status brew--service-status-faces))
                               'warning))
                    (or .user "")
                    (or .file ""))))))

(define-derived-mode brew-services-mode tabulated-list-mode "Brew-Services"
  "Major mode for managing Homebrew services.

\\{brew-services-mode-map}"
  (setq tabulated-list-format
        (vector '("Name" 24 t)
                '("Status" 12 t)
                '("User" 10 t)
                '("File" 0 nil)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq-local revert-buffer-function #'brew--services-revert)
  (tabulated-list-init-header))

;;;###autoload
(defun brew-services ()
  "Display a list of Homebrew services."
  (interactive)
  (let ((buffer (get-buffer-create brew-services-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'brew-services-mode)
        (brew-services-mode))
      (brew--services-revert))
    (pop-to-buffer buffer)))

(defun brew--services-revert (&rest _args)
  "Refresh the services buffer from brew."
  (let ((buffer (current-buffer)))
    (message "brew: refreshing services...")
    (brew--call-json
     '("services" "list" "--json")
     (lambda (services)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq tabulated-list-entries
                 (mapcar #'brew--service-entry services))
           (tabulated-list-print t)
           (message "brew: %d services" (length services))))))))

(defun brew--services-action (action)
  "Run the brew services ACTION on the service at point."
  (let ((name (or (tabulated-list-get-id)
                  (user-error "No service on this line")))
        (buffer (current-buffer)))
    (brew--run (list "services" action name)
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (revert-buffer)))))))

(defun brew-services-start ()
  "Start the service at point."
  (interactive)
  (brew--services-action "start"))

(defun brew-services-stop ()
  "Stop the service at point."
  (interactive)
  (brew--services-action "stop"))

(defun brew-services-restart ()
  "Restart the service at point."
  (interactive)
  (brew--services-action "restart"))

(defun brew-services-visit-file ()
  "Visit the launchd plist or systemd unit file of the service at point."
  (interactive)
  (let* ((entry (or (tabulated-list-get-entry)
                    (user-error "No service on this line")))
         (file (aref entry 3)))
    (cond ((equal file "")
           (user-error "Service has no file"))
          ((not (file-exists-p file))
           (user-error "Service file %s does not exist" file))
          (t (find-file file)))))

;;;; Taps

(defvar brew-taps-buffer-name "*brew-taps*"
  "Name of the Homebrew taps buffer.")

(defvar brew-taps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'brew-taps-add)
    (define-key map (kbd "d") #'brew-taps-remove)
    (define-key map (kbd "RET") #'brew-taps-show-packages)
    (define-key map (kbd "?") #'brew-taps-help)
    map)
  "Keymap for `brew-taps-mode'.")

(define-derived-mode brew-taps-mode tabulated-list-mode "Brew-Taps"
  "Major mode for managing Homebrew taps.

\\{brew-taps-mode-map}"
  (setq tabulated-list-format (vector '("Tap" 40 t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Tap" nil))
  (setq-local revert-buffer-function #'brew--taps-revert)
  (tabulated-list-init-header))

;;;###autoload
(defun brew-taps ()
  "Display a list of Homebrew taps."
  (interactive)
  (let ((buffer (get-buffer-create brew-taps-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'brew-taps-mode)
        (brew-taps-mode))
      (brew--taps-revert))
    (pop-to-buffer buffer)))

(defun brew--taps-revert (&rest _args)
  "Refresh the taps buffer from brew."
  (let ((buffer (current-buffer)))
    (brew--call-lines
     '("tap")
     (lambda (taps)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq tabulated-list-entries
                 (mapcar (lambda (tap)
                           (list tap
                                 (vector
                                  (cons tap
                                        (list 'action #'brew--taps-button-action
                                              'follow-link t)))))
                         taps))
           (tabulated-list-print t)))))))

(defun brew--revert-taps-buffer ()
  "Revert the taps buffer if it exists."
  (when-let* ((buffer (get-buffer brew-taps-buffer-name)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun brew-taps-add (tap)
  "Add the tap named TAP (in \"user/repo\" form)."
  (interactive "sTap (user/repo): ")
  (brew--run (list "tap" tap) #'brew--revert-taps-buffer))

(defun brew-taps-remove ()
  "Remove the tap at point."
  (interactive)
  (let ((tap (or (tabulated-list-get-id)
                 (user-error "No tap on this line"))))
    (when (yes-or-no-p (format "Untap %s? " tap))
      (brew--run (list "untap" tap) #'brew--revert-taps-buffer))))

(defun brew--tap-package-names (json)
  "Return the package names listed in the tap-info JSON array."
  (delete-dups (append (alist-get 'formula_names (car json))
                       (alist-get 'cask_tokens (car json)))))

(defun brew--tap-enrich (buffer generation tap names)
  "Fetch details for the TAP packages NAMES and display them in BUFFER.
The results are dropped when GENERATION no longer matches the
buffer's `brew--generation'."
  (let* ((total (length names))
         (names (seq-take names brew-tap-max-packages)))
    (if (null names)
        (message "brew: no packages in tap %s" tap)
      (when (> total (length names))
        (message "brew: showing %d of %d packages" (length names) total))
      (brew--call-json
       (append '("info" "--json=v2") names)
       (lambda (json)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (= generation brew--generation)
               (brew--set-info json (cons 'tap tap))
               (message "brew: %d packages in tap %s"
                        (length brew--entries) tap)))))))))

(defun brew-taps-show-packages (tap)
  "Show the packages provided by TAP in the package list."
  (interactive
   (list (or (tabulated-list-get-id)
             (user-error "No tap on this line"))))
  (let* ((buffer (brew--package-list-buffer))
         (generation (with-current-buffer buffer
                       (setq brew--generation (1+ brew--generation)))))
    (message "brew: listing packages in tap %s..." tap)
    (pop-to-buffer buffer)
    (brew--call-json
     (list "tap-info" tap "--json")
     (lambda (json)
       (brew--tap-enrich buffer generation tap
                         (brew--tap-package-names json))))))

(defun brew--taps-button-action (button)
  "Show the packages of the tap named by BUTTON."
  (brew-taps-show-packages (tabulated-list-get-id (button-start button))))

;;;; Help transients

;;;###autoload (autoload 'brew-package-list-help "brew" nil t)
(transient-define-prefix brew-package-list-help ()
  "List the commands of `brew-package-list-mode'."
  [["Marks"
    ("i" "Mark for install" brew-mark-install)
    ("d" "Mark for uninstall" brew-mark-uninstall)
    ("U" "Mark all upgrades" brew-mark-upgrades)
    ("u" "Unmark" brew-unmark)
    ("x" "Execute marks…" brew-execute)]
   ["Package at point"
    ("RET" "Show details" brew-package-info)
    ("b" "Browse homepage" brew-browse-homepage)
    ("P" "Pin/unpin" brew-pin-toggle)]
   ["Buffer"
    ("s" "Search Homebrew…" brew-search)
    ("g" "Refresh" revert-buffer)
    ("/ n" "Filter by name" brew-filter-by-name)
    ("/ s" "Filter by status" brew-filter-by-status)
    ("/ t" "Filter by type" brew-filter-by-type)
    ("/ /" "Clear filters" brew-filter-clear)]])

;;;###autoload (autoload 'brew-services-help "brew" nil t)
(transient-define-prefix brew-services-help ()
  "List the commands of `brew-services-mode'."
  ["Services"
   ("s" "Start service" brew-services-start)
   ("o" "Stop service" brew-services-stop)
   ("r" "Restart service" brew-services-restart)
   ("RET" "Visit service file" brew-services-visit-file)
   ("g" "Refresh" revert-buffer)])

;;;###autoload (autoload 'brew-taps-help "brew" nil t)
(transient-define-prefix brew-taps-help ()
  "List the commands of `brew-taps-mode'."
  ["Taps"
   ("RET" "Show tap packages" brew-taps-show-packages)
   ("a" "Add tap…" brew-taps-add)
   ("d" "Remove tap" brew-taps-remove)
   ("g" "Refresh" revert-buffer)])

;;;; Top-level dispatcher

;;;###autoload (autoload 'brew "brew" nil t)
(transient-define-prefix brew ()
  "Manage Homebrew packages, services and taps."
  [["Buffers"
    ("l" "Packages" brew-list-packages)
    ("o" "Outdated packages" brew-list-outdated)
    ("s" "Services" brew-services)
    ("t" "Taps" brew-taps)]
   ["Packages"
    ("i" "Install…" brew-install)
    ("u" "Upgrade all…" brew-upgrade-all)
    ("d" "Uninstall…" brew-uninstall)]
   ["Maintenance"
    ("U" "Update" brew-update)
    ("c" "Cleanup…" brew-cleanup)
    ("A" "Autoremove" brew-autoremove)
    ("D" "Doctor" brew-doctor)]])

(provide 'brew)
;;; brew.el ends here
