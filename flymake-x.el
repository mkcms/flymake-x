;;; flymake-x.el --- Simple flymake checker definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools
;; Version: 0.1.0
;; Homepage: https://github.com/mkcms/flymake-x
;; Package-Requires: ((emacs "27.1") (flymake "1.0"))

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

;;
;;; flymake-x
;;
;; Package that provides an easy way to define flymake checkers.
;;
;; For example, a pycodestyle checker can be defined like this:
;;
;;  (pycodestyle
;;    :class pipe
;;    :modes (python-mode)
;;    :command "pycodestyle -"
;;    :error-patterns
;;    ((:warning
;;      (bol file ":" line ":" column ": " (message (zero-or-more nonl)) eol)))
;;
;;; Usage
;;
;; Call `flymake-x-setup' once, and modify `flymake-x-checkers'.  See it's
;; documentation for details.
;;
;; You can also load the included \\='flymake-x-sample-checkers\\=' library
;; which contains a few predefined checkers.
;;
;; For easily defining new checkers, a helpful command is provided to display
;; the checker output, found diagnostics and other information:
;; `flymake-x-debug'.  It should be called with the name of a checker or it's
;; definition, as described in `flymake-x-checkers'.  Another useful command is
;; `flymake-x-construct-pattern-interactively', which aids in constructing the
;; error patterns.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 're-builder)
(require 'rx)
(require 'subr-x)
(require 'eieio)
(require 'map)

(defvar-local flymake-x-buffer-checkers nil
  "List of checkers that were started for current buffer.")

(defun flymake-x-checker-obsolete-p (checker)
  "Return non-nil if CHECKER is obsolete."
  (with-current-buffer (flymake-x-checker-buffer checker)
    (not (memq checker flymake-x-buffer-checkers))))


;; Base checker class

(defclass flymake-x-checker ()
  ((name
    :initarg :name
    :accessor flymake-x-checker-name
    :documentation "Name of this checker.")
   (buffer
    :initarg :buffer
    :accessor flymake-x-checker-buffer
    :documentation "The buffer for which this checker was started.")
   (report-fn
    :initarg :report-fn
    :accessor flymake-x-checker-report-fn
    :documentation
    "Function which should be called to report errors in the buffer."))
  :abstract t
  :documentation "Objects of this class report diagnostics/errors in a buffer.

Each object has a name, a buffer and report-function.  The
checker should implement `flymake-x-start' and `flymake-x-stop'
functions.")

(cl-defgeneric flymake-x-start (checker)
  "Start CHECKER.")

(cl-defgeneric flymake-x-stop (checker)
  "Stop CHECKER.")

(cl-defmethod slot-unbound ((_object flymake-x-checker) _class _slot-name _fn)
  "Override `slot-unbound' error for all checker objects.
We override it to return nil in that case."
  nil)


;; Process checker

(defclass flymake-x-process-checker (flymake-x-checker)
  ((command
    :initarg :command
    :accessor flymake-x-checker-command
    :documentation
    "The shell command to run, or a lambda function that returns one.")
   (lines-start-from-0
    :initarg :lines-start-from-0
    :accessor flymake-x-checker-lines-start-from-0-p
    :documentation "If non-nil, assume lines start from 0.")
   (columns-start-from-1
    :initarg :columns-start-from-1
    :accessor flymake-x-checker-columns-start-from-1-p
    :documentation "If non-nil, assume columns start from 1.")
   (stderr
    :initarg :stderr
    :initform nil
    :accessor flymake-x-checker-use-stderr-p
    :documentation "Use stderr instead of stdout")
   (process
    :accessor flymake-x-checker-process
    :documentation "The process object.")
   (error-patterns
    :accessor flymake-x-checker-error-patterns
    :initarg :error-patterns
    :documentation
    "List of error patterns as described in `flymake-x-checkers'."))
  :abstract t
  :documentation "Base class for process checkers.")

(defun flymake-x-process-output-buffer (checker)
  "Get the output buffer of CHECKER."
  (or (process-buffer (flymake-x-checker-process checker))
      (format " *stderr of flymake-x-%s*"
              (flymake-x-checker-name checker))))

(cl-defmethod initialize-instance
  :after ((instance flymake-x-process-checker) _slots) ;lint:ignore
  "Constructor for flymake-x process checker class."
  (when (functionp (flymake-x-checker-command instance))
    (setf (flymake-x-checker-command instance)
          (funcall (flymake-x-checker-command instance)))))

(cl-defmethod flymake-x-start ((checker flymake-x-process-checker))
  "Start a process CHECKER."
  (with-slots (name command stderr) checker
    (setq command (split-string-and-unquote command))
    (cond
     ((null (executable-find (car command)))
      (flymake-log :error
                   "Could not find program %S required for checker %s"
                   (car command) name)
      (flymake-x-stop checker))
     (t
      (let* ((stderr-buf (format " *stderr of flymake-x-%s*" name))
             (process (make-process
                       :name (format " *flymake-x-%s*" name)
                       :buffer (if stderr
                                   nil
                                 (generate-new-buffer
                                  (format " *flymake-x-%s*" name)))
                       :stderr stderr-buf
                       :command command)))
        (set-process-sentinel
         process
         (lambda (_proc _status)
           (when (buffer-live-p (flymake-x-checker-buffer checker))
             (condition-case err
                 (flymake-x--report-process-diagnostics checker)
               (error
                (flymake-log
                 :error "Error when processing output of checker %s: %s"
                 name err))))
           (ignore-errors
             (when-let* ((buf (flymake-x-process-output-buffer checker)))
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer buf))))
           (condition-case err (flymake-x-stop checker)
             (error (flymake-log
                     :error "Failed to stop checker %s: %s" name err)))))
        (setf (flymake-x-checker-process checker) process))))))

(cl-defmethod flymake-x-stop ((checker flymake-x-process-checker))
  "Stop a process CHECKER."
  (when-let* ((process (flymake-x-checker-process checker)))
    (when (process-live-p process)
      (kill-process process))))

(eval-and-compile
  (defvar flymake-x--rx-base-group 10))

(defmacro flymake-x--rx-let-eval (&rest body)
  "Evaluate BODY with extended rx forms supported by flymake-x.
Special variables are available in BODY:

- \\='line-group\\='
- \\='column-group\\='
- \\='message-group\\='
- \\='file-group\\='

These variables hold the regexp group numbers that would be matched by
flymake-x patterns."
  (declare (indent 0) (debug (body)))
  (let* ((line flymake-x--rx-base-group)
         (column (1+ line))
         (message (1+ column))
         (file (1+ message)))
    `(let ((line-group ,line)
           (column-group ,column)
           (message-group ,message)
           (file-group ,file))
       ;; Prevent warning about unused variables.
       (ignore line-group)
       (ignore column-group)
       (ignore message-group)
       (ignore file-group)
       (rx-let-eval '((line (group-n ,line (one-or-more digit)))
                      (column (group-n ,column (one-or-more digit)))
                      (message (&rest pat) (group-n ,message pat))
                      (file (group-n ,file (zero-or-more nonl))))
         ,@body))))

(defun flymake-x--patterns-search (patterns)
  "Search for rx PATTERNS in current buffer and return a match.

PATTERNS should be a list of `rx' sexps to match.  They can
contain these additional RX constructs:

  line             matches [0-9]+ and is used to extract line number
  column           matches [0-9]+ and is used to extract column number
  (message PAT)    matches PAT and is used to extract the error message
  file             matches .* and is used to extract the file name

The patterns are searched in order, line-by-line from point to
the end of buffer.

The return value is non-nil if match for any pattern was found.
It is single MATCH:

  (INDEX LINE COLUMN MESSAGE)

where:
INDEX is the 0-based index of the pattern,
LINE is either nil or the line number matched by the pattern,
COLUMN is either nil or the column matched by the pattern,
MESSAGE is either nil or the message matched by the pattern."
  (save-match-data
    (flymake-x--rx-let-eval
      (let (retval start)
        (while (and (not (eobp)) (null retval))
          (setq start (point))
          (setq retval
                (cl-loop
                 for pattern in patterns
                 for i from 0
                 if (looking-at (rx-to-string `(seq ,@pattern)))
                 return
                 (list i
                       (when-let* ((line (match-string line-group)))
                         (string-to-number line))
                       (when-let* ((column (match-string column-group)))
                         (string-to-number column))
                       (when-let* ((message (match-string message-group)))
                         message))))
          (if retval
              (goto-char (match-end 0))
            (forward-line 1)
            (beginning-of-line)
            (when (<= (point) start)
              (goto-char (point-max)))))
        retval))))

(defun flymake-x--report-process-diagnostics (checker)
  "For process CHECKER, check it's output buffer and collect diagnostics."
  (when (and (not (flymake-x-checker-obsolete-p checker))
             (flymake-x-process-output-buffer checker))
    (let* ((diagnostics '())
           (error-patterns (flymake-x-checker-error-patterns checker))
           (searchable-patterns (mapcar #'cadr error-patterns))
           (match nil))
      (with-current-buffer (flymake-x-process-output-buffer checker)
        (goto-char (point-min))
        (while (setq match (flymake-x--patterns-search searchable-patterns))
          (pcase-let* ((`(,index ,line ,column ,message) match)
                       (`(,type _ ,pattern-message) (nth index error-patterns))
                       (beg) (end))
            (setq message (or message pattern-message))
            (with-current-buffer (flymake-x-checker-buffer checker)
              (save-excursion
                (goto-char (point-min))
                (when line
                  (unless (flymake-x-checker-lines-start-from-0-p checker)
                    (cl-decf line))
                  (forward-line line))
                (setq beg (line-beginning-position))
                (setq end (line-end-position))
                (when column
                  (when (flymake-x-checker-columns-start-from-1-p checker)
                    (cl-decf column))
                  (cond
                   ((< (+ (point) column) (line-end-position))
                    (goto-char (+ (point) column))
                    (if-let* ((bounds (bounds-of-thing-at-point 'sexp)))
                        (setq beg (car bounds) end (cdr bounds))
                      (setq beg (point))))
                   (t (setq beg (point)))))
                (when (and (= beg end) (< end (point-max)))
                  (setq end (1+ end)))
                (push (flymake-make-diagnostic
                       (current-buffer)
                       beg end
                       type
                       (format
                        "[%s] %s" (flymake-x-checker-name checker) message))
                      diagnostics)))))
        (funcall (flymake-x-checker-report-fn checker) diagnostics)))))


;; Pipe checker

(defclass flymake-x-pipe-checker (flymake-x-process-checker) ()
  :documentation "A process checker that accepts the input file via stdin.")

(cl-defmethod flymake-x-start
  :after ((checker flymake-x-pipe-checker)) ;lint:ignore
  "Start a pipe CHECKER."
  (when-let* ((process (flymake-x-checker-process checker)))
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)))


;; Checker with temporary file

(defclass flymake-x-temp-file-checker (flymake-x-process-checker)
  ((temp-file :accessor flymake-x-checker-temp-file))
  :documentation
  "A process checker that gets called with a temporary file.
The checked buffer is saved into a temporary file, and the path
to that file is appended to the checker's command line.  After
the checker process exits, the file is deleted.")

(cl-defmethod flymake-x-start
  :before ((checker flymake-x-temp-file-checker)) ;lint:ignore
  "Start a temp-file CHECKER."
  (when-let* ((buffer-file (buffer-file-name)))
    (let* ((directory (file-name-directory buffer-file))
           (ext (file-name-extension buffer-file))
           (temporary-file-directory directory)
           (tempfile (make-temp-file "flymake" nil (and ext
                                                        (concat "." ext)))))
      (write-region (point-min) (point-max) tempfile nil 'no-message)
      (setf (flymake-x-checker-temp-file checker) tempfile)
      (setf (flymake-x-checker-command checker)
            (concat (flymake-x-checker-command checker) " " tempfile)))))

(cl-defmethod flymake-x-stop
  :after ((checker flymake-x-temp-file-checker)) ;lint:ignore
  "Stop a temp-file CHECKER."
  (when-let* ((file (flymake-x-checker-temp-file checker)))
    (when (file-exists-p file) (delete-file file))))


;; Flymake interface

(defvar flymake-x-checkers)
(defvar-local flymake-x--reported-diagnostics ())

(defun flymake-x--cleanup ()
  "Ensure all checkers are stopped."
  (mapc (lambda (checker)
          (condition-case err
              (flymake-x-stop checker)
            (error (flymake-log
                    :error "Failed to stop checker %s: %s"
                    (flymake-x-checker-name checker) err))))
        flymake-x-buffer-checkers)
  (setq flymake-x-buffer-checkers nil)
  (setq flymake-x--reported-diagnostics nil))

(defun flymake-x--start-checkers (report-fn &rest _args)
  "Flymake diagnostic function using REPORT-FN.
This is the main function of `flymake-x'.  It starts the defined
checkers, gathers their diagnostics and reports them to REPORT-FN."
  (flymake-x--cleanup)
  (add-hook 'kill-buffer-hook #'flymake-x--cleanup nil t)
  (cl-loop
   for (name . plist) in flymake-x-checkers
   for class = (pcase (plist-get plist :class)
                 ('pipe 'flymake-x-pipe-checker)
                 ('temp-file 'flymake-x-temp-file-checker)
                 (value value))
   for modes = (plist-get plist :modes)
   when (or (null modes) (memq major-mode modes))
   do
   (condition-case err
       (push
        (apply #'make-instance class
               :name name
               :buffer (current-buffer)
               :report-fn
               (lambda (report-action &rest args)
                 (cond
                  ((listp report-action)
                   (setf (map-elt flymake-x--reported-diagnostics name)
                         report-action)
                   (funcall report-fn
                            (cl-reduce #'append
                                       flymake-x--reported-diagnostics
                                       :key #'cdr)))
                  (t
                   (apply report-fn report-action args))))

               (cl-loop for (k v) on plist by 'cddr
                        unless (memq k '(:modes :class))
                        nconc (list k v)))
        flymake-x-buffer-checkers)
     (error
      (flymake-log
       :error "Failed to instantiate checker %s: %s" name err))))
  (dolist (checker flymake-x-buffer-checkers)
    (condition-case err
        (flymake-x-start checker)
      (error
       (flymake-log :error "Failed to start checker %s: %s"
                    (flymake-x-checker-name checker) err)))))


;; Public interface

(defvar flymake-x-checkers '()
  "List of checkers to use.
Each checker should look like this:

  (NAME                           ; a symbol
    :class flymake-x-TYPE-checker
    :modes MODE-LIST
    :command \"COMMAND\"
    :stderr t
    :lines-start-from-0 t
    :columns-start-from-1 t
    :error-patterns
    ((:KIND
      RX-STYLE-CONSTRUCT)
     (:KIND
      RX-STYLE-CONSTRUCT)
     ...))

The keyword arguments to each checker are:

- :class CLASS

  Required.  This says how the checker process will accept input.
  This is one of:

    - \\='flymake-x-pipe-checker (or just \\='pipe)

    The checker will accept the buffer string via stdin.

    - \\='flymake-x-temp-file-checker (or just \\='temp-file)

    A temporary file will be created with the buffer contents,
    and it's path will be passed as an argument to the checker
    process.

- :command COMMAND

  Required.  This is the shell command (string) to run.  It can
  also be a function that accepts no arguments and returns the
  command string.

- :error-patterns PATTERNS

  Required.  This defines the patterns to search for in the
  checker process output.

  Each pattern is a list (KIND RX-PAT [MSG]).  KIND can be :note,
  :warning or :error.

  RX-PAT is an `rx'-style pattern (a sexp) and these additional
  `rx' constructs are available in it:

    - line

    matches \"[0-9]+\" and is used to extract line number

    - column

    matches \"[0-9]+\" and is used to extract column number

    - (message PAT) (or just message)

    matches PAT (\".*\" by default) and is used to extract the
    error message

    - file

    matches \".*\" and is used to extract the file name

  MSG can be provided to set the diagnostic message for the
  pattern.  Otherwise, it should be matched by the pattern.

  The patterns are searched in order, line-by-line from the
  beginning of buffer.

- :modes MODES

  Optional.  If this is present, only run the checker in given
  modes.  By default, a checker run in all modes.

- :stderr t

  Optional.  If this is non-nil, standard error of the checker
  process will be used to search for the error patterns instead
  of stdout.

- :lines-start-from-0 t

  Optional.  If non-nil, assume lines start from 0.

- :columns-start-from-1 t

  Optional.  If non-nil, assume columns start from 1.")

(defun flymake-x-setup (&optional this-buffer-only)
  "Enable flymake-x globally.
If THIS-BUFFER-ONLY, enable it only in this buffer instead."
  (add-hook 'flymake-diagnostic-functions #'flymake-x--start-checkers
            nil this-buffer-only))


;; Debugging

(defun flymake-x-construct-pattern-interactively (&optional initial)
  "Interactively construct patterns using `re-builder'.
INITIAL if given is the initial `rx'-style pattern for it.
The current buffer should contain the output of a checker program.  The
`re-builder' buffer is set up such that the custom `rx' constructs
for checkers (e.g. \\='file\\=') are available to use in it.

The patterns are constructed under `recursive-edit', so
\\[abort-recursive-edit] or \\[exit-recursive-edit] must be called to
exit from the command.

Interactively, when called with a prefix argument, prompts for the name
of a checker from `flymake-x-checkers' and then for a pattern kind, and
uses that as initial pattern to edit."
  (interactive
   (when current-prefix-arg
     (let* ((checker
             (intern
              (completing-read "Construct pattern for checker: "
                               (mapcar #'symbol-name
                                       (mapcar #'car flymake-x-checkers)))))
            (checker (map-elt flymake-x-checkers checker))
            (patterns (plist-get checker :error-patterns))
            (pattern-kind
             (intern
              (completing-read "Pattern to construct: "
                               (mapcar #'symbol-name
                                       (mapcar #'car patterns)))))
            (pattern (car (map-elt patterns pattern-kind))))
       (list (or (and (listp pattern) `(seq ,@pattern)) pattern)))))
  (let ((buf (current-buffer))
        (window (selected-window)))
    (flymake-x--rx-let-eval
      (re-builder)
      (unwind-protect
          (progn
            (setq reb-re-syntax 'rx)
            (setq reb-target-buffer buf)
            (setq reb-target-window window)
            (when initial
              (with-current-buffer reb-buffer
                (erase-buffer)
                (insert "'")
                (prin1 initial (current-buffer))))
            (recursive-edit)
            (message (substitute-command-keys
                      (concat "Use \\[abort-recursive-edit] or"
                              " \\[exit-recursive-edit] to exit."))))
        (reb-quit)))))

(defun flymake-x-debug (checker)
  "Debug CHECKER: run it and show a buffer displaying helpful information.
CHECKER should be either a symbol or a checker definition as described
in `flymake-x-checkers'.  If it is a symbol, then the checker definition
is looked up in `flymake-x-checkers' using that symbol.

The checker is run for the current buffer.
The displayed information includes:
- Process exit status
- Command that was run
- Found diagnostics and details
- Compiled error patterns."
  (interactive
   (list
    (intern
     (completing-read "Debug checker in this buffer: "
                      (mapcar #'symbol-name (mapcar #'car flymake-x-checkers))
                      nil t))))
  (when (symbolp checker)
    (setq checker (assoc checker flymake-x-checkers)))

  (flymake-x--cleanup)

  (let* ((flymake-x-checkers (list checker))
         (buf (current-buffer))
         (goto-pt (lambda (point)
                    (lambda (&rest _args)
                      (with-current-buffer buf
                        (pop-to-buffer (current-buffer))
                        (goto-char point)))))
         process
         stderr-buffer
         diagnostics
         sentinel
         process-output)
    (flymake-x--start-checkers (lambda (diags) (setq diagnostics diags)))
    (unless flymake-x-buffer-checkers
      (error (concat
              "No checkers were created for the current buffer;"
              " is the checker defined for another major mode?")))
    (setq checker (car flymake-x-buffer-checkers))
    (setq process (flymake-x-checker-process checker))

    (setq sentinel (process-sentinel process))
    (set-process-sentinel
     process
     (lambda (&rest args)
       (with-current-buffer (flymake-x-process-output-buffer checker)
         (setq process-output (buffer-string)))
       (apply sentinel args)))

    (ignore-errors (kill-buffer "*flymake-x debug process output*"))

    (with-current-buffer (get-buffer-create "*flymake-x debug*")
      (let ((inhibit-read-only t))
        (setq buffer-read-only t)
        (setq buffer-undo-list t)
        (setq truncate-lines t)
        (erase-buffer)

        (setq stderr-buffer (format " *stderr of flymake-x-%s*"
                                    (flymake-x-checker-name checker)))

        (save-current-buffer
          (while (accept-process-output process))
          (sit-for 0.1))

        (let ((standard-output (current-buffer)))
          (insert (format "Checker: %s\n" (flymake-x-checker-name checker)))
          (insert (format "Type: %s\n" (eieio-object-class checker)))
          (insert (format "Diagnostics from stderr: %s\n"
                          (if (flymake-x-checker-use-stderr-p checker)
                              "yes" "no")))
          (insert (format "Lines start from 0: %s\n"
                          (if (flymake-x-checker-lines-start-from-0-p checker)
                              "yes" "no")))
          (insert
           (format "Columns start from 1: %s\n"
                   (if (flymake-x-checker-columns-start-from-1-p checker)
                       "yes" "no")))
          (insert (format "Command: %S\n" (flymake-x-checker-command checker)))
          (insert (format "Process exit status: %s\n"
                          (process-exit-status process)))
          (insert (format "Output: %s chars " (length process-output)))
          (insert-button
           "[Show process output]" 'action
           (lambda (&rest _args)
             (with-current-buffer
                 (get-buffer-create "*flymake-x debug process output*")
               (let ((inhibit-read-only t))
                 (setq buffer-read-only t)
                 (setq buffer-undo-list t)
                 (erase-buffer)
                 (insert process-output)
                 (pop-to-buffer (current-buffer))
                 (special-mode)))))
          (insert "\n")
          (insert "Stderr: ")
          (insert-button
           "[Show stderr buffer]" 'action
           (lambda (&rest _args) (pop-to-buffer stderr-buffer)))
          (insert "\n")
          (insert "Error patterns:\n")
          (cl-loop for pat in (flymake-x-checker-error-patterns checker)
                   do (progn (insert "\n")
                             (insert "    ")
                             (prin1 pat)
                             (insert "\n")
                             (insert "    Which is:\n")
                             (insert
                              (format "    %S"
                                      (flymake-x--rx-let-eval
                                        (rx-to-string `(seq ,@(cadr pat))))))
                             (insert "\n")))
          (insert "\n")

          (insert (format "Diagnostics found: %s\n" (length diagnostics)))
          (pcase-dolist
              (`(,i . ,diag) (cl-loop for x in diagnostics
                                      for i from 0
                                      collect (cons i x)))
            (insert "\n")
            (insert (format "    Index: %d\n" i))
            (insert (format "    Type: %s\n"
                            (flymake-diagnostic-type diag)))

            (insert (format "    Beg: %s " (flymake-diagnostic-beg diag)))
            (insert-button "[Goto]" 'action
                           (funcall goto-pt (flymake-diagnostic-beg diag)))
            (insert "\n")
            (insert (format "    End: %s " (flymake-diagnostic-end diag)))
            (insert-button "[Goto]" 'action
                           (funcall goto-pt (flymake-diagnostic-end diag)))
            (insert "\n")

            (insert (format "    Text: %s\n"
                            (format "%S" (flymake-diagnostic-text diag))))
            )))
      (special-mode)
      (button-mode)
      (font-lock-add-keywords nil
                              '(("\\(:error\\)" 1 'error prepend)
                                ("\\(:warning\\)" 1 'warning prepend)
                                ("\\(:note\\)" 1 'flymake-note-echo prepend)))
      (setq-local font-lock-keywords-only nil)
      (font-lock-fontify-region (point-min) (point-max))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;; This code is only evaluated when linting.  It is used to ignore certain
;; unfixable warnings on older Emacs versions.
;;
;;lint: (progn
;;lint:   (require 'checkdoc)
;;lint:   (defvar lint-orig-checkdoc-create-error-function
;;lint:     checkdoc-create-error-function)
;;lint:   (defun lint-checkdoc-create-error-function (text start end &rest args)
;;lint:     "Create checkdoc error for TEXT START END ARGS."
;;lint:     (save-excursion
;;lint:       (goto-char start)
;;lint:       (goto-char (line-beginning-position))
;;lint:       (when (looking-at ".*;.*\\blint:ignore\\b")
;;lint:         (setq text (concat "__ignore: " text)))
;;lint:       (apply lint-orig-checkdoc-create-error-function text start end args)))
;;lint:   (setq checkdoc-create-error-function
;;lint:         #'lint-checkdoc-create-error-function))

(provide 'flymake-x)
;;; flymake-x.el ends here
