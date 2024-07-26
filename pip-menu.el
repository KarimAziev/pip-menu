;;; pip-menu.el --- Transient menu for the `pip` package installer -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/pip-menu
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (transient "0.7.2") (pyvenv "1.21") (project "0.11.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu for the `pip` package installer

;;; Code:

(require 'pyvenv)
(require 'project)
(require 'transient)
(require 'compile)

(defcustom pip-menu-project-markers-files '("Pipfile"
                                            "pyproject.toml"
                                            "requirements.txt"
                                            "setup.py"
                                            "setup.cfg"
                                            "environment.yml")
  "List of filenames used to identify Python project directories.

A list of filenames used to identify Python projects.

The default filenames are \"Pipfile\", \"pyproject.toml\",
\"requirements.txt\", \"setup.py\", \"setup.cfg\", and \"environment.yml\".

Each element in the list should be a string representing a filename
that is commonly found in the root directory of a Python project."
  :group 'km-py
  :type '(repeat string))

(defcustom pip-menu-venv-names '(".env" "env" ".venv" "venv")
  "List of virtual environment directory names to search for.

A list of directory names that are considered potential Python virtual
environments.

Each element in the list is a string that represents a directory name to be
checked when searching for a Python virtual environment in the current path. The
search function looks for these directories at the current path and upwards,
stopping at the root directory or when a matching virtual environment is found."
  :group 'km-py
  :type '(repeat
          (string :tag "Venv directory name")))

(declare-function json-read "json")

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(defcustom pip-menu-inhibit-cache nil
  "Inhibition flag for caching pip menu commands.

When non-nil, disables the caching mechanism for pip commands.

By default, pip commands are cached to improve performance.
Setting this variable to t will inhibit this caching, causing
pip commands to be re-evaluated each time the menu is invoked.

This can be useful for development or debugging purposes where
the command set might change frequently."
  :group 'pip-menu
  :type 'boolean)

(defcustom pip-menu-extra-command-props nil
  "Transient properties for additional pip commands.

An alist of additional properties for pip commands in the menu.

Each element is a cons cell where the car is a string representing
the pip command, and the cdr is a plist of properties. The plist
can contain various keys to control the behavior and appearance
of the command in the menu."
  :group 'pip-menu
  :type
  '(alist
    :key-type (string :tag "Command")
    :value-type (plist
                 :key-type (choice
                            (const :if)
                            (const :if-not)
                            (const :if-non-nil)
                            (const :if-nil)
                            (const :if-mode)
                            (const :if-not-mode)
                            (const :if-derived)
                            (const :if-not-derived)
                            (const :inapt-face)
                            (const :inapt-if)
                            (const :inapt-if-not)
                            (const :inapt-if-non-nil)
                            (const :inapt-if-nil)
                            (const :inapt-if-mode)
                            (const :inapt-if-not-mode)
                            (const :inapt-if-derived)
                            (const :inapt-if-not-derived)
                            (symbol))
                 :value-type sexp)))


(defun pip-menu--find-project-root (&optional directory)
  "Locate the root DIRECTORY with `package.json'.

Optional argument DIRECTORY is the directory from which to start searching for
the project root. If not provided, `default-directory' is used."
  (unless directory (setq directory default-directory))
  (if-let ((found (seq-find
                   (lambda (it)
                     (file-exists-p (expand-file-name it directory)))
                   pip-menu-project-markers-files)))
      (file-name-as-directory directory)
    (let ((parent (expand-file-name ".." directory)))
      (unless (or (string= parent directory)
                  (string= directory "")
                  (string= directory "/"))
        (pip-menu--find-project-root parent)))))

(defun pip-menu--project-current-root ()
  "Return the root directory of the current project."
  (when-let ((project (ignore-errors (project-current))))
    (if (fboundp 'project-root)
        (project-root project)
      (with-no-warnings
        (car (project-roots project))))))

(defconst pip-menu--command-regex
  (rx (seq bol
           (zero-or-more " ")
           (group
            (opt (group
                  (group
                   (one-or-more
                    (any "a-z" "-")))
                  (opt (group ","))
                  " "))
            (group
             (one-or-more
              (any "a-z" "-"))))
           (group
            (or (seq " "
                     (one-or-more " "))
                eol))))
  "Regular expression for matching pip commands.")

(defconst pip-menu-argument-group-title-regex
  (rx (seq bol
           (zero-or-more
            (any "\t "))
           (group
            (any "a-z")
            (one-or-more nonl))
           ":"
           (zero-or-more " ")
           eol))
  "Regular expression matching argument group titles.")

(defconst pip-menu--argument-regex
  (rx (seq bol
           (zero-or-more " ")
           (opt (group
                 (group "-"
                        (any "a-z"))
                 ", "))
           (group "--"
                  (opt (group
                        (opt "[")
                        (group
                         (one-or-more
                          (any "a-z" "_|-"))
                         "-")
                        "]"))
                  (one-or-more
                   (any "a-z" "_-")))
           (opt (group
                 (any " =")
                 (group
                  (one-or-more
                   (not (any " "))))))))
  "Regular expression for matching pip command-line arguments.")

(defvar pip-menu-version nil)
(defvar pip-menu-cache (make-hash-table :test 'equal))
(defvar pip-menu-commands nil)
(defvar pip-menu-general-options nil)
(defvar pip-menu-current-description nil)

(eval-and-compile
  (defun pip-menu--expand (init-fn)
    "Expand the given macro and return the expanded form.
Argument INIT-FN is the macro to be expanded."
    (setq init-fn (macroexpand init-fn))
    (if (symbolp init-fn)
        `(#',init-fn)
      `(,init-fn))))

(defmacro pip-menu--parse-help-with-output (output &rest body)
  "Parse pip help OUTPUT in a temporary buffer.

Argument OUTPUT is a string containing the help output to parse.

Remaining arguments BODY are forms that are evaluated with the OUTPUT inserted
into a temporary buffer."
  (declare (indent 1)
           (debug t))
  `(with-temp-buffer
     (save-excursion
       (insert ,output))
     ,@body))

(defmacro pip-menu--compose (&rest functions)
  "Compose FUNCTIONS into a single callable chain.

Remaining arguments FUNCTIONS are Lisp functions to be composed."
  (declare (debug t)
           (pure t)
           (indent defun)
           (side-effect-free t))
  (setq functions (reverse functions))
  (let ((args-var (make-symbol "arguments")))
    `(lambda (&rest ,args-var)
       ,@(let ((init-fn (pop functions)))
          (list
           (seq-reduce
            (lambda (acc fn)
              `(funcall ,@(pip-menu--expand fn) ,acc))
            functions
            `(apply ,@(pip-menu--expand init-fn) ,args-var)))))))

(defun pip-menu--unquote (exp)
  "Remove `function' symbols from list start.

Argument EXP is an expression to be unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun pip-menu--make-command-name (&rest args)
  "Convert arguments into a trimmed, space-separated string.

Remaining arguments ARGS are strings that will be concatenated to form the
command name after being unquoted and trimmed."
  (let ((name (mapconcat (pip-menu--compose
                           string-trim
                           (apply-partially #'format "%s"))
                         (delq nil
                               (mapcar #'pip-menu--unquote
                                       (flatten-list args)))
                         " ")))
    name))

(defun pip-menu--confirm-command (cmd &rest args)
  "Prompt user to confirm pip command before execution.

Argument CMD is the name of the pip command to run.

Remaining arguments ARGS are additional command-line arguments to pass to the
pip command."
  (read-string "Run: "
               (pip-menu--make-command-name
                cmd args)))

(defun pip-menu--get-project-buffer-name ()
  "Generate a buffer name for an pip project."
  (when-let ((name (pip-menu--get-project-root)))
    (format "pip<%s>" (replace-regexp-in-string "^~/\\|/$" "" name))))

(defun pip-menu--get-project-root ()
  "Find and return the root directory of the current Python project."
  (or
   (pip-menu--project-current-root)
   (pip-menu--find-project-root)))

(defun pip-menu--run-compile (command &optional buff-name env)
  "Run a compilation COMMAND using the current Node.js version.

Argument COMMAND is a string representing the shell command to execute.

Optional argument BUFF-NAME is either a string specifying the buffer name or a
function that generates the buffer name.

Optional argument ENV is a list of environment variables to set before running
the COMMAND; it defaults to `process-environment'."
  (let ((compenv (or env process-environment)))
    (let*
        ((compilation-read-command nil)
         (compilation-environment compenv)
         (compile-command command)
         (compilation-buffer-name-function
          (if (functionp buff-name)
              buff-name
            (lambda (&optional _mode)
              (or buff-name
                  (concat (pip-menu--get-project-buffer-name) "-compilation"))))))
      (compile compile-command))))

(defun pip-menu--stringify (item)
  "Convert various data types to string representation.

Argument ITEM is the object to be stringified; it can be a string, number,
vector, list, cons cell, symbol, or any other type."
  (pcase item
    ((pred not)
     item)
    ((pred stringp)
     (substring-no-properties item))
    ((pred numberp)
     (number-to-string item))
    ((pred vectorp)
     (apply #'vector (mapcar #'pip-menu--stringify (append item nil))))
    ((pred proper-list-p)
     (mapcar #'pip-menu--stringify item))
    ((guard (and (consp item)
                 (atom (cdr item))))
     (cons (pip-menu--stringify (car item))
           (pip-menu--stringify (cdr item))))
    ((guard (and item (symbolp item)))
     (substring-no-properties (symbol-name item)))
    (_ item)))

(defun pip-menu--get-arguments ()
  "Retrieve and process arguments for npmjs command."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    (pip-menu--sort-args (pip-menu--stringify raw-args))))

(defun pip-menu--sort-args (args)
  "Sort arguments using a custom transformer.

Argument ARGS is a sequence of elements to be sorted."
  (seq-sort-by #'pip-menu--arg-sort-transformer #'> args))

(defun pip-menu--arg-sort-transformer (argument)
  "Sort command-line arguments by prefix type.

ARGUMENT argument is a string or a list where the first element is a string
representing a command-line argument."
  (let ((arg (if (listp argument)
                 (car argument)
               argument)))
    (cond ((string= "--" arg)
           -4)
          ((string-prefix-p "--" arg)
           -3)
          ((string-prefix-p "-" arg)
           -2)
          ((string-prefix-p "<" arg)
           -1)
          (t 0))))

(defun pip-menu--format-args (args)
  "Format npm arguments by joining lists and substituting hints.

Argument ARGS is a list of arguments to be formatted."
  (let ((new-args (mapcar
                   (lambda (it)
                     (cond ((listp it)
                            (string-join it " "))
                           (t it)))
                   args)))
    (replace-regexp-in-string "=[\s]" "="
                              (pip-menu--substitute-hints
                               (string-join new-args "\s")))))

(defun pip-menu--substitute-hints (str)
  "Replace HTML-like tags in a string.

Argument STR is a string containing the text to be processed."
  (with-temp-buffer
    (insert
     str)
    (goto-char (point-min))
    (while (re-search-forward "[@./=]?\\(<\\([a-z0-9/@-][^>]+\\)[>][\s./=]?\\)+"
                              nil t 1)
      (replace-match ""))
    (buffer-string)))

(defun pip-menu--get-formatted-transient-args ()
  "Format transient arguments for npmjs."
  (pip-menu--format-args (reverse (pip-menu--get-arguments))))

(transient-define-suffix pip-menu-show-args ()
  "Display formatted npm command with arguments."
  :transient t
  (interactive)
  (let ((name (get transient-current-command 'pip-menu-command))
        (args
         (pip-menu--get-formatted-transient-args)))
    (message
     (propertize (pip-menu--make-command-name name args)
                 'face 'success))))

(defmacro pip-menu-define-command (name cmd description &rest body)
  "Define a transient command with NAME, CMD, DESCRIPTION, and BODY.

Argument NAME is the symbol for the command being defined.

Argument CMD is the string representing the pip command to run.

Argument DESCRIPTION is the string describing the command.

Remaining arguments BODY are the forms to be included in the command definition."
  (let ((sym (make-symbol "sym"))
        (help-output (make-symbol "help-output"))
        (args (make-symbol "args"))
        (value (make-symbol "value")))
    `(progn
       (let ((,sym)
             (,help-output)
             (,args)
             (,value))
        (transient-define-prefix ,name (&optional ,args)
          ,(concat (or description cmd) ".")
          :man-page "pip"
          [:description (lambda ()
                          (setq ,help-output
                           (or ,help-output
                            (pip-menu--parse-help-with-output
                                (pip-menu--trim-warning
                                 (shell-command-to-string (format
                                                           "pip help %s"
                                                           ,cmd)))
                              (buffer-string))))
                          (setq ,sym (with-temp-buffer
                                       (save-excursion
                                         (insert ,help-output))
                                       (pip-menu--parse-commands ,cmd)))
                          (plist-get ,sym :usage))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 0 (plist-get ,sym :options)))))]
          [:description (lambda ()
                          (car (nth 1 (plist-get ,sym :options))))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 1 (plist-get ,sym :options)))))]
          [:description (lambda ()
                          (car (nth 2 (plist-get ,sym :options))))
           :if (lambda ()
                 (car (nth 2 (plist-get ,sym :options))))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 2 (plist-get ,sym :options)))))]
          ,@body
          [[("RET" "Run" (lambda ()
                           (interactive)
                           (pip-menu--run-compile
                            (pip-menu--confirm-command
                             "pip"
                             ,cmd
                             (pip-menu--get-formatted-transient-args))
                            (pip-menu--get-project-buffer-name))))
            ("C-c C-a" "Show arguments" pip-menu-show-args)]]
          (interactive (list (pip-menu--get-arguments)))
          (message "`%s'=" ,args)
          (setq ,help-output
           (or ,help-output (pip-menu--parse-help-with-output
                                (pip-menu--trim-warning
                                 (shell-command-to-string
                                  (concat "pip help " ,cmd)))
                              (buffer-string))))
          (setq ,sym (with-temp-buffer
                       (save-excursion
                         (insert ,help-output))
                       (pip-menu--parse-commands ,cmd)))
          (put ',name 'transient--prefix
           (transient-prefix
            :command ',name
            :value ,args))
          (transient-setup ',name)))
       (put ',name 'pip-menu-command
        ,(concat "pip " cmd)))))

(defun pip-menu--trim-warning (str)
  "Remove \"WARNING:\" lines from the beginning of the string STR.

Argument STR is the input string to be processed."
  (if (string-match-p "^WARNING:" str)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (looking-at "^WARNING:")
          (forward-line 1))
        (string-trim (buffer-substring-no-properties (point)
                                                     (point-max))))
    (string-trim str)))

(defun pip-menu--json-read-buffer (&optional object-type array-type null-object
                                             false-object)
  "Parse json from the current buffer using specified object and array types.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))

(defvar pip-menu---packages nil)

(defun pip-menu---packages (&optional recache)
  "Return alist of installed pip packages, optionally recaching the result.

Optional argument RECACHE, when non-nil, forces the cache to be refreshed."
  (interactive (list current-prefix-arg))
  (and recache (setq pip-menu---packages nil))
  (or pip-menu---packages
      (setq pip-menu---packages
            (with-temp-buffer
              (call-process "pip" nil (current-buffer) nil
                            "list" "--format=json")
              (goto-char (point-min))
              (when (re-search-forward "\\[" nil t 1)
                (forward-char -1))
              (let
                  ((it
                    (pip-menu--json-read-buffer)))
                (if it
                    (progn
                      (seq-map
                       #'(lambda
                           (e)
                           (cons
                            (assoc-default 'name e)
                            e))
                       it))))))))




(defun pip-menu--arg-description-at-point ()
  "Concatenate and return argument descriptions from the current point onward."
  (skip-chars-forward "\s\t\n")
  (let* ((descr-parts
          (list (buffer-substring-no-properties (point)
                                                (line-end-position))))
         (col (current-column))
         (re (concat (make-string col ?\s) "\\([^\s][^\n]+\\)")))
    (forward-line 1)
    (while (looking-at re)
      (push (match-string-no-properties 1) descr-parts)
      (forward-line 1))
    (string-join (nreverse descr-parts) "\s")))

(defun pip-menu--extract-choices-from-description (description)
  "Extract and return choices from DESCRIPTION string separated by |.

Argument DESCRIPTION is a string containing the choices separated by |."
  (when (string-match-p "\\([a-zA-Z0-9-]+|\\)+" description)
    (with-temp-buffer (save-excursion
                        (insert description))
                      (re-search-forward "\\([a-z0-9_-]+|\\)+" nil t 1)
                      (let ((beg (match-beginning 0)))
                        (split-string
                         (buffer-substring-no-properties beg
                                                         (progn
                                                           (skip-chars-forward
                                                            "a-z0-9_-")
                                                           (point)))
                         "|" t)))))

(defun pip-menu--show-help (arg description &optional short specifier)
  "Display a help buffer with ARG, DESCRIPTION, and optional SHORT and SPECIFIER.

Argument ARG is the main string to be displayed in the help buffer.

Argument DESCRIPTION is the detailed text to be shown in the help buffer.

Optional argument SHORT is an additional short DESCRIPTION to be displayed.

Optional argument SPECIFIER is an extra string to be shown in the help buffer."
  (let* ((buffer (get-buffer-create
                  "*pip-menu-help*"))
         (orign-wnd (selected-window)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-at-bottom
                '((window-height . fit-window-to-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq truncate-lines nil)
                (when-let* ((quit-key (where-is-internal
                                       'quit-window
                                       special-mode-map
                                       t t t))
                            (map
                             (make-sparse-keymap))
                            (buff (current-buffer)))
                  (define-key map quit-key
                              (lambda ()
                                (interactive)
                                (when (and orign-wnd
                                           (window-live-p orign-wnd))
                                  (select-window orign-wnd)
                                  (transient-resume)
                                  (when (buffer-live-p buff)
                                    (kill-buffer buff)))))
                  (use-local-map
                   (make-composed-keymap
                    map
                    (current-local-map))))
                (save-excursion
                  (insert (propertize (substring-no-properties arg) 'face
                                      'font-lock-keyword-face))
                  (when short
                    (insert ", "
                            (propertize (substring-no-properties short) 'face
                                        'font-lock-keyword-face)))
                  (when specifier
                    (insert " " (propertize
                                 (or (substring-no-properties specifier) "")
                                 'face
                                 'font-lock-type-face)))
                  (insert "\n")
                  (when description
                    (let ((pos (point)))
                      (insert description)
                      (fill-region-as-paragraph pos (point)))))))
            (select-window window))))))

(defun pip-menu-read-dir-relative (&optional prompt &rest _)
  "Read a directory name with optional PROMPT and return its relative path.

Optional argument PROMPT is a string used to prompt the user for a directory.

Remaining arguments _ are ignored."
  (file-relative-name
   (file-local-name
    (expand-file-name
     (read-directory-name
      (or prompt "Directory: "))))
   default-directory))

(defun pip-menu--parse-options ()
  "Parse command-line options and return used keys and commands."
  (let ((commands)
        (used-keys))
    (while (looking-at pip-menu--argument-regex)
      (let ((short (match-string-no-properties 2))
            (argument  (match-string-no-properties 3))
            (no-option (match-string-no-properties 4))
            (bracket-value (match-string-no-properties 5))
            (specifier (match-string-no-properties 7))
            (descr)
            (short-descr)
            (arg-list))
        (goto-char (match-end 0))
        (skip-chars-forward "\s")
        (setq descr (pip-menu--arg-description-at-point))
        (unless (member argument '("--help"))
          (when short
            (push short used-keys)
            (setq arg-list (list short argument)))
          (setq short-descr (replace-regexp-in-string "^--" "" argument))
          (when specifier
            (setq specifier (string-trim specifier)))
          (cond ((and argument
                      no-option
                      bracket-value)
                 (let* ((arg-value (substring-no-properties
                                    argument
                                    (length
                                     (concat
                                      "--"
                                      no-option))))
                        (choices (mapcan
                                  (lambda (it)
                                    (list (concat "--" it arg-value)
                                          (concat "--" arg-value)))
                                  (split-string bracket-value "|" t)))
                        (pl `(:choices ',choices
                              :argument-format "%s"
                              :argument-regexp ,(regexp-opt
                                                 choices)
                              :show-help (lambda (&rest _)
                                           (interactive)
                                           (pip-menu--show-help
                                            ,(string-trim
                                              argument)
                                            ,descr
                                            ,short
                                            ,specifier)))))
                   (push
                    (append (list (car choices))
                            pl)
                    commands)))
                ((and specifier
                      (string-match-p "\\([a-zA-Z0-9-]+|\\)+"
                                      descr))
                 (let* ((choices (pip-menu--extract-choices-from-description
                                  descr))
                        (pl `(:choices ',choices
                              :argument-format ,(concat argument " %s")
                              :argument-regexp ,(regexp-opt
                                                 choices)
                              :show-help (lambda (&rest _)
                                           (interactive)
                                           (pip-menu--show-help
                                            ,(string-trim
                                              argument)
                                            ,descr
                                            ,short
                                            ,specifier)))))
                   (push
                    (append (list (car choices))
                            pl)
                    commands)))
                (specifier
                 (setq argument (concat argument " "))
                 (let ((pl
                        (pcase specifier
                          ((pred
                            (string-match-p
                             "<\\(path\\|dir\\|directory\\)>"))
                           (list
                            :prompt "Directory: "
                            :class 'transient-option
                            :reader (pip-menu--compose
                                      (apply-partially
                                       #'format
                                       (replace-regexp-in-string
                                        "<[^>]+>"
                                        "%s"
                                        specifier))
                                      pip-menu-read-dir-relative)))
                          (_ (list
                              :prompt specifier
                              :class 'transient-option
                              :reader (pip-menu--compose
                                        (apply-partially
                                         #'format
                                         (replace-regexp-in-string
                                          "<[^>]+>"
                                          "%s"
                                          specifier))
                                        #'read-string))))))
                   (setq pl (plist-put pl
                                       :show-help
                                       (lambda (&rest _)
                                         (interactive)
                                         (pip-menu--show-help
                                          (string-trim argument) descr
                                          short
                                          specifier))))
                   (push (append (list short-descr argument)
                                 pl)
                         commands)))
                (t
                 (push (list short-descr (or arg-list argument)
                             :show-help (lambda (&rest _)
                                          (interactive)
                                          (pip-menu--show-help
                                           (string-trim
                                            argument)
                                           descr
                                           short
                                           specifier)))
                       commands))))))
    (cons used-keys (nreverse commands))))

(defun pip-menu--eval-infix (name &optional args inhibit-eval)
  "Evaluate or define transient infix for npmjs commands.

Argument NAME is a symbol representing the name of the infix command.

Optional argument ARGS is a list of arguments for the infix command.

Optional argument INHIBIT-EVAL is a boolean; when non-nil, the infix command is
not evaluated."
  (let ((descr (car (seq-filter
                     (lambda (it)
                       (and (stringp it)
                            (not	(string-empty-p it))))
                     (list
                      (when (stringp (car args))
                        (car (last (split-string (car args) "-" t))))
                      (replace-regexp-in-string "pip-menu-" ""
                                                (format "%s" name))))))
        (pl (if (stringp (car args))
                (cdr args)
              args)))
    (when (memq :argument-format pl)
      (plist-put pl :class ''transient-switches))
    (if inhibit-eval
        (list descr name `(transient-define-infix
                            ,name
                            ()
                            ,@pl))
      (eval `(transient-define-infix
               ,name
               ()
               ,@pl)
            t)
      (list descr name))))

(defun pip-menu--parse-commands (&optional parent-cmd used-keys)
  "Parse command groups and options from the current buffer.

Optional argument PARENT-CMD is a string representing the parent command.

Optional argument USED-KEYS is a list of keys that have already been used."
  (let ((groups)
        (options-groups)
        (usage)
        (used-keys (append used-keys '("q" "v")))
        (options-used-keys '("q")))
    (let ((case-fold-search t))
      (while (re-search-forward pip-menu-argument-group-title-regex nil t 1)
        (let ((group-title (match-string-no-properties 1)))
          (forward-line 1)
          (skip-chars-forward "\n")
          (cond ((looking-at pip-menu--argument-regex)
                 (pcase-let ((`(,keys . ,options)
                              (pip-menu--parse-options)))
                   (when options
                     (when keys
                       (setq options-used-keys (nconc options-used-keys keys)))
                     (let ((items
                            (pip-menu--generate-shortcuts
                             options
                             (pcase-lambda
                               (`(,alias ,cmd
                                  . _))
                               (or alias cmd))
                             (lambda (key def)
                               (push key options-used-keys)
                               (cond ((memq :argument-format def)
                                      (append (list key)
                                              (pip-menu--eval-infix
                                               (make-symbol
                                                (concat "pip-menu-"
                                                        parent-cmd
                                                        (car def)))
                                               def)))
                                     ((listp (cadr def))
                                      (append (list
                                               (or (car (cadr def)) key)
                                               (car def)
                                               (cadr def))
                                              (seq-drop def 2)))
                                     (t (append (list key) def))))
                             options-used-keys)))
                       (push (cons group-title items) options-groups)))))
                ((looking-at pip-menu--command-regex)
                 (let ((commands))
                   (while (looking-at pip-menu--command-regex)
                     (let ((short (match-string-no-properties 3))
                           (long  (match-string-no-properties 5))
                           (descr))
                       (goto-char (match-end 0))
                       (skip-chars-forward "\s")
                       (setq descr (pip-menu--arg-description-at-point))
                       (push (list short long descr)
                             commands)))
                   (setq commands (nreverse commands))
                   (let ((non-alised-cmds (seq-filter
                                           (pip-menu--compose not car)
                                           commands)))
                     (pip-menu--generate-shortcuts
                      non-alised-cmds
                      (pcase-lambda
                        (`(,_alias ,cmd
                           . _))
                        cmd)
                      (lambda (key def)
                        (push key used-keys)
                        (setcar def key)
                        def)
                      used-keys))
                   (push (cons group-title commands) groups)))
                (t
                 (let ((end (or (save-excursion
                                  (when (re-search-forward
                                         pip-menu-argument-group-title-regex nil
                                         t
                                         1)
                                    (match-beginning 0)))
                                (point-max))))
                   (setq usage (concat usage
                                       (when usage "\n")
                                       (concat group-title
                                               "\n"
                                               (buffer-substring-no-properties (point) end)))))))))
      (setq groups (nreverse groups))
      (setq options-groups (nreverse options-groups))
      (list
       :usage usage
       :commands groups
       :options options-groups))))

(defun pip-menu--key-splitted-variants (word len separator)
  "Generate unique key variants from a word.

Argument WORD is a string to be split into variants.

Argument LEN is an integer representing the length of each variant to be
generated.

Argument SEPARATOR is a string used as the delimiter for splitting WORD."
  (when-let* ((slen
               (when (> len 1)
                 (1- len)))
              (splitted (mapcar (apply-partially
                                 #'pip-menu--key-builder-safe-substring 1)
                                (seq-drop (split-string word separator t)
                                          1)))
              (first-letter (pip-menu--key-builder-safe-substring 1 word)))
    (seq-uniq
     (append (reverse (mapcar (lambda (it)
                                (unless (> slen (length it))
                                  (concat first-letter
                                          (string-join it ""))))
                              (seq-split splitted slen)))
             (list
              (mapconcat (lambda (_) first-letter)
                         (number-sequence 0 slen)
                         ""))))))

(defun pip-menu--key-builder-shared-start (s1 s2)
  "Find common string prefix between S1 and S2.

Argument S1 is a string to compare.

Argument S2 is another string to compare against S1."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun pip-menu--key-builder-capitalize-variants (word)
  "Generate capitalized variants of a string.

Argument WORD is a string to be split and capitalized in various ways."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join
                                              (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join
                                            (seq-drop parts (1+ i))
                                            "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun pip-menu--key-builder-safe-substring (len word)
  "Truncate WORD to LEN characters, preserving properties.

Argument LEN is an integer representing the maximum length of the substring.

Argument WORD is a string from which the substring will be extracted."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun pip-menu--key-builder-get-all-key-strategies (word len)
  "Generate unique key strategies from a word.

Argument WORD is a string to be processed for key strategies.

Argument LEN is an integer specifying the desired length of the key strategies."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short
                                           (number-to-string (random 10)))))
                     (pip-menu--key-builder-safe-substring len short)))
         (vars
          (mapcar finalize (pip-menu--key-builder-capitalize-variants
                            (pip-menu--key-builder-safe-substring
                             len
                             word)))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (pip-menu--key-builder-shared-start (downcase word)
                                                            (downcase it))))))
     #'>
     (seq-uniq (delq nil
                     (append
                      vars
                      (pip-menu--key-splitted-variants word len "^[a-z]")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'pip-menu--key-builder-safe-substring
                                             n)
                                            parts "")))
                       (number-sequence 1 (min len parts-len)))
                      (pip-menu--key-splitted-variants word len "")
                      (mapcar
                       (lambda (n)
                         (funcall finalize (mapconcat
                                            (apply-partially
                                             #'pip-menu--key-builder-safe-substring
                                             n)
                                            (reverse parts) "")))
                       (number-sequence 1 (min len parts-len)))))))))

(defun pip-menu--generate-shortcut-key (word key-len shortcuts all-keys)
  "Generate a unique shortcut key.

Argument WORD is a string from which the shortcut key is generated.

Argument KEY-LEN is an integer that specifies the maximum length of the
generated shortcut key.

Argument SHORTCUTS is a list of strings representing existing shortcuts to avoid
conflicts.

Argument ALL-KEYS is a list of strings representing all possible keys to ensure
uniqueness."
  (let ((short
         (downcase
          (substring-no-properties word 0
                                   (min key-len
                                        (length word))))))
    (setq short (if (string-match-p short "[a-z]")
                    (replace-regexp-in-string "^[^a-z]+" "" short)
                  short))
    (setq short
          (seq-find
           (lambda (it)
             (not
              (seq-find
               (apply-partially
                #'string-prefix-p it)
               shortcuts)))
           (append
            (pip-menu--key-builder-get-all-key-strategies
             word
             key-len)
            (let ((random-variants
                   (pip-menu--get-alphabet)))
              (or (seq-remove (lambda (key)
                                (seq-find (apply-partially
                                           #'string-prefix-p
                                           (downcase key))
                                          all-keys))
                              random-variants)
                  random-variants)))))
    (while (and
            (< (length short) key-len))
      (setq short (concat (or short "")
                          (number-to-string (random 10)))))
    short))

(defmacro pip-menu--cond (&rest pairs)
  "Transform conditions into a lambda function.

Remaining arguments PAIRS are lists where each list contains a condition and a
result form."
  (declare (pure t)
           (indent defun)
           (side-effect-free error-free))
  (setq pairs (mapcar (lambda (it)
                        (if (listp it)
                            (apply #'vector it)
                          it))
                      pairs))
  (let ((args (make-symbol "arguments")))
    `(lambda (&rest ,args)
       (cond ,@(mapcar (lambda (v)
                         (list (if (eq (aref v 0) t) t
                                `(apply ,@(pip-menu--expand (aref v 0)) ,args))
                          `(apply ,@(pip-menu--expand (aref v 1)) ,args)))
                pairs)))))

(defun pip-menu--get-alphabet ()
  "Generate list of lowercase, uppercase letters and symbols @ .."
  (append
   (mapcar #'char-to-string
           (number-sequence (string-to-char
                             "a")
                            (string-to-char
                             "z")))
   (mapcar #'char-to-string
           (number-sequence (string-to-char
                             "A")
                            (string-to-char
                             "Z")))
   (list "@" ".")))

(defun pip-menu--key-builder-default-value-fn (key value)
  "Generate list with key and value(s).

Argument KEY is the key associated with the VALUE in the key-value pair.

Argument VALUE is the value associated with the KEY; it can be a proper list or
any other object."
  (if (proper-list-p value)
      (append (list key) value)
    (cons key value)))

(defun pip-menu--key-builder-default-key-fn (def)
  "Generate string from symbol or return argument.

Argument DEF is a symbol or a string that represents the key to be built."
  (if (symbolp def)
      (symbol-name def)
    def))

(defun pip-menu--generate-shortcuts (items &optional key-fn value-fn used-keys
                                           key-len)
  "Generate keyboard shortcuts for items.

Argument ITEMS is a list of items to generate shortcuts for.

Optional argument KEY-FN is a function used to extract a key from an item. It
defaults to `pip-menu--key-builder-default-key-fn'.

Optional argument VALUE-FN is a function used to create a value from a shortcut
and an item. It defaults to `pip-menu--key-builder-default-value-fn'.

Optional argument USED-KEYS is a list of already used keys to avoid conflicts.

Optional argument KEY-LEN is an integer specifying the minimum length of the
generated keys."
  (when items
    (unless key-fn (setq key-fn #'pip-menu--key-builder-default-key-fn))
    (unless value-fn (setq value-fn #'pip-menu--key-builder-default-value-fn))
    (let ((min-len
           (or key-len
               (let ((variants-len (length (pip-menu--get-alphabet)))
                     (total (length items)))
                 (max 1 (length
                         (car
                          (seq-sort-by #'length
                                       #'>
                                       used-keys)))
                      (ceiling (log total variants-len)))))))
      (let ((shortcuts used-keys)
            (used-words '())
            (all-keys (mapcar
                       (pip-menu--compose
                         (pip-menu--cond
                           [(pip-menu--compose not
                              (apply-partially #'string-match-p
                                               "[a-z]"))
                            identity]
                           [t (apply-partially #'replace-regexp-in-string
                                               "^[^a-z]+" "")])
                         (lambda (it)
                           (funcall key-fn it)))
                       items))
            (result))
        (dotimes (i (length items))
          (let ((word (nth i all-keys))
                (def (nth i items)))
            (when-let* ((shortcut
                         (when (not (member word used-words))
                           (pip-menu--generate-shortcut-key
                            word
                            min-len
                            shortcuts
                            all-keys)))
                        (value (funcall value-fn shortcut def)))
              (setq used-words (push word used-words))
              (setq shortcuts (push shortcut shortcuts))
              (setq result (push value result)))))
        (reverse result)))))

(defun pip-menu--version ()
  "Return the version of pip by calling the \"pip --version\" command."
  (with-temp-buffer
    (let ((status (call-process "pip"  nil t nil "--version")))
      (when (zerop status)
        (mapconcat
         (lambda (it)
           (if (and (file-name-absolute-p it)
                    (file-exists-p it))
               (abbreviate-file-name it)
             it))
         (split-string (string-trim (buffer-string)) "\s")
         " ")))))

(defun pip-menu--find-venv-path ()
  "Find the nearest virtual environment directory from the current path."
  (let ((found)
        (directory default-directory))
    (while (and
            (not found)
            (not (string= "/" directory)))
      (setq found (when-let ((name (seq-find
                                    (lambda (venv-name)
                                      (let ((venv-path
                                             (expand-file-name
                                              venv-name
                                              directory))
                                            (cands '("bin/activate"
                                                     "Scripts/activate"
                                                     "bin/activate.csh"
                                                     "bin/activate.fish")))
                                        (and (file-directory-p venv-path)
                                             (seq-find
                                              (lambda (file)
                                                (file-exists-p
                                                 (expand-file-name
                                                  file
                                                  venv-path)))
                                              cands))))
                                    pip-menu-venv-names)))
                    (file-name-as-directory (expand-file-name name directory))))
      (setq directory (expand-file-name "../" directory)))
    found))

(transient-define-suffix pip-menu-pyvenv-deactivate ()
  "Deactivate the current Python virtual environment."
  :inapt-if-not (lambda ()
                  (bound-and-true-p pyvenv-virtual-env))
  :description (lambda ()
                 (concat "Deactivate venv"
                         (when (bound-and-true-p pyvenv-virtual-env)
                           (concat " ("
                                   (propertize (substring-no-properties
                                                pyvenv-virtual-env)
                                               'face
                                               'transient-value)
                                   ")"))))
  (interactive)
  (pyvenv-deactivate)
  (when transient-current-command
    (transient-setup transient-current-command)))

(defun pip-menu-toggle-pyvenv-mode ()
  "Toggle `pyvenv-mode' and update the transient description accordingly."
  (interactive)
  (pyvenv-mode (if pyvenv-mode -1 1))
  (when transient-current-command
    (transient-setup transient-current-command)))

(defun pip-menu-toggle-tracking-mode ()
  "Toggle Pyvenv Tracking Mode and update the transient description."
  (interactive)
  (pyvenv-tracking-mode (if pyvenv-tracking-mode -1 1))
  (when transient-current-command
    (transient-setup transient-current-command)))

;;;###autoload (autoload 'pip-menu-pyvenv "pip-menu" nil t)
(transient-define-prefix pip-menu-pyvenv ()
  "Define a transient prefix for managing Python virtual environments."
  ["Virtual Envs"
   ("d" pip-menu-pyvenv-deactivate)
   ("a" "Activate a virtual env by directory" pyvenv-activate)
   ("w" "Activate a virtualenvwrapper env" pyvenv-workon)
   ("c" "Create a virtual env" pyvenv-create)
   ("r" "Restart Python Processes" pyvenv-restart-python)]
  ["Toggle mode"
   ("t" pip-menu-toggle-tracking-mode
    :description (lambda ()
                   (concat
                    (truncate-string-to-width "Pyvenv Tracking Mode " 24 0 ?\s)
                    "["
                    (if (bound-and-true-p pyvenv-tracking-mode)
                        "X"
                      " ")
                    "]")))
   ("m" pip-menu-toggle-pyvenv-mode
    :description (lambda ()
                   (concat
                    (truncate-string-to-width "Pyvenv Mode " 24 0 ?\s)
                    "["
                    (if (bound-and-true-p pyvenv-mode)
                        "X"
                      " ")
                    "]")))])

(defun pip-menu--setup ()
  "Initialize and cache PIP commands and their descriptions."
  (setq pip-menu-version (pip-menu--version))
  (setq pip-menu-commands (unless pip-menu-inhibit-cache
                            (gethash pip-menu-version
                                     pip-menu-cache)))
  (unless pip-menu-commands
    (setq pip-menu-commands
          (with-temp-buffer
            (let ((status (call-process "pip"  nil t nil "help")))
              (when (zerop status)
                (goto-char (point-min))
                (let* ((pl (pip-menu--parse-commands))
                       (groups (plist-get pl :commands))
                       (options (plist-get pl :options))
                       (usage (plist-get pl :usage)))
                  (setq pip-menu-current-description usage)
                  (setq pip-menu-general-options options)
                  (let ((results))
                    (pcase-dolist (`(,group-name . ,cmds) groups)
                      (let ((mapped-cmds
                             (mapcar
                              (pcase-lambda (`(,k ,cmd ,descr))
                                (let
                                    ((opt
                                      (list
                                       (string-join
                                        (delq nil
                                              (list cmd
                                                    descr))
                                        ": ")
                                       k))
                                     (extra-props
                                      (cdr
                                       (assoc-string
                                        cmd
                                        pip-menu-extra-command-props))))
                                  (let ((name
                                         (intern
                                          (concat "pip-menu-"
                                                  (replace-regexp-in-string
                                                   " " "-" cmd)
                                                  "-"
                                                  (when pip-menu-version
                                                    (with-temp-buffer
                                                      (save-excursion (insert pip-menu-version))
                                                      (when (re-search-forward "[0-9]+[.][0-9]+[.][0-9]+" nil t 1)
                                                        (match-string-no-properties 0))))))))
                                    (eval `(pip-menu-define-command
                                            ,name
                                            ,cmd
                                            ,descr)
                                          t)
                                    (push name opt)
                                    name)
                                  (if extra-props
                                      (append (reverse opt) extra-props)
                                    (reverse opt))))
                              cmds)))
                        (when mapped-cmds
                          (push (cons group-name mapped-cmds) results))))
                    (pcase-dolist (`(,k . ,items) pip-menu-general-options)
                      (dolist (it items)
                        (unless (string-prefix-p "-" (car it))
                          (setcar it (concat "-" (car it)))))
                      (push (cons k items) results))
                    (push (cons "Virtual Env" (list '("v" "Manage virtual venv" pip-menu-pyvenv))) results)
                    (nreverse results))))))))
  (puthash pip-menu-version pip-menu-commands pip-menu-cache))

;;;###autoload (autoload 'pip-menu "pip-menu" nil t)
(transient-define-prefix pip-menu ()
  "Manage dependencies with pip."
  :man-page "pip"
  [[:description (lambda ()
                   (if pip-menu-version
                       (string-trim
                        pip-menu-version)
                     ""))
    :class transient-column
    :if (lambda ()
          (cdr (nth 0 pip-menu-commands)))
    :setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        (oref transient--prefix command))
       (cdr (nth 0 pip-menu-commands))))]
   [:description (lambda ()
                   (car (nth 1 pip-menu-commands)))
    :class transient-column
    :if (lambda ()
          (cdr (nth 1 pip-menu-commands)))
    :setup-children
    (lambda (&rest _argsn)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        (oref transient--prefix command))
       (cdr (nth 1 pip-menu-commands))))]]
  [:description (lambda ()
                  (car (nth 2 pip-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 2 pip-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 2 pip-menu-commands))))]
  [:description (lambda ()
                  (car (nth 3 pip-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 3 pip-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 3 pip-menu-commands))))]
  [:description (lambda ()
                  (car (nth 4 pip-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 4 pip-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 4 pip-menu-commands))))]
  [:description (lambda ()
                  (car (nth 5 pip-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 5 pip-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 5 pip-menu-commands))))]
  (interactive)
  (pip-menu--setup)
  (transient-setup #'pip-menu))

(provide 'pip-menu)
;;; pip-menu.el ends here
