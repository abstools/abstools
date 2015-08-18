;;; abs-mode.el --- ABS major mode for Emacs

;; Copyright (C) 2010  Rudolf Schlatte

;; Author: Rudi Schlatte <rudi@constantly.at>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file contains a mode for the modeling language Abs.
;;; 
;;; To start Maude, you also need maude-mode, available from
;;; http://sourceforge.net/projects/maude-mode/ -- or now included in
;;; this directory as well.

(require 'compile)
(require 'custom)
(require 'easymenu)
(eval-when-compile (require 'rx))
(require 'flymake)
(require 'cl-lib)
(autoload 'run-maude "maude-mode" nil t)
(autoload 'inferior-erlang "erlang" nil t)

;;; Code:

;;; Customization
(defgroup abs nil
  "Major mode for editing files in the programming / modeling language Abs."
  :group 'languages)

(defcustom abs-target-language 'maude
  "The default target language for code generation."
  :type '(radio (const maude)
                (const java)
                (const erlang)
                (const prolog)
                (const keyabs))
  :group 'abs)
(put 'abs-target-language 'safe-local-variable
     #'(lambda (x) (member x '(maude java erlang prolog keyabs))))

(defcustom abs-compiler-program (or (executable-find "absc") "absc")
  "Path to the Abs compiler."
  :type 'file
  :group 'abs)
(put 'abs-compiler-program 'risky-local-variable t)

(defcustom abs-java-classpath "absfrontend.jar"
  "The classpath for the Java backend.
The contents of this variable will be passed to the java
executable via the `-cp' argument."
  :type 'string
  :group 'abs)
(put 'abs-java-classpath 'risky-local-variable t)

(defcustom abs-indent standard-indent
  "The width of one indentation step for Abs code."
  :type 'integer
  :group 'abs)
(put 'abs-indent 'safe-local-variable 'integerp)

(defcustom abs-use-timed-interpreter nil
  "Control whether to compile Abs code using the timed interpreter by default.
This influences the default compilation command executed by
\\[abs-next-action].  Note that you can set this variable as a
file-local variable as well."
  :type 'boolean
  :group 'abs)
(put 'abs-use-timed-interpreter 'safe-local-variable 'booleanp)

(defcustom abs-mode-hook (list 'imenu-add-menubar-index 'flymake-mode-on)
  "Hook for customizing `abs-mode'."
  :type 'hook
  :options (list 'imenu-add-menubar-index 'flymake-mode-on)
  :group 'abs)

(defcustom abs-clock-limit 100
  "Default limit value for the clock in the timed Abs interpreter.
Note that you can set this variable as a file-local variable as well."
  :type 'integer
  :group 'abs)
(put 'abs-clock-limit 'safe-local-variable 'integerp)

(defcustom abs-default-resourcecost 0
  "Default resource cost of executing one ABS statement in the timed interpreter."
  :type 'integer
  :group 'abs)
(put 'abs-default-resourcecost 'safe-local-variable 'integerp)

(defvar abs-product-name nil
  "Product to be generated when compiling.")
(put 'abs-product-name 'safe-local-variable 'stringp)

(defcustom abs-debug-output nil
  "Control whether to tell the backend to be verbose.
This setting might not be supported on all backends, or produce
different results."
  :type 'boolean
  :group 'abs)
(put 'abs-debug-output 'safe-local-variable 'booleanp)

;;; Making faces
(defface abs-keyword-face '((default (:inherit font-lock-keyword-face)))
  "Face for Abs keywords"
  :group 'abs)
(defvar abs-keyword-face 'abs-keyword-face
  "Face for Abs keywords.")

(defface abs-constant-face '((default (:inherit font-lock-constant-face)))
  "Face for Abs constants"
  :group 'abs)
(defvar abs-constant-face 'abs-constant-face
  "Face for Abs constants.")

(defface abs-function-name-face
    '((default (:inherit font-lock-function-name-face)))
  "Face for Abs function-names"
  :group 'abs)
(defvar abs-function-name-face 'abs-function-name-face
  "Face for Abs function-names.")

(defface abs-type-face '((default (:inherit font-lock-type-face)))
  "Face for Abs types"
  :group 'abs)
(defvar abs-type-face 'abs-type-face
  "Face for Abs types.")

(defface abs-variable-name-face
    '((default (:inherit font-lock-variable-name-face)))
  "Face for Abs variables"
  :group 'abs)
(defvar abs-variable-name-face 'abs-variable-name-face
  "Face for Abs variables.")

(defconst abs--cid-regexp "\\_<[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\_>")
(defconst abs--id-regexp
  "\\_<\\(?:[[:lower:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\|'\\)*\\_>")

;;; Font-lock for Abs.
;;;
(defconst abs-keywords
  (eval-when-compile
    (regexp-opt
     '("module" "import" "export" "from"              ; the top levels
       "data" "type" "def" "interface" "class" "exception"
       "case" "=>" "new" "local"                      ; the functionals
       "extends"                                      ; the interfaces
       "implements"                                   ; the class
       "delta" "adds" "modifies" "removes" "uses"
       "hasField" "hasMethod" "hasInterface"          ; Deltas
       "productline" "features" "core" "after" "when" ; productlines
       "root" "extension" "group" "opt"
       "oneof" "allof" "ifin" "ifout" "exclude" "require"
       "critical" "port" "rebind" "location"          ; component model
       "move" "father"
       "product"                                      ; product definition
       "let" "in"
       "if" "then" "else" "return" "while"            ; the statements
       "await" "assert" "throw" "die" "get" "skip" "suspend"
       "original" "movecogto"
       "try" "catch" "finally"
       "duration"                       ; guard / statement
       ) 'symbols))
  "List of Abs keywords.")
(defconst abs-constants
  (eval-when-compile
    (regexp-opt
     '("True" "False" "null" "this" "Nil" "Cons")
     'words))
  "List of Abs special words.")

(defvar abs-font-lock-keywords
    (list
     ;; order is important here; earlier entries override later ones
     (cons abs-keywords 'abs-keyword-face)
     (cons abs-constants 'abs-constant-face)
     (cons (concat "\\(" abs--cid-regexp "\\)") 'abs-type-face)
     (list (concat "\\(" abs--id-regexp "\\)[[:space:]]*(") 1
           'abs-function-name-face)
     (cons (concat "\\(" abs--id-regexp "\\)") 'abs-variable-name-face)
     (list "\\<\\(# \w+\\)\\>" 1 'font-lock-warning-face t))
    "Abs keywords.")

;;; Abs syntax table
(defvar abs-mode-syntax-table (copy-syntax-table)
  "Syntax table for abs-mode.")
(modify-syntax-entry ?+  "."     abs-mode-syntax-table)
(modify-syntax-entry ?-  "."     abs-mode-syntax-table)
(modify-syntax-entry ?=  "."     abs-mode-syntax-table)
(modify-syntax-entry ?%  "."     abs-mode-syntax-table)
(modify-syntax-entry ?<  "."     abs-mode-syntax-table)
(modify-syntax-entry ?>  "."     abs-mode-syntax-table)
(modify-syntax-entry ?&  "."     abs-mode-syntax-table)
(modify-syntax-entry ?|  "."     abs-mode-syntax-table)
(modify-syntax-entry ?/ ". 124" abs-mode-syntax-table)
(modify-syntax-entry ?* ". 23b"   abs-mode-syntax-table)
(modify-syntax-entry ?\n ">"   abs-mode-syntax-table)
(modify-syntax-entry ?\^m ">"  abs-mode-syntax-table)

(defvar abs-imenu-syntax-alist
  ;; Treat dot as symbol constituent to handle qualified identifiers
  '(("." . "_")))

(defvar abs-imenu-generic-expression
    `(("Deltas"
       ,(rx bol (* whitespace) "delta" (1+ whitespace)
            (group (char upper) (* (char alnum))))
       1)
      ("Functions"
       ,(rx bol (* whitespace) "def" (1+ whitespace)
            (char upper) (* (or (char alnum) "<" ">"))
            (1+ whitespace)
            ;; not quite correct since the last part of a qualified name
            ;; should start with lowercase.
            (group (* (char alnum))))
       1)
      ("Datatypes"
       ,(rx bol (* whitespace) (or "data" "type") (1+ whitespace)
            (group (char upper) (* (char alnum))))
       1)
      ("Exceptions"
       ,(rx bol (* whitespace) "exception" (1+ whitespace)
            (group (char upper) (* (char alnum))))
       1)
      ("Classes"
       ,(rx bol (* whitespace) "class" (1+ whitespace)
            (group (char upper) (* (char alnum))))
       1)
      ("Interfaces"
       ,(rx bol (* whitespace) "interface" (1+ whitespace)
            (group (char upper) (* (char alnum))))
       1)
      ("Modules"
       ,(rx bol (* whitespace) "module" (1+ whitespace)
            (group (char upper) (* (or (char alnum) "."))))
       1))
  "Imenu expression for `abs-mode'.  See `imenu-generic-expression'.")

;;; Minimal auto-insert mode support
(define-auto-insert 'abs-mode '("Module name: " "module " str ";" ?\n ?\n))

;;; Compiling the current buffer.
;;;
(defvar abs-maude-output-file nil
  "The Maude file that will be generated or loaded by \\[abs-next-action].
Defaults to the buffer filename with a \".maude\" extension if
`abs-input-files' is unset, or to the name of the first element
in `abs-input-files' with a \".maude\" extension otherwise.

Add a file-local setting to override the default value.")
(put 'abs-maude-output-file 'safe-local-variable 'stringp)

(defvar abs-input-files nil
  "List of Abs files to be compiled by \\[abs-next-action].
If nil, the file visited in the current buffer will be used.  If
set, the first element determines the name of the generated Maude
file if generating Maude code.

Add a file-local setting (\\[add-file-local-variable]) to
override the default value.  Put a section like the following at
the end of your buffer:

// Local Variables:
// abs-input-files: (\"file1.abs\" \"file2.abs\")
// End:")
(put 'abs-input-files 'safe-local-variable
     (lambda (list) (every #'stringp list)))

(defvar abs-compile-command nil
  "The compile command called by \\[abs-next-action].
The default behavior is to call \"make\" if a Makefile is in the
current directory, otherwise call the program named by
`abs-compiler-program' on the current file.  This behavior can be
changed by giving `abs-compile-comand' a file-local or dir-local
value.")
(put 'abs-compile-command 'safe-local-variable
     (lambda (a) (and (stringp a)
                      (or (not (boundp 'compilation-read-command))
                          compilation-read-command))))

;;; Put the regular expression for finding error messages here.
;;;
(defconst abs-error-regexp
  "^[^\0-@]+ \"\\(^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by the abs compiler.")

(unless (assoc abs-error-regexp compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist (list abs-error-regexp 1 2)))

;;; flymake support
(defun abs-flymake-init ()
  (when abs-compiler-program
    (list
     abs-compiler-program
     (remove nil (list
                  (when (string= (file-name-nondirectory (buffer-file-name))
                                 "abslang.abs")
                    "-nostdlib")
                  (flymake-init-create-temp-buffer-copy
                   'flymake-create-temp-inplace))))))

(unless (assoc "\\.abs\\'" flymake-allowed-file-name-masks)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.abs\\'" abs-flymake-init)))

;;; Compilation support
(defun abs--file-date-< (d1 d2)
  "Compare file dates D1 and D2, as returned by `file-attributes'."
  (or (and (= (cl-first d1) (cl-first d2))
           (< (cl-second d1) (cl-second d2)))
      (< (cl-first d1) (cl-first d2))))

(defun abs--input-files ()
  (or abs-input-files (list (file-name-nondirectory (buffer-file-name)))))

(defun abs--maude-filename ()
  (or abs-maude-output-file
      (concat (file-name-sans-extension (car (abs--input-files))) ".maude")))

(defun abs--keyabs-filename ()
  (concat (file-name-sans-extension (car (abs--input-files))) ".inv"))

(defun abs--absolutify-filename (filename)
  (if (file-name-absolute-p filename)
      filename
    (concat (file-name-directory (buffer-file-name)) filename)))

(defun abs--guess-module ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward (rx bol (* whitespace) "module" (1+ whitespace)
                            (group (char upper) (* (or (char alnum) "." "_")))))
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun abs--calculate-compile-command (backend)
  (cond (abs-compile-command)
        ((file-exists-p "Makefile") compile-command)
        (t (concat abs-compiler-program
                   " -" (symbol-name backend)
                   " "
                   ;; FIXME: make it work with filenames with spaces
                   (mapconcat (lambda (s) (concat "\"" s "\""))
                              (abs--input-files) " ")
                   (when (eql backend 'maude)
                     (concat " -o \"" (abs--maude-filename) "\""))
                   (when (eql backend 'keyabs)
                     (concat " -o \"" (abs--keyabs-filename) "\""))
                   (when abs-product-name
                     (concat " -product=" abs-product-name))
                   (when (and (eql backend 'maude) abs-use-timed-interpreter)
                     (concat " -timed -limit="
                             (number-to-string abs-clock-limit)))
                   (when (and (eql backend 'maude)
                              (< 0 abs-default-resourcecost))
                     (concat " -defaultcost="
                             (number-to-string abs-default-resourcecost)))
                   " "))))

(defun abs--needs-compilation (backend)
  (let* ((abs-output-file
          (abs--absolutify-filename (pcase backend
                                      (`maude (abs--maude-filename))
                                      (`erlang "gen/erl/Emakefile")
                                      (`java "gen/ABS/StdLib/Bool.java")
                                      ;; FIXME Prolog backend can use -fn outfile
                                      (`prolog "abs.pl")
                                      (`keyabs (abs--keyabs-filename)))))
         (abs-modtime (nth 5 (file-attributes (buffer-file-name))))
         (output-modtime (nth 5 (file-attributes abs-output-file))))
    (or (not output-modtime)
        (abs--file-date-< output-modtime abs-modtime)
        (buffer-modified-p))))

(defun abs--compile-model (backend)
  (let ((compile-command (abs--calculate-compile-command backend)))
   (call-interactively 'compile)))

(defun abs--compile-model-no-prompt (backend)
  (let ((compile-command (abs--calculate-compile-command backend)))
   (compile compile-command)))

(defun abs--run-model (backend)
  "Start the model running on language BACKEND."
  (pcase backend
    (`maude (save-excursion (run-maude))
            (comint-send-string inferior-maude-buffer
                                (concat "in \""
                                        (abs--absolutify-filename
                                         (abs--maude-filename))
                                        "\"\n"))
            (with-current-buffer inferior-maude-buffer
              (sit-for 1)
              (goto-char (point-max))
              (insert "frew start .")
              (comint-send-input)))
    (`erlang (let ((erlang-buffer (or (get-buffer "*erlang*")
                                      (progn (save-excursion
                                               ;; don't propagate
                                               ;; interactive args, if any
                                               (inferior-erlang nil))
                                             (get-buffer "*erlang*"))))
                   (erlang-dir (concat (file-name-directory (buffer-file-name))
                                       "gen/erl"))
                   (module (abs--guess-module))
                   (debug-output abs-debug-output))
               (with-current-buffer erlang-buffer
                 (comint-send-string erlang-buffer
                                     (concat "cd (\"" erlang-dir "\").\n"))
                 (comint-send-string erlang-buffer "make:all([load]).\n")
                 (comint-send-string erlang-buffer
                                     (concat "code:add_paths([\""
                                             erlang-dir "/ebin\", \""
                                             erlang-dir "/deps/cowboy/ebin\", \""
                                             erlang-dir "/deps/cowlib/ebin\", \""
                                             erlang-dir "/deps/ranch/ebin\"]).\n"))
                 (comint-send-string erlang-buffer
                                     (concat "runtime:start(\""
                                             (when debug-output "-d ")
                                             module
                                             "\").\n")))
               (pop-to-buffer erlang-buffer)))
    (`java (let ((java-buffer (save-excursion (shell "*abs java*")))
                 (java-dir (file-name-directory (buffer-file-name)))
                 (module (abs--guess-module)))
             (with-current-buffer java-buffer
               (comint-send-string java-buffer
                                   (concat "cd \"" java-dir "\"\n"))
               (goto-char (point-max))
               (insert "java -cp gen:" abs-java-classpath
                       " " module ".Main && exit")
               (comint-send-input))))
    (other (error "Don't know how to run with target %s" backend))))

(defun abs-next-action (flag)
  "Compile the buffer or load it into Maude.

The language backend for compilation can be chosen by giving a
`C-u' prefix to this command.  The default backend is set via
customizing or setting `abs-target-language' and can be
overridden for a specific abs file by giving a file-local value
via `add-file-local-variable'.

To execute on the Maude backend, remember to make
`abs-interpreter.maude' accessible to Maude, either by copying or
symlinking that file to the current directory, or via the
`MAUDE_LIB' environment variable.

To execute on the Java backend, set `abs-java-classpath' to
include the file absfrontend.jar.

Argument FLAG will prompt for language backend to use if 1."
  (interactive "p")
  (let ((backend (if (= 1 flag)
                     abs-target-language
                   (intern (completing-read "Target language: "
                                            '("maude" "erlang" "java" "keyabs")
                                            nil t nil nil "maude")))))
    (if (abs--needs-compilation backend)
        (abs--compile-model backend)
      (abs--run-model backend))))

;;; Movement

(defsubst abs--inside-string-or-comment-p ()
  (let ((state (save-excursion (syntax-ppss))))
    (or (nth 3 state) (nth 4 state))))

(defvar abs-definition-begin-re
  (rx (and (or "interface" "class" "def" "data" "type") blank))
  "Regex of beginning of Abs definition.")

(defun abs-beginning-of-definition ()
  "Move backward to the beginning of the current definition.

A definition can be interface, class, datatype or function."
  (interactive)
  (catch 'found
    (while (re-search-backward abs-definition-begin-re nil 'move)
      (unless (abs--inside-string-or-comment-p)
        (throw 'found t))))
  (move-beginning-of-line nil))

(defun abs-end-of-definition ()
  "Move forward to the end of the current definition."
  (interactive)
  (let ((startpos (point)))
    ;; FIXME: this is slightly buggy.
    (forward-char)
    (re-search-forward abs-definition-begin-re nil 'move)
    (re-search-forward abs-definition-begin-re nil 'move)
    (catch 'found
      (while (re-search-backward (rx (or "}" ";")) startpos)
        (unless (abs--inside-string-or-comment-p)
          (throw 'found t))))
    (forward-char)))

;;; Indentation

(defun abs--prev-code-line ()
  "Moves point to first non-ws char of the previous line containing code.
Returns point.  Purely whitespace and comment lines are skipped."
  (back-to-indentation)
  (while (forward-comment -1))
  (back-to-indentation)
  (point))

(defun abs--calculate-indentation ()
  (let* ((this-parse-status (save-excursion 
                              (syntax-ppss (line-beginning-position))))
         (prev-parse-status (save-excursion 
                              (syntax-ppss (abs--prev-code-line))))
         (end-parse-status (save-excursion (syntax-ppss (line-end-position))))
         (prev-line-indent (save-excursion (abs--prev-code-line)
                                           (current-indentation)))
         (depth-difference-prev-line (- (nth 0 this-parse-status)
                                        (nth 0 prev-parse-status)))
         (depth-difference-this-line (- (nth 0 this-parse-status)
                                        (nth 0 end-parse-status))))
    (cond
     ((save-excursion (back-to-indentation)
                      (looking-at abs-definition-begin-re))
      ;; At the start of a definition: no indent
      0)
     ((> depth-difference-prev-line 0) ; different paren depth
      (max 0 (+ prev-line-indent (* abs-indent depth-difference-prev-line))))
     ((> depth-difference-this-line 0)   ; closing paren here
      (- prev-line-indent (* abs-indent depth-difference-this-line)))
     (t                        ; Default: indent like the previous line.
      (save-excursion (abs--prev-code-line) (current-indentation))))))

(defun abs-indent-line ()
  "Indent the current line as Abs code.
Uses the variable `abs-indent'."
  (interactive)
  (let ((save-point-position (> (current-column) (current-indentation)))
	(indentation (abs--calculate-indentation)))
    (if save-point-position
	(save-excursion (indent-line-to indentation))
      (indent-line-to indentation))))

;;; Putting it all together.

(define-derived-mode abs-mode fundamental-mode "Abs"
  "Major mode for editing Abs files.

The following keys are set:
\\{abs-mode-map}"
  :group 'abs
  :syntax-table abs-mode-syntax-table
  (define-key abs-mode-map "\C-c\C-c" 'abs-next-action)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(abs-font-lock-keywords))
  ;; Movement
  (set (make-local-variable 'beginning-of-defun-function)
       'abs-beginning-of-definition)
  (set (make-local-variable 'end-of-defun-function) 'abs-end-of-definition)
  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'abs-indent-line)
  ;; imenu
  (setq imenu-generic-expression abs-imenu-generic-expression)
  (setq imenu-syntax-alist abs-imenu-syntax-alist)
  ;; Menu
  (easy-menu-add abs-mode-menu abs-mode-map)
  ;; speedbar support
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".abs")))

;;; Set up the "Abs" pull-down menu
(easy-menu-define abs-mode-menu abs-mode-map
  "Abs mode menu."
  '("Abs"
    ["Compile" (abs--compile-model-no-prompt abs-target-language) :active t]
    ["Run" (abs--run-model abs-target-language)
     :active (not (abs--needs-compilation abs-target-language))]
    "---"
    ("Select Backend"
     ["Maude" (setq abs-target-language 'maude)
      :active t
      :style radio
      :selected (eq abs-target-language 'maude)]
     ["Erlang" (setq abs-target-language 'erlang)
      :active t
      :style radio
      :selected (eq abs-target-language 'erlang)]
     ["Key-abs" (setq abs-target-language 'keyabs)
      :active t
      :style radio
      :selected (eq abs-target-language 'keyabs)])
    ("Maude Backend Options"
     ["Timed interpreter"
      (setq abs-use-timed-interpreter (not abs-use-timed-interpreter))
      :active t :style toggle
      :selected abs-use-timed-interpreter])
    ("Erlang Backend Options"
     ["Debugging output"
      (setq abs-debug-output (not abs-debug-output))
      :active t :style toggle
      :selected abs-debug-output])))

(unless (assoc "\\.abs\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.abs\\'" . abs-mode)))


(provide 'abs-mode)
;;; abs-mode.el ends here
