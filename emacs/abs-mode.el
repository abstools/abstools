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
(eval-when-compile (require 'rx))
(require 'maude-mode)

;;; Code:

;;; Customization
(defgroup abs nil
  "Major mode for editing files in the programming / modeling language Abs."
  :group 'languages)

(defcustom abs-compiler-command (executable-find "generateMaude")
  "Path to the Abs compiler.
The given command will be called with a filename, followed by
\"-o\" and the name of an output file."
  :type 'file
  :group 'abs)

(defcustom abs-indent standard-indent
  "The width of one indentation step for Abs code."
  :type 'integer
  :group 'abs)
(put 'abs-indent 'safe-local-variable 'integerp)

(defcustom abs-use-timed-interpreter nil
  "Controls whether to compile Abs code using the timed interpreter by default.
This influences the default compilation command executed by
\\[abs-next-action].  Note that you can set this variable as a
file-local variable as well."
  :type 'boolean
  :group 'abs)
(put 'abs-use-timed-interpreter 'safe-local-variable 'booleanp)

(defcustom abs-mode-hook (list 'imenu-add-menubar-index)
  "Hook for customizing `abs-mode'."
  :type 'hook
  :options (list 'imenu-add-menubar-index)
  :group 'abs)

(defcustom abs-clock-limit 100
  "Default limit value for the clock in the timed Abs interpreter.
Note that you can set this variable as a file-local variable as well."
  :type 'integer
  :group 'abs)
(put 'abs-clock-limit 'safe-local-variable 'integerp)

(defcustom abs-default-resourcecost 0
  "Default resource cost of executing one ABS statement."
  :type 'integer
  :group 'abs)
(put 'abs-default-resourcecost 'safe-local-variable 'integerp)

;;; Making faces
(defface abs-keyword-face '((default (:inherit font-lock-keyword-face)))
  "Face for Abs keywords"
  :group 'abs)
(defvar abs-keyword-face 'abs-keyword-face
  "Face for Abs keywords")

(defface abs-constant-face '((default (:inherit font-lock-constant-face)))
  "Face for Abs constants"
  :group 'abs)
(defvar abs-constant-face 'abs-constant-face
  "Face for Abs constants")

(defface abs-function-name-face '((default (:inherit font-lock-function-name-face)))
  "Face for Abs function-names"
  :group 'abs)
(defvar abs-function-name-face 'abs-function-name-face
  "Face for Abs function-names")

(defface abs-type-face '((default (:inherit font-lock-type-face)))
  "Face for Abs types"
  :group 'abs)
(defvar abs-type-face 'abs-type-face
  "Face for Abs types")

(defface abs-variable-name-face '((default (:inherit font-lock-variable-name-face)))
  "Face for Abs variables"
  :group 'abs)
(defvar abs-variable-name-face 'abs-variable-name-face
  "Face for Abs variables")

(defconst abs--cid-regexp "\\_<[[:upper:]]\\(?:\\sw\\|\\s_\\)*\\_>")
(defconst abs--id-regexp
  "\\_<\\(?:[[:lower:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\|'\\)*\\_>")

;;; Font-lock for Abs.
;;;
(defconst abs-keywords
  (eval-when-compile
    (regexp-opt
     '("module" "import" "export" "from"              ; the top levels
       "data" "type" "def" "interface" "class"
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
       "await" "assert" "get" "skip" "suspend"
       "original" "movecogto"
       "duration"                       ; guard / statement
       ) 'words))
  "List of Abs keywords.")
(defconst abs-constants
  (eval-when-compile
    (regexp-opt
     '("True" "False" "null" "this" "Nil" "Cons")
     'words))
  "List of Abs special words")

(defvar abs-font-lock-keywords
    (list
     ;; order is important here; earlier entries override later ones
     (cons abs-keywords 'abs-keyword-face)
     (cons abs-constants 'abs-constant-face)
     (cons (concat "\\(" abs--cid-regexp "\\)") 'abs-type-face)
     (list (concat "\\(" abs--id-regexp "\\)[[:space:]]*(") 1 'abs-function-name-face)
     (cons (concat "\\(" abs--id-regexp "\\)") 'abs-variable-name-face)
     (list "\\<\\(# \w+\\)\\>" 1 'font-lock-warning-face t))
    "Abs keywords")

;;; Abs syntax table
(defvar abs-mode-syntax-table (copy-syntax-table)
  "Syntax table for abs-mode")
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
            (group (char upper) (* (char alnum))))
       1))
  "Imenu expression for abs-mode.  See `imenu-generic-expression'.")

;;; Compiling the current buffer.
;;;
(defvar abs-output-file nil
  "The Maude file that will be loaded by \\[abs-next-action].
Defaults to the buffer filename with a \".maude\" extension.

Add a file-local setting to override the default value.")
(put 'abs-output-file 'safe-local-variable 'stringp)

(defvar abs-compile-command nil
  "The compile command called by \\[abs-next-action].
The default behavior is to call \"make\" if a Makefile is in the
current directory, otherwise call the program named by
`abs-compiler-command' on the current file.  This behavior can be
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

(defun abs--file-date-< (d1 d2)
  "Compare two file dates, as returned by `file-attributes'."
  (or (and (= (first d1) (first d2))
           (< (second d1) (second d2)))
      (< (first d1) (first d2))))

(defun abs--maude-filename ()
  (or abs-output-file 
      (concat (file-name-sans-extension 
               (file-name-nondirectory (buffer-file-name)))
              ".maude")))

(defun abs--absolute-maude-filename ()
  (let ((abs-output-file (abs--maude-filename)))
    (if (file-name-absolute-p abs-output-file)
        abs-output-file
      (concat (file-name-directory (buffer-file-name)) abs-output-file))))

(defun abs--calculate-compile-command ()
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (makefilep (file-exists-p "Makefile"))
        (maude-filename (abs--maude-filename)))
    (cond (abs-compile-command)
          (makefilep compile-command)
          (t (concat abs-compiler-command " " filename " -o " maude-filename
                     (if abs-use-timed-interpreter
                         (concat " -timed -limit=" (number-to-string abs-clock-limit))
                       "")
                     (if (< 0 abs-default-resourcecost)
                         (concat " -defaultcost="
                                 (number-to-string abs-default-resourcecost)))
                     " ")))))

(defun abs-next-action ()
  "Compile the buffer or load it into Maude.

Remember to make `abs-interpreter.maude' accessible to Maude!
This can be done either by copying or symlinking that file to the
current directory, or via the `MAUDE_LIB' environment variable.

To determine whether to compile or load, `abs-next-action'
assumes that the result of compilation is stored in a file with
the same name as the current buffer but with a `.maude'
extension.  Set the variable `abs-output-file' to override this."
  (interactive)
  (let* ((abs-maude-file (abs--absolute-maude-filename))
         (abs-modtime (nth 5 (file-attributes (buffer-file-name))))
         (maude-modtime (nth 5 (file-attributes abs-maude-file))))
    (if (or (not maude-modtime)
            (abs--file-date-< maude-modtime abs-modtime)
            (buffer-modified-p))
        (let ((compile-command (abs--calculate-compile-command)))
          (call-interactively 'compile))
      (run-maude)
      (message "Maude loading %s" abs-maude-file)
      (comint-send-string inferior-maude-buffer
                          (concat "in " abs-maude-file "\n"))
      (with-current-buffer inferior-maude-buffer
        (sit-for 1)
        (goto-char (point-max))
        (insert "frew start .")
        (comint-send-input)))))

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
  ;; speedbar support
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".abs")))


(unless (assoc "\\.abs\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.abs\\'" . abs-mode)))


(provide 'abs-mode)
;;; abs-mode.el ends here
