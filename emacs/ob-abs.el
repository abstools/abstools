;;; ob-abs.el --- Babel Functions for abs -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Rudolf Schlatte

;; Author: Rudolf Schlatte <rudi@constantly.at>
;; Keywords: literate programming, reproducible research, data, docs, languages

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

;; Org-Babel support for evaluating ABS code.  Derived from ob-C.el
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:

(require 'abs-mode)
(require 'ob)

;; * [2/4] TODOs
;; 
;; - [X] auto-create module
;; - [X] parameters: simple
;; - [ ] parameters: list
;; - [ ] parameters: matrix
;; 
;; * [0/1] Maybe
;; - [ ] auto-create main block when code block is not surrounded by { }

(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-remove-indentation "org" (code &optional n))
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("ABS" . "abs"))

(defvar org-babel-default-header-args:abs '())

(defconst org-babel-header-args:abs '((module . :any))
  "ABS-specific header arguments.")

(defun org-babel-execute:abs (body params)
  "Execute a block of ABS code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((tmp-src-file (org-babel-temp-file "abs-src-" ".abs"))
         (full-body (org-babel-expand-body:abs body params)))
  (with-temp-file tmp-src-file (insert full-body))
  (org-babel-eval
   (format "cd %s ; %s -erlang %s"
           (file-name-directory tmp-src-file)
           abs-compiler-program
           (org-babel-process-file-name tmp-src-file))
   "")
  (let ((results
         (org-babel-eval
          (format "cd %s ; gen/erl/run"
                  (file-name-directory tmp-src-file))
          "")))
    (when results
      (setq results (org-trim (org-remove-indentation results)))
      ;; (org-babel-reassemble-table
      ;;  (org-babel-result-cond (cdr (assq :result-params params))
      ;;    (org-babel-read results t)
      ;;    (let ((tmp-file (org-babel-temp-file "abs-")))
      ;;      (with-temp-file tmp-file (insert results))
      ;;      (org-babel-import-elisp-from-file tmp-file)))
      ;;  (org-babel-pick-name
      ;;   (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
      ;;  (org-babel-pick-name
      ;;   (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))
      ))))

(defun org-babel-expand-body:abs (body params)
  "Expand a block of ABS code with org-babel according to its header arguments."
  (let ((vars (org-babel--get-vars params))
        (colnames (cdr (assq :colname-names params)))
        (module (or (cdr (assq :module params)) "Org")))
    (mapconcat 'identity
               (list
                (when module (concat  "module " module ";"))
                ;; variables
                (mapconcat 'org-babel-abs-var-exports vars "\n")
                (mapconcat 'org-babel-abs-var-to-abs vars "\n")

                ;; ;; table sizes
                ;; (mapconcat 'org-babel-abs-table-sizes-to-abs vars "\n")
                ;; ;; tables headers utility
                ;; (when colnames
                ;;   (org-babel-abs-utility-header-to-abs))
                ;; ;; tables headers
                ;; (mapconcat 'org-babel-abs-header-to-abs colnames "\n")
                "\n"
                body)
               "\n")))

(defun org-babel-prep-session:abs (_session _params)
  "This function does nothing as abs is a compiled language with no
support for sessions"
  (error "ABS is a compiled language -- no support for sessions"))

(defun org-babel-load-session:abs (_session _body _params)
  "This function does nothing as abs is a compiled language with no
support for sessions"
  (error "ABS is a compiled language -- no support for sessions"))

;; helper functions

(defun org-babel-abs-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
        (cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-abs-val-to-abs-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  ;; TODO
  (let* ((basetype (org-babel-abs-val-to-base-type val))
         (type
          (pcase basetype
            (`integerp '("Int" "%d"))
            (`floatp '("Float" "%f"))
            (`stringp '("String" "\"%s\""))
            (_ (error "unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ;; ((and (listp val) (listp (car val))) ;; a table
     ;;  `(,(car type)
     ;;    (lambda (val)
     ;;      (cons
     ;;       (format "[%d][%d]" (length val) (length (car val)))
     ;;       (concat
     ;;        (if (eq org-babel-c-variant 'd) "[\n" "{\n")
     ;;        (mapconcat
     ;;         (lambda (v)
     ;;           (concat
     ;;            (if (eq org-babel-c-variant 'd) " [" " {")
     ;;            (mapconcat (lambda (w) (format ,(cadr type) w)) v ",")
     ;;            (if (eq org-babel-c-variant 'd) "]" "}")))
     ;;         val
     ;;         ",\n")
     ;;        (if (eq org-babel-c-variant 'd) "\n]" "\n}"))))))
     ;; ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
     ;;  `(,(car type)
     ;;    (lambda (val)
     ;;      (cons
     ;;       (format "[%d]" (length val))
     ;;       (concat
     ;;        (if (eq org-babel-c-variant 'd) "[" "{")
     ;;        (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
     ;;        (if (eq org-babel-c-variant 'd) "]" "}"))))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-abs-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ;; ((or (listp val) (vectorp val))
   ;;  (let ((type nil))
   ;;    (mapc (lambda (v)
   ;;            (pcase (org-babel-abs-val-to-base-type v)
   ;;              (`stringp (setq type 'stringp))
   ;;              (`floatp
   ;;               (if (or (not type) (eq type 'integerp))
   ;;                   (setq type 'floatp)))
   ;;              (`integerp
   ;;               (unless type (setq type 'integerp)))))
   ;;          val)
   ;;    type))
   (t 'stringp)))

(defun org-babel-abs-var-exports (pair)
  "Generate export clause for a var defined in the header."
  (let ((var (car pair)))
    (when (symbolp var) (setq var (symbol-name var)))
    (setq var (downcase var))
    (concat "export " var ";\n")))

(defun org-babel-abs-var-to-abs (pair)
  "Convert an elisp val into a string of abs code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp var) (setq var (symbol-name var)))
    (setq var (downcase var))
    (when (symbolp val) (setq val (symbol-name val)))
    (let* ((type-data (org-babel-abs-val-to-abs-type val))
           (type (car type-data))
           (formatted (org-babel-abs-format-val type-data val))
           (suffix (car formatted))
           (data (cdr formatted)))
      (format "def %s %s%s() = %s;"
              type
              var
              suffix
              data))))

;; (defun org-babel-abs-table-sizes-to-abs (pair)
;;   "Create constants of table dimensions, if PAIR is a table."
;;   ;; TODO
;;   (when (listp (cdr pair))
;;     (cond
;;      ((listp (cadr pair)) ;; a table
;;       (concat
;;        (format "Int %s_rows = %d;" (car pair) (length (cdr pair)))
;;        "\n"
;;        (format "Int %s_cols = %d;" (car pair) (length (cadr pair)))))
;;      (t ;; a list declared in the #+begin_src line
;;       (format "Int %s_cols = %d;" (car pair) (length (cdr pair)))))))

;; (defun org-babel-abs-utility-header-to-abs ()
;;   "Generate a utility function to convert a column name
;; into a column number."
;;   ;; TODO
;;   (pcase org-babel-c-variant
;;     ((or `c `cpp)
;;      "int get_column_num (int nbcols, const char** header, const char* column)
;; {
;;   int c;
;;   for (c=0; c<nbcols; c++)
;;     if (strcmp(header[c],column)==0)
;;       return c;
;;   return -1;
;; }
;; ")
;;     (`d
;;      "int get_column_num (string[] header, string column)
;; {
;;   foreach (c, h; header)
;;     if (h==column)
;;       return to!int(c);
;;   return -1;
;; }
;; ")))

;; (defun org-babel-abs-header-to-abs (head)
;;   "Convert an elisp list of header table into an abs list
;; specifying a variable with the name of the table."
;;   ;; TODO
;;   (let ((table (car head))
;;         (headers (cdr head)))
;;     (concat
;;      (format
;;       "List<String> %s_header = list[%s];"
;;       table
;;       (mapconcat (lambda (h) (format "%S" h)) headers ","))
;;      "\n"
;;      (format
;;       "String %s_h (Int row, Int col) = nth(get_column_num(%s_header,col), nth(row, table)); }"
;;       table table table))))

(provide 'ob-abs)

;;; ob-abs.el ends here
