;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Version: 0.2.4
;; Package-Requires: ((emacs "24"))
;; Keywords: lisp wp files convenience
;; URL: https://github.com/equwal/coleslaw/
;; Homepage: https://spensertruex.com/coleslaw-mode
;; This file is not part of GNU Emacs, but you want to use  GNU Emacs to run it.
;; This file is very free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Please add (coleslaw-setup) to your init file for the author's mode
;; selections.

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;;; Code:

(defvar coleslaw-mode-hook nil "Coleslaw-mode, for editing static web content.")

(defvar coleslaw-formats (list "md" "cl-who" "rst" "html" "org")
  "The format header values that coleslaw will allow to be auto-inserted.")

(defvar coleslaw-separator ";;;;;"
  "The string used between the coleslaw headers as in the example:
;;;;;
title: Example
format: cl-who
date: 2019-06-15
;;;;;
Where the separator is \";;;;;\".")

(defvar coleslaw-auto-insert (when (boundp 'coleslaw-auto-insert)
                               coleslaw-auto-insert)
  "Predicate to insert the skeleton on opening a new Coleslaw file type.
unless the function `coleslaw-setup' is ran, when it is set to T.")

(defvar coleslaw-modes nil
  (concatenate 'string
               "Modes based on the regex (special characters quoted)"
               (regexp-quote coleslaw-separator)
               "
  format: FORMAT
" (regexp-quote coleslaw-separator) "
  headers in the coleslaw file. A simple default choice is:
  (setq coleslaw-modes
        '((\"md\" . (markdown-mode))
          (\"cl-who\" . (lisp-mode))
          (\"html\" . (html-mode))
          (\"rst\" . (rst-mode))))
  in your init file."))

(defun coleslaw--valid-format (str)
  "Determine if the STR is permissible for a format: header in Coleslaw."
  (when (stringp str)
    (some (lambda (x) (string-equal x str)) coleslaw-formats)))

(defun coleslaw-setup ()
  "Setup your coleslaw like the author suggests (conservative edits only).
strongly recommended!  Enable auto insertion for .page and .post
files, enable such basic editing modes as the mode function
`markdown-mode', the mode function `lisp-mode', the mode function
`html-mode', or the mode function `rst-mode' based on the format
header field.  Conservative additions only."
  (when (require 'autoinsert nil 'installed)
    (setq coleslaw-auto-insert t)
    (dolist (type '(".page" ".post"))
      (add-to-list 'auto-insert-alist (cons type 'coleslaw-insert-header))))
  (dolist (type '("\\.page\\'" "\\.post\\'"))
    (add-to-list 'auto-mode-alist (cons type 'coleslaw-mode)))
  (add-hook 'coleslaw-mode-hook 'coleslaw--dispatch)
  (setq coleslaw-modes
        '(("md" . (markdown-mode))
          ("cl-who" . (lisp-mode))
          ("html" . (html-mode))
          ("rst" . (rst-mode)))))

(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

(defun coleslaw--mode-spawn (format)
  "Select the mode for a file of type FORMAT."
  (mapc (lambda (mode) (funcall mode))
        (cdr (assoc format coleslaw-modes #'string-equal))))

(defun coleslaw--dispatch ()
  "Set modes based on this buffer's 'format: (md, cl-who, etc.)' metadata line."
  (when (coleslaw--header-detected)
    (coleslaw--mode-spawn (coleslaw--header-field "format"))))

(defun coleslaw--insist-format (first-prompt &optional second-prompt)
  "Insist that the format inserted be a valid coleslaw format.
Add formats to the `coleslaw-formats' list for new features or
nonstandard formats.  FIRST-PROMPT and SECOND-PROMPT are used to
prompt the user at the minibuffer.  The FIRST-PROMPT is for the
first, and SECOND-PROMPT is used in subsequent requests.  If
FIRST-PROMPT is NIL, the second-prompt is only used."
  (let ((res (read-from-minibuffer (if first-prompt
                                       first-prompt
                                     second-prompt))))
    (if (not (coleslaw--valid-format res))
        (coleslaw--insist-format nil second-prompt))))
;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw file type."
  (skeleton-insert '(nil str
                         "\ntitle: "
                         (skeleton-read "title: ")
                         "\nformat: "
                         (coleslaw--insist-format "format: "
                                                  "Bad format, try another format: ")
                         (if (coleslaw--bufftype ".page")
                             (concat "\nurl: " (skeleton-read "url: "))
                           "")
                         (if (coleslaw--bufftype ".post")
                             (if (y-or-n-p "Insert excerpt? ")
                                 (concat "\nexcerpt: "
                                         (skeleton-read "excerpt: "))
                               "")
                           "")
                         "\ndate: "
                         (format-time-string "%Y-%m-%d" (current-time))
			 "\n" str)
		   0 (regexp-quote coleslaw-separator))
  (coleslaw--dispatch))

(defun coleslaw--re-search-whole (regex &optional bound noerror count)
  "Search forward and backwards from the point in the buffer for REGEX.
BOUND, NOERROR, and COUNT are arguments to `re-search-forward'
and `re-search-backward'."
  (let ((args (list regex bound noerror count)))
    (if (apply #'re-search-forward args)
        (match-string 1)
      (when (apply #'re-search-backward args)
        (match-string 1)))))

(defun coleslaw--header-detected ()
  "Detect if a header is already in the file."
  ;; pointer in fields, under fields, or in separator
  (or (re-search-forward (regexp-quote coleslaw-separator)
                         nil t 1)
      (re-search-backward (regexp-quote coleslaw-separator)
                          nil t)))

(defun coleslaw--header-field (field)
  "Search the current bufffer for the header FIELD.
Don't include the colon in the FIELD string (e.g. \"format\")."
  (when (coleslaw--header-detected)
    (coleslaw--re-search-whole (concatenate 'string
                                            field
                                            ":"
                                            "[\t ]*\\(?1:.*\\)\n")
                               nil t)))

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " Coleslaw"
  (when (require 'autoload) (auto-insert))
  (coleslaw--dispatch))

(provide 'coleslaw)
;;; coleslaw.el ends here
