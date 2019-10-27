;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Version: 0.2.4
;; Package-Requires: ((emacs "24.4"))
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
;; selections. Bind `coleslaw-dispatch' to a key to make coleslaw discover the
;; mode.

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;; Consider also installing the coleslaw-snippet package to generate your snippets.
;;; Code:

(require 'cl-lib)

(defvar coleslaw-mode-hook nil "Coleslaw-mode, for editing static web content.")
(defvar coleslaw-separator ";;;;;" "Character that separates keys and values in the coleslaw header")

(defvar coleslaw-modes nil
  (concat
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

(add-hook 'coleslaw-mode-hook 'coleslaw-dispatch)

;;;###autoload
(defun coleslaw-setup ()
  "Setup your coleslaw like the author suggests (conservative edits only).
strongly recommended!  Enable auto insertion for .page and .post
files, enable such basic editing modes as the mode function
`markdown-mode', the mode function `lisp-mode', the mode function
`html-mode', or the mode function `rst-mode' based on the format
header field.  Conservative additions only."
  (setq coleslaw-modes
        '(("md" . (markdown-mode))
          ("cl-who" . (lisp-mode))
          ("html" . (html-mode))
          ("rst" . (rst-mode)))))

(defun coleslaw-dispatch-format (format)
  "Select the mode for a file of type FORMAT."
  (mapc (lambda (mode) (funcall mode))
        (cdr (assoc format coleslaw-modes #'string-equal))))

(defun coleslaw-dispatch ()
  "Set modes based on this buffer's 'format: (md, cl-who, etc.)' metadata line."
  (interactive)
  (when (coleslaw--header-detected)
    (coleslaw-dispatch-format (coleslaw--header-field "format"))))

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
    (coleslaw--re-search-whole (concat field
                                       ":"
                                       "[\t ]*\\(?1:.*\\)\n")
                               nil t)))

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " Coleslaw"
  (dolist (type '("\\.page\\'" "\\.post\\'"))
    (add-to-list 'auto-mode-alist (cons type 'coleslaw-mode)))
  (coleslaw-dispatch))

(provide 'coleslaw)
;;; coleslaw.el ends here
