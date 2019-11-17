;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Version: 0.2.5
;; Package-Requires: ((emacs "24.5"))
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
(defvar coleslaw-separator ";;;;;"
  "The string used between the coleslaw headers as in the example:
;;;;;
title: Example
format: cl-who
date: 2019-06-15
;;;;;
Where the separator is \";;;;;\".")

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
(defvar coleslaw-formats (list "md" "cl-who" "rst" "html" "org")
  "The format header values that coleslaw will allow to be auto-inserted.")

(defun coleslaw--valid-format (str)
  "Determine if the STR is permissible for a format: header in Coleslaw."
  (when (stringp str)
    (cl-some (lambda (x)
               (string-equal x str))
             coleslaw-formats)))
(defun coleslaw--url-charp (char)
  "Is the CHAR legal in a static URL according to RFC3986? See
Section 2."
  (find char (concat "abcdefghijklmnopqrstuvwxyz"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     "0123456789-_~.%")))
(defun coleslaw--urlp (str)
  "Is the STR of valid URL characters?"
  (every #'coleslaw--url-charp str))
(defun coleslaw--valid-tags (str)
  (coleslaw-urlp str))

(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))


(defun coleslaw--insist (predicate prompt1 promt2)
  "Insist based on the PREDICATE that the minibuffer reponse to
PROMPT1 is valid; otherwise, show PROMPT2 until correct."
  (let ((res (read-from-minibuffer (if first-prompt
                                       first-prompt
                                     second-prompt))))
    (if (coleslaw--valid-format res)
        res
      (coleslaw--insist-format nil second-prompt))))
(defun coleslaw--field (predicate field-prompt &optional fail-prompt)
  (concat "\n" field-prompt
          (coleslaw--insist predicate field-prompt
                            (if fail-prompt
                                fail-prompt
                              (concat "INVALID! " field-prompt " ")))))
;;;###autoload
(defmacro coleslaw--skeleton-when (condition &rest code)
    "Generate a `when' clause, but return the empty string for
false conditions."
    `(if ,condition
         (concat ,@code)
       ""))
;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw file type."
  (interactive)
  (skeleton-insert '(nil coleslaw-separator
                         (coleslaw--field #'identity "title:")
                         (coleslaw--field #'coleslaw--valid-format
                                   "format:"
                                   "format be one of `coleslaw-formats':")
                         (coleslaw-skeleton-when
                          (y-or-n-p "Insert tags?")
                          (coleslaw--field #'coleslaw-urlp
                                    "tags: "
                                    "Invalid character for a tag. tags"))
                         (coleslaw-skeleton-when
                          (coleslaw--bufftype ".page")
                          (coleslaw--field #'coleslaw--urlp "url: " "Bad URL:"))
                         (coleslaw-skeleton-when
                          (and (coleslaw-bufftype ".post") (y-or-n-p "Insert excerpt?"))
                          (coleslaw--field #'identity "excerpt:"))
                         "\ndate: "
                         (format-time-string "%Y-%m-%d" (current-time))
			 "\n" coleslaw-separator "\n") 0))


(defun coleslaw-mode-regex (mode)
  (concat coleslaw-separator
          "\\(\n\\|.\\)+"
          "format:[\t ]"
          mode
          "\\(\n\\|.\\)+"
          coleslaw-separator))
;;;###autoload
(defun coleslaw-setup ()
  "Setup your coleslaw like the author suggests (conservative edits only).
strongly recommended!  Enable auto insertion for .page and .post
files, enable such basic editing modes as the mode function
`markdown-mode', the mode function `lisp-mode', the mode function
`html-mode', or the mode function `rst-mode' based on the format
header field.  Conservative additions only."
  (setq coleslaw-modes
        '(("md" . markdown-mode)
          ("cl-who" . lisp-mode)
          ("html" . html-mode)
          ("rst" . rst-mode)))
  (cl-loop for mode in coleslaw-modes
           do (add-to-list 'magic-mode-alist
                           (cons (coleslaw-mode-regex (car mode))
                                 (cdr mode))))

  (setq auto-insert t)
  (add-to-list 'auto-insert-alist '("\\.\\(page\\|post\\)\\'" . coleslaw-insert-header)))

(defvar coleslaw-formats (list "md" "cl-who" "rst" "html" "org")
  "The format header values that coleslaw will allow to be auto-inserted.")

(defun coleslaw--valid-format (str)
  "Determine if the STR is permissible for a format: header in Coleslaw."
  (when (stringp str)
    (cl-some (lambda (x) (string-equal x str)) coleslaw-formats)))


(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

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
    (if (coleslaw--valid-format res)
        res
      (coleslaw--insist-format nil second-prompt))))
;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw file type."
  (interactive)
  (skeleton-insert '(nil str
                         "\ntitle: "
                         (skeleton-read "title: ")
                         "\nformat: "
                         (coleslaw--insist-format "format: "
                                                  "format be one of `coleslaw-formats':")
                         (if (coleslaw--bufftype ".page")
                             (concat "\nurl: "
                                     (coleslaw--insist-url "url: "
                                                           "Bad url datum. Should be a word like \"mypost\": "))
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
		               0 (regexp-quote coleslaw-separator)))


;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " Coleslaw"
  (dolist (type '("\\.page\\'" "\\.post\\'"))
    (add-to-list 'auto-mode-alist (cons type 'coleslaw-mode))))

(provide 'coleslaw)
;;; coleslaw.el ends here
