;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: equwal 2018-12-09
;; Version: 0.1.3 [2019-06-15]
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

(defvar coleslaw-mode-hook nil)

(defvar coleslaw-header-separator ";;;;;"
  "The string used between the top and bottom of the coleslaw
  headers, as in the example:
;;;;;
title: Example
format: cl-who
date: 2019-06-15
;;;;;")

(defvar coleslaw-default-format-modes nil
  (concatenate 'string
               "Modes based on the regex (special characters quoted)"
               (regexp-quote coleslaw-header-separator)
               "
  format: FORMAT
" (regexp-quote coleslaw-header-separator) "
  headers in the coleslaw file. A simple default choice is:
  (setq coleslaw-default-format-modes
        '((\"md\" . (markdown-mode))
          (\"cl-who\" . (lisp-mode))
          (\"html\" . (html-mode))
          (\"rst\" . (rst-mode))))
  in your init file."))

(defvar coleslaw-mode-map
  (make-sparse-keymap)
  "Keymap for COLESLAW minor mode.")

(defun coleslaw-setup ()
  "Setup your coleslaw like the author suggests (conservative edits only).
strongly recommended!  set M-; to `coleslaw-insert-header-or-dispatch', enable
auto insertion for .page and .post files, enable such basic editing modes as
markdown-mode, lisp-mode, html-mode, and rst-mode based on the format header
field."
  (dolist (type '(".page" ".post"))
    (add-to-list 'auto-insert-alist (cons type 'coleslaw-insert-header)))
  (dolist (type '("\\.page\\'" "\\.post\\'"))
    (add-to-list 'auto-mode-alist (cons type 'coleslaw-mode)))
  (define-key coleslaw-mode-map (kbd "M-;") 'coleslaw-insert-header-or-dispatch)
  (add-hook 'coleslaw-mode-hook 'coleslaw--dispatch)
  (setq coleslaw-default-format-modes
        '(("md" . (markdown-mode))
          ("cl-who" . (lisp-mode))
          ("html" . (html-mode))
          ("rst" . (rst-mode)))))

(defun coleslaw-insert-header-or-dispatch ()
  "Insert the coleslaw headers into this file if they don't
already exist, or dispatch the modes based on 'format: MODE' if
it is already there."
  (interactive)
  (if (coleslaw--header-detected)
      (coleslaw--dispatch)
    (coleslaw-insert-header)))

(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

(defun coleslaw--mode-spawn (format)
  "Select the mode for a file of type FORMAT."
  (mapc (lambda (mode) (funcall mode))
        (cdr (assoc format coleslaw-default-format-modes #'string-equal))))

(defun coleslaw--dispatch ()
  "Set modes based on this buffer's 'format: (md, cl-who, etc.)'
  metadata line."
  (when (coleslaw--header-detected)
    (coleslaw--mode-spawn (coleslaw--header-field "format"))))

;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw
file type."
  (skeleton-insert '(nil str
                         "\ntitle: "
                         (skeleton-read "title: ")
                         "\nformat: "
                         (skeleton-read "format: ")
                         (if (coleslaw--bufftype ".page")
                             "\nurl: "
                           "")
                         (if (coleslaw--bufftype ".page")
                             (skeleton-read "url: ")
                           "")
                         (if (coleslaw--bufftype ".post")
                             "\nexcerpt: "
                           "")
                         (if (coleslaw--bufftype ".post")
                             (skeleton-read "excerpt: ")
                           "")
                         "\ndate: "
                         (skeleton-read "date: ")
                         "\n"
                         str) 0 (regexp-quote coleslaw-header-separator))
  (move-end-of-line 0)
  (coleslaw--dispatch))

(defun coleslaw--re-search-whole (regex &optional bound noerror count)
  (let ((args (list regex bound noerror count)))
    (if (apply #'re-search-forward args)
        (match-string 1)
      (when (apply #'re-search-backward args)
        (match-string 1)))))

(defun coleslaw--header-detected ()
  "Detect if a header is already in the file."
  ;; pointer in fields, under fields, or in separator
  (or (re-search-forward (regexp-quote coleslaw-header-separator)
                         nil t 1)
      (re-search-backward (regexp-quote coleslaw-header-separator)
                          nil t)))

(defun coleslaw--header-field (field)
  "Search the current bufffer for the header field."
  (when (coleslaw--header-detected)
    (coleslaw--re-search-whole (concatenate 'string
                                            field
                                            ":"
                                            "[\t ]*\\(?1:.*\\)\n")
                               nil t)))

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " CSLAW"
  (use-local-map coleslaw-mode-map)
  (auto-insert)
  (coleslaw--dispatch))

(provide 'coleslaw)

;;; coleslaw.el ends here
