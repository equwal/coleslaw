;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Spenser Truex
;; Author: Spenser Truex <spensertruexonline@example.com>
;; Created: Equwal 2018-12-09
;; Version:  1.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: lisp wp files convenience
;; URL: https://github.com/equwal/coleslaw/
;; Homepage: http://truex.eu
;; This file is not part of GNU Emacs.
;; This file is free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Please add (coleslaw-setup) to your init file.

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;;; Code:

(defvar coleslaw-mode-hook nil)

(defvar coleslaw-mode-map
  (make-sparse-keymap)
  "Keymap for COLESLAW major mode.")

(defvar coleslaw--auto-insert nil
  "Whether to auto insert coleslaw header in new files.")
(defvar coleslaw--markdown-mode nil
  "Whether to enable markdown in coleslaw format: md files.")
(defvar coleslaw--markdown-live nil
  "Whether to enable live markdown preview in coleslaw format: md files.")
(defvar coleslaw--html-mode nil
  "Whether to enable live markdown preview in coleslaw format: html files.")
(defvar coleslaw--lisp-mode nil
  "Whether to enable Lisp mode markdown preview in coleslaw format: cl-who files.")
(defvar coleslaw--rst-mode nil
  "Whether to enable ReSTructured text mode in coleslaw format: rst files.")

(defun coleslaw-auto-insert (val)
  "Set auto insertion of headers according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--auto-insert (> val 0)))
(defun coleslaw-markdown-mode (val)
  "Set automatic markdown mode according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--markdown-mode (> val 0)))
(defun coleslaw-markdown-live (val)
  "Set automatic markdown live preview according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--markdown-live (> val 0)))
(defun coleslaw-html-mode (val)
  "Set automatic markdown live preview according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--html-mode (> val 0)))
(defun coleslaw-lisp-mode (val)
  "Set automatic Lisp mode selection according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--lisp-mode (> val 0)))
(defun coleslaw-rst-mode (val)
  "Set automatic rst mode selection according to VAL.  Positive set it on, zero or negative turn it off."
  (setq coleslaw--rst-mode (> val 0)))

(defun coleslaw-setup ()
  "Setup your coleslaw like the Author suggests (conservative edits only).
Strongly recommended!  Set M-; to `coleslaw-insert-header', enable
auto insertion for .page and .post files, enable markdown mode
and live preview."
  (require 'markdown-mode)
  (require 'autoinsert)
  (require 'lisp-mode)
  (require 'sgml-mode)
  (require 'rst)
  (coleslaw-auto-insert 1)
  (coleslaw-markdown-mode 1)
  (coleslaw-markdown-live 1)
  (coleslaw-lisp-mode 1)
  (coleslaw-html-mode 1)
  (coleslaw-rst-mode 1)
  (dolist (type '(".page" ".post"))
    (cl-pushnew (cons type 'coleslaw-insert-header) auto-insert-alist))
  (add-hook 'coleslaw-mode-hook #'flyspell-mode)
  (add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))
  (add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))
  (define-key coleslaw-mode-map (kbd "M-;") 'coleslaw-insert-header))

(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

(defun coleslaw--format ()
  "Read in FORMAT from the user and return it."
  (interactive)
  ;; Note: skeleton-read doesn't return it's input so we need this
  (read-from-minibuffer "Format: " ))

(defun coleslaw--skeleton-insert ()
  "Insert the skeleton for this type of file with FORMAT filled in."
                                        ;  (beginning-of-buffer)
  (let ((format (coleslaw--format)))
    (skeleton-insert '(nil ";;;;;\ntitle: "
                           (skeleton-read "title: ")
                           "\nformat: "
                           str
                           (if (coleslaw--bufftype ".page")
                               "\nurl: "
                             "")
                           (if (coleslaw--bufftype ".page")
                               (skeleton-read "url: ")
                             "")
                           "\ndate: "
                           (skeleton-read "date: ")
                           "\n;;;;;\n") 0 format)
    format))
;;;###autoload
(defun coleslaw-insert-header ()
  "Spawn a skeleton as specified by default for a coleslaw file type.
Automatically changes the mode.  FORMAT is filled into the
skeleton and used to select the mode"
  (let ((format (coleslaw--skeleton-insert)))
    (cond ((string-equal format "md")
           (when coleslaw--markdown-mode (markdown-mode))
           (when coleslaw--markdown-live (markdown-live-preview-mode)))
          ((string-equal format "cl-who")
           (when coleslaw--lisp-mode (lisp-mode)))
          ((string-equal format "html")
           (when coleslaw--html-mode (html-mode)))
          ((string-equal format "rst")
           (when coleslaw---rst-mode (rst-mode))))))

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " KRAUT"
  (use-local-map coleslaw-mode-map)
  (auto-insert))

;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.

(autoload 'markdown-preview-eww "view markdown in w3m web browser.")

(provide 'coleslaw)

;;; coleslaw.el ends here
