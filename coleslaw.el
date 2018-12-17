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
;; coleslaw-insert-header (bound to M-;) inserts the comment block
;; depending on the type of file, and is how the major mode is selected for
;; the file.  Meant to work for all formats supported by kingcons's
;; "coleslaw" static content generator.  Default enabled in .page and .post
;; files. Automagically generates headers when opening new empty coleslaw files.

;;; Code:

(defvar coleslaw-mode-hook nil)
(require 'autoinsert)
(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

(defun coleslaw--format ()
  "Read in FORMAT from the user and return it."
  (interactive)
  ;; Note: skeleton-read doesn't return it's input so we need this
  (read-from-minibuffer "Format: " ))

;;;###autoload
(defun coleslaw-skeleton-insert ()
  "Insert the skeleton for this type of file with FORMAT filled in."
                                        ;  (beginning-of-buffer)
  (let ((format (coleslaw--format)))
    (skeleton-insert '(nil ";;;;;\ntitle: "
                           (skeleton-read "title: ")
                           "\nformat: "
                           format
                           (if (coleslaw--bufftype ".page")
                               "\nurl: "
                             "")
                           (if (coleslaw--bufftype ".page")
                               (skeleton-read "url: ")
                             "")
                           "\ndate: "
                           (skeleton-read "date: ")
                           "\n;;;;;\n"))
    format))

(defun coleslaw-insert-header ()
  "Spawn a skeleton as specified by default for a coleslaw file type.
Automatically changes the mode.  FORMAT is filled into the
skeleton and used to select the mode"
  (let ((format (coleslaw-skeleton-insert)))
    (cond ((string-equal format "md")
           (markdown-live-preview-mode))
          ((string-equal format "cl-who")
           (lisp-mode))
          ((string-equal format "html")
           (html-mode))
          ((string-equal format "rst")
           (markdown-mode))))
  (forward-line)
  (move-end-of-line 1))

(defvar coleslaw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-;") 'coleslaw-insert-header)
    map)
  "Keymap for COLESLAW major mode.")

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " KRAUT"
  (add-hook 'coleslaw-mode-hook #'flyspell-mode)
  (run-hooks 'coleslaw-mode-hook)
  (use-local-map coleslaw-mode-map)
  (setq minor-mode #'coleslaw-mode))

(add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))

(add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))

(setf auto-insert t)

(cl-pushnew (cons ".page" 'coleslaw-insert-header) auto-insert-alist)

(cl-pushnew (cons ".post" 'coleslaw-insert-header) auto-insert-alist)

(add-hook 'coleslaw-mode-hook 'auto-insert)

;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.
(require 'markdown-mode)

(autoload 'markdown-preview-eww "view markdown in w3m web browser.")

(provide 'coleslaw)

;;; coleslaw.el ends here
