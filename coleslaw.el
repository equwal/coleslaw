;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: equwal 2018-12-09
;; Version: 0.1.2 [2019-06-09]
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
;; Please add (coleslaw-setup) to your init file.

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;;; Code:

(defvar coleslaw-mode-hook nil)

(defvar coleslaw-mode-map
  (make-sparse-keymap)
  "Keymap for COLESLAW major mode.")

(defvar coleslaw--mode-doc
  "Set %s according to VAL.  Positive set it on, zero or negative turn it off."
  "The doc for modesetting functions.")
(defmacro coleslaw--gen-mode (name global docstring)
  `(defun ,name (val)
     (format coleslaw--mode-doc docstring)
     (setq ,global (> val 0))))

(defvar coleslaw--auto-insert nil
  "Whether to auto insert coleslaw header in new files.")
(coleslaw--gen-mode coleslaw-auto-insert
                    coleslaw--auto-insert
                    "auto insertion of headers")

(defvar coleslaw--markdown-mode nil
  "Whether to enable markdown in coleslaw format: md files.")
(coleslaw--gen-mode coleslaw-markdown-mode
                    coleslaw--markdown-mode
                    "automatic markdown mode")

(defvar coleslaw--markdown-live nil
  "Whether to enable live markdown preview in coleslaw format: md files.")
(coleslaw--gen-mode coleslaw-markdown-live
                    coleslaw--markdown-live
                    "automatic markdown live preview")

(defvar coleslaw--html-mode nil
  "Whether to enable live markdown preview in coleslaw format: html files.")
(coleslaw--gen-mode coleslaw-html-mode
                    coleslaw--html-mode
                    "automatic markdown live preview")

(defvar coleslaw--lisp-mode nil
  "Whether to enable Lisp mode markdown preview in coleslaw format: cl-who files.")
(coleslaw--gen-mode coleslaw-lisp-mode
                    coleslaw--lisp-mode
                    "automatic Lisp mode selection")

(defvar coleslaw--rst-mode nil
  "Whether to enable ReSTructured text mode in coleslaw format: rst files.")
(coleslaw--gen-mode coleslaw-rst-mode
                    coleslaw--rst-mode
                    "automatic rst mode selection")

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
  (coleslaw-lisp-mode 1)
  (coleslaw-html-mode 1)
  (coleslaw-rst-mode 1)
  (dolist (type '(".page" ".post"))
    (add-to-list (cons type 'coleslaw-insert-header) auto-insert-alist))
  (add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))
  (add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))
  (define-key coleslaw-mode-map (kbd "M-;") 'coleslaw-insert-header)
  (define-key coleslaw-mode-map (kbd "C-;") 'coleslaw-mode-dispatch))

(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (cl-subseq buffer-file-name (- (length buffer-file-name) 5))))

(defun coleslaw--mode-spawn (format)
  "Select the mode for a file of type FORMAT."
  (cond ((string-equal format "md")
         (when coleslaw--markdown-mode (markdown-mode))
         (when coleslaw--markdown-live (markdown-live-preview-mode)))
        ((string-equal format "cl-who")
         (when coleslaw--lisp-mode (lisp-mode)))
        ((string-equal format "html")
         (when coleslaw--html-mode (html-mode)))
        ((string-equal format "rst")
         (when coleslaw---rst-mode (rst-mode)))))

;;;###autoload
(defun coleslaw-mode-dispatch ()
  "Set modes based on this buffer's 'format: (md, cl-who, etc.)'
  metadata line."
  (coleslaw--mode-spawn ()))

;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw
file type."
  (skeleton-insert '(nil ";;;;;\ntitle: "
                         (skeleton-read "title: ")
                         "\nformat: "
                         (seleton-read format)
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
                         "\n;;;;;") 0 format)
  (move-end-of-line 0)
  format)
;; (coleslaw--never-nil ((a 1) (b 2)) b) ;=> 2
;; (coleslaw--never-nil ((a 1) (b nil) (c 3)) c) ;=> NIL

(defmacro coleslaw--let-true (let-binds &rest body)
  "Bind the LET-BINDS, but ensure that nothing is NIL or
therefore return NIL without evaluating the BODY code."
  (let ((vars (mapcar #'car let-binds)))
    (let ((vals (mapcar #'cadr let-binds)))
      (when (every #'identity vals)
        `(let ,(loop for r in vars
                     for l in vals
                     collect (list r l))
           ,@body)))))

;; =>
(coleslaw--let-true ((a t))
                    t)
(coleslaw--let-true ((a t)
                     (b t)))
;; =>
(coleslaw--let-true ((a 1)))
(coleslaw--let-true ((b 1)))
(coleslaw--let-true nil b)
;; =>
(coleslaw--let-true ((a 1))
                    (coleslaw--let-true ((b 1))
                                        (coleslaw--let-true nil b)))

(coleslaw--let-true* ((a t))
                     t)
(coleslaw--let-true* ((a t)
                      (b t)) b)
;; =>
(coleslaw--let-true* ((a 1)))
(coleslaw--let-true* ((b 1)))
(coleslaw--let-true* nil b)


(coleslaw--let-true* ((a t))
                     t)

(defmacro coleslaw--let-true* (binds &rest body)
  ((mapcar (lambda (b) `(coleslaw--let-true (,b)))
           binds))
  (append ',acc b)
  ;; ,@(append (car bs) (mapcar #'list (cdr bs)))
  )

(defun coleslaw--split-buffer (query start end)
  (coleslaw--let-true ((f (buffer-search? (car wrap-pairs) (1- (cdr general)))))
                      (coleslaw--let-true ((s (buffer-searvh? (1- (cdr general)) t)))
                                          (cons f s))))

(defun coleslaw--split-string (base query start end)
  (coleslaw--let-true ((f (search query (subseq base start end))))
                      (coleslaw--let-true ((s (search f (substring base f))))
                                          (subseq string f s))))

(defun coleslaw--split (string &rest wrap-pairs)
  "Split the string between the WRAP-PAIRS repeatedly. If any fail,
  NIL. Send NIL string to assume the current buffer."
  (if (null wrap-pairs)
      string
    (if string
        (if (coleslaw--split-string string (car wrap-pairs))))))

;;;###autoload
(define-minor-mode coleslaw-mode "Edit coleslaw static content gloriously."
  :lighter " CSLAW"
  (use-local-map coleslaw-mode-map)
  (auto-insert)
  (let ((header ;; Is the ;;;;;....;;;;; header available in buffer?
         ))
    (when header
      (coleslaw--mode-spawn ;; split at "format: " "\n"
       )
      )))

;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.

(autoload 'markdown-preview-eww "view markdown in w3m web browser.")

(provide 'coleslaw)

;;; coleslaw.el ends here
