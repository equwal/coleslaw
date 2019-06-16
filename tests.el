;;; tests.el --- Tests for coleslaw-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/equwal/coleslaw/
;; Homepage: https://spensertruex.com/coleslaw-mode
;; This file is not part of GNU Emacs.
;; This file is free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.
;;; Commentary:
;; Tests to run for coleslaw-mode. Main code in coleslaw.el

(require 'coleslaw)
(defvar coleslaw--tests nil "Test functions for coleslaw. Return T or failed.")

(defmacro coleslaw-add-test (name &rest body)
  `(add-to-list 'coleslaw--tests
                '(,name (lambda () ,@body))
                t
                (lambda (x y) (and (listp x)
                                   (listp y)
                                   (eql (car x) (car y))))))

(coleslaw-add-test coleslaw--true t)
(coleslaw-add-test coleslaw--false nil)


;;;###autoload
(defun coleslaw-tests ()
  (let ((res (cl-loop for test in coleslaw--tests
                      collect (let ((success (funcall (cadr test))))
                                (if (not (eql success t))
                                    (format "Test %s failed with %S."
                                            (symbol-name (car test))
                                            success)
                                  success)))))
    (remove-if (lambda (x) (eql t x)) res)
    ;; (if (car res) res t)
    ))
(provide 'coleslaw-tests)
;;; tests.el ends here
