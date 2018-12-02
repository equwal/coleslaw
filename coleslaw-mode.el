(defun group (ps n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))
(defun defkeys (map &rest (key-fn-ps))
  (dolist (p (group key-fn-ps 2) (funcall #'define-key (car p) (cadr p))) ))
(defvar coleslaw-mode-hook nil)
(defvar coleslaw-mode-map
  (let ((map (make-keymap)))
    (defkeys map
      "H-c" 'markdown-preview-cleanup)
    map)
  "Keymap for COLESLAW major mode")
(define-derived-mode coleslaw-mode fundamental-mode "COLESLAW"
  "Major mode for editing coleslaw site generation files."
  (interactive)
  (kill-all-local-variables)
  ;; (set-syntax-table coleslaw-mode-syntax-table)
  (setq-local comment-start ";;;;;
")
  (setq-local comment-end "
;;;;;")
  (use-local-map coleslaw-mode-map)
  (setq major-mode 'coleslaw-mode)
  (setq mode-name "COLESLAW")
  (run-hooks 'coleslaw-mode-hook))
(add-to-list 'auto-mode-alist '("\\.page\\'" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))
(provide 'coleslaw-mode)
;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.
(require 'markdown-preview-eww)
(require 'markdown-mode)
(require 'w3m)
(setq browse-url-browser-function 'w3m-goto-url)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-preview-eww))
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-preview-eww))
