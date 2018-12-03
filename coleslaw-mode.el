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
(defmacro defkeys (map &rest (key-fn-ps))
  (let ((a (gensym)))
    `(let ((,a ,map))
       (dolist ((p (group ',key-fn-ps 2) ,a)
		(define-key ,a (kbd ,(car p)) ',(cadr p)))))))

(defvar coleslaw-mode-hook nil)
(defun coleslaw-insert-header ()
  (interactive)
  (insert ";;;;;
title: 
url: 
format: 
date: 
;;;;;"))
(defvar coleslaw-mode-map
  (defkeys (make-sparse-keymap) "M-;" coleslaw-insert-header)
  "Keymap for COLESLAW major mode")
(defun coleslaw-mode ()
  "Mode for editing coleslaw site generation files."
  (interactive)
  (kill-all-local-variables)
  (add-hook 'coleslaw-mode-hook 'flyspell-mode)
  (add-hook 'coleslaw-mode-hook 'markdown-live-preview-mode)
  (add-hook 'coleslaw-mode-hook 'markdown-mode)
  ;(add-hook 'coleslaw-mode-hook 'markdown-mode)
  (setq minor-mode 'coleslaw-mode)
  (setq mode-name "COLESLAW")
  (run-hooks 'coleslaw-mode-hook)
  (use-local-map coleslaw-mode-map))
(add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))
(provide 'coleslaw-mode)
;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.
(setq browse-url-browser-function 'w3m-goto-url)
(autoload 'markdown-mode "markdown-mode"			       
  "Major mode for editing Markdown files")			       
(autoload 'markdown-preview-eww "view markdown in w3m web browser.") 

