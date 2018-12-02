(defvar coleslaw-mode-hook nil)
(defvar coleslaw-mode-map
  (let ((map (make-sparse-keymap))) ;could use make-keymap if it had much in it.
    (define-key map (kbd "H-c") 'markdown-preview-cleanup)
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
(require 'markdown-preview-mode)
(require 'markdown-mode)
(require 'w3m)
(setq browse-url-browser-function 'w3m-goto-url)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-preview-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-preview-mode))
