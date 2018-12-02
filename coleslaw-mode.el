(defvar coleslaw-mode-hook nil)
(defvar coleslaw-mode-map
  (let ((map (make-sparse-keymap))) ;could use make-keymap if it had much in it.
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
