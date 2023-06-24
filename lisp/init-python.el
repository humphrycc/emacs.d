(use-package python-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  ;; (add-hook 'go-mode-hook 'lsp-deferred)
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
)

(provide 'init-python)
