(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook 'gofmt-before-save)
)

(provide 'init-typescript)
