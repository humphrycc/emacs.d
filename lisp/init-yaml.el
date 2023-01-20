(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '(".clang-format" . yaml-mode))
  )

(provide 'init-yaml)
