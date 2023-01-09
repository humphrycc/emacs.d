(use-package rust-mode
  :init
  )

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)

(provide 'init-rust)
