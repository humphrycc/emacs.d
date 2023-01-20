(use-package protobuf-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
  )

(provide 'init-protobuf)
