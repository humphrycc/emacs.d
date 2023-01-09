(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(provide 'init-undo-tree)
