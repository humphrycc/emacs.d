;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

(use-package editorconfig
  :ensure t)

(use-package copilot
  :load-path (lambda () (expand-file-name "third_party/copilot.el" user-emacs-directory))
  ;; don't show in mode line
  :diminish)

(provide 'init-copilot)
