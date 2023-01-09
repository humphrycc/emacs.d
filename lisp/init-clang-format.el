(use-package clang-format
  :init
  (progn
    (setq clang-format-fallback-style "google")

    ;; key bindings
    (with-eval-after-load 'clang-format
      (define-key c-mode-map    [(f12)] 'clang-format-buffer)
      (define-key c++-mode-map  [(f12)] 'clang-format-buffer))))

;; (defun clang-format-save-hook-for-this-buffer ()
;;   "Create a buffer local save hook."
;;   (add-hook 'before-save-hook
;;             (lambda ()
;;               (progn
;;                 (when (locate-dominating-file "." ".clang-format")
;;                   (clang-format-buffer))
;;                 ;; Continue to save.
;;                 nil))
;;             nil
;;             ;; Buffer local hook.
;;             t))

;; (add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
;; (add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(provide 'init-clang-format)
