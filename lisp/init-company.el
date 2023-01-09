(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))

(define-key c-mode-map    "\M-/" 'company-complete)
(define-key c++-mode-map  "\M-/" 'company-complete)
(define-key rust-mode-map  "\M-/" 'company-complete)

(provide 'init-company)
