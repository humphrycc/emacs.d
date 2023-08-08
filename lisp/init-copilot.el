;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

(use-package editorconfig
  :ensure t)

(use-package copilot
  :load-path (lambda () (expand-file-name "third_party/copilot.el" user-emacs-directory))
  ;; don't show in mode line
  :diminish)

(add-hook 'prog-mode-hook 'copilot-mode)

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

;; (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

;; (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
;; (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
;; (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
;; (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
;; (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)
(define-key copilot-mode-map (kbd "ESC <down>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "ESC <up>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-n") #'copilot-accept-completion-by-word)
(define-key global-map (kbd "M-RET") #'rk/copilot-complete-or-accept)

(defun rk/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (company-yasnippet-or-completion)
      (indent-for-tab-command)))

(define-key global-map (kbd "<tab>") #'rk/copilot-tab)

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error nil)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)

(provide 'init-copilot)
