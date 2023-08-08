;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)

(require 'package)
;; (add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '( "melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
;; http://xahlee.info/emacs/emacs/emacs_set_backup_into_a_directory.html
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(global-display-line-numbers-mode)
(global-hl-line-mode)
(setq-default tab-width 4)
(electric-pair-mode)
(show-paren-mode 1)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(defun save-frame-to-register (register)
  "Save the current frame layout to a register."
  (interactive "cSave frameset to register: ")
  (frameset-to-register register))
(global-set-key (kbd "C-c r") 'save-frame-to-register)

(global-set-key (kbd "C-c l") 'jump-to-register)

(require 'init-theme)
(require 'init-ace-window)
(require 'init-undo-tree)
(require 'init-helm)
(require 'init-lsp)
(require 'init-yasnippet)

;; Language support
(require 'init-bazel)
(require 'init-c-cpp)
(require 'init-clang-format)
(require 'init-cmake)
(require 'init-rust)
(require 'init-go)
(require 'init-python)
(require 'init-javascript)
(require 'init-protobuf)
(require 'init-yaml)
(require 'init-org)
(require 'init-flycheck)
(require 'init-company)
(require 'init-copilot)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
