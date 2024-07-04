;;; elpaca install
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; packages

;;; exec-path-from-shell to make sure eshell matches .bashrc
(use-package exec-path-from-shell :ensure t
  :demand t
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;;; xref (updated version for project.el)
(use-package xref :ensure t)

;;; project.el
(use-package project :ensure t
  :after xref
  :config
  (setq Buffer-menu-show-internal t))

;;; ivy, swiper, counsel
(use-package ivy :ensure t
  :demand
  :bind (("C-c C-r" . ivy-resume)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-push-view))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel :ensure t
  :demand
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper :ensure t
  :after ivy
  :bind ("C-s" . swiper-isearch))

;;; company for autocompletion
(use-package company :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (company-mode))

;;; flycheck for on-the-fly checking
(use-package flycheck :ensure t
  :config
  (global-flycheck-mode))

;;; lsp-mode for compatible langs

(use-package lsp-mode :ensure t
  :hook (;; rust hooks
	 (rust-mode . lsp)
	 ;; go hooks
	 (go-mode . (lambda() (progn
		      (add-hook 'before-save-hook #'lsp-format-buffer t t)
		      (add-hook 'before-save-hook #'lsp-organize-imports t t))))
	 (go-mode . lsp-deferred)))

(use-package lsp-ui :ensure t)

;; ivy interface for lsp-mode
(use-package lsp-ivy :ensure t)

;;; adaptive wrap
(use-package adaptive-wrap :ensure
  :hook (org-mode . adaptive-wrap-prefix-mode)
  :config
  (setq adaptive-wrap-extra-indent 2))

;;; org mode
(use-package org :ensure t
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))
  (setq org-startup-indented t)
  (setq org-agenda-todo-list-sublevels t))

;;; org-superstar
(use-package org-superstar :ensure t
  :after org
  :hook (org-mode . org-superstar-mode))

;;; seq (required update for transient)
(use-package seq :ensure t)

;;; transient (required update for magit)
(use-package transient :ensure t
  :after seq)

;;; magit
(use-package magit :ensure t
  :after transient)

;;; go
(use-package go-mode :ensure t)

;;; rust
(use-package rust-mode :ensure t)

;;; emacs settings
(use-package emacs :ensure nil
  :config
  ;; rudimentary setup options
  (setq ring-bell-function #'ignore)
  (setq enable-recursive-minibuffers t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  ;; autocomplete brackets
  (electric-pair-mode 1)
  ;; delete whitepace on save
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)
  ;; always start fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; set custom var file so this isn't polluted
  (defconst custom-file-path "~/.emacs.d/custom.el")
  (setq custom-file custom-file-path)
  ;; load it if not already loaded
  (unless (or (fboundp 'custom-file-loaded) (not (file-exists-p custom-file-path)))
    (load custom-file)
    (setq custom--file-loaded t))
  ;; load theme
  (load-theme 'modus-vivendi)
  ;; add default font
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-12")))


;;; Local Variables:
;;  outline-regexp: ";;; "
;;  eval: (outline-minor-mode t)
;;  byte-compile-warnings: (not free-vars)
;;  End:
