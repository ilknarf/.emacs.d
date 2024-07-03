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

;;; xref (updated version for project.el)
(use-package xref :ensure t)

;;; project.el
(use-package project :ensure t
  :after xref
  :config
  (setq Buffer-menu-show-internal t))

;;; gruvbox theme
(use-package gruvbox-theme :ensure t
  :config
  (load-theme 'gruvbox-dark-hard))

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

;;; lsp-mode for compatible langs

(use-package lsp-mode :ensure t)

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
(use-package go-mode :ensure t
  :after lsp-mode
  :hook (go-mode . lsp))

;;; rust
(use-package rust-mode :ensure t
  :after lsp-mode
  :hook (rust-mode . lsp))

;;; emacs settings
(use-package emacs :ensure nil
  :config
  ;; rudimentary setup options
  (setq ring-bell-function #'ignore)
  (setq enable-recursive-minibuffers t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  ;; always start fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; set custom var file so this isn't polluted
  (setq custom-file "~/.emacs.d/custom.el")
  ;; load it if not already loaded
  (unless (fboundp 'custom-file-loaded)
    (load custom-file)
    (setq custom--file-loaded t)))

;;; Local Variables:
;;  outline-regexp: ";;; "
;;  eval: (outline-minor-mode t)
;;  End:
