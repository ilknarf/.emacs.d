;;; elpaca install
(defvar is-windows (eq system-type 'windows-nt))
(defvar is-linux (eq system-type 'gnu/linux))
(defvar is-mac (eq system-type 'ms-dos))

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
  :demand t
  :bind (("C-c C-r" . ivy-resume)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-push-view))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel :ensure t
  :demand t
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper :ensure t
  :after ivy
  :bind ("C-s" . swiper-isearch))

;;; company for autocompletion
(use-package company :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

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
	 (go-mode . lsp-deferred)
	 ;; c/cpp hooks
	 (c-mode . lsp)
	 (c++-mode . lsp))
  :config
  (setq
   lsp-clangd-binary-path "/usr/bin/clangd"
   lsp-idle-delay 0.2
   ))

(use-package lsp-ui :ensure t)

;;; ivy interface for lsp-mode
(use-package lsp-ivy :ensure t)

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

;;; adaptive wrap
(use-package adaptive-wrap :ensure
  :hook (org-mode . adaptive-wrap-prefix-mode)
  :config
  (setq adaptive-wrap-extra-indent 2))

;;; org-superstar
(use-package org-superstar :ensure t
  :after org
  :hook (org-mode . org-superstar-mode))

;;; org roam
(use-package org-roam :ensure t
  ;; FIXME done so the bindings aren't autoloaded, there's definitely a better way to do this
  :demand t
  :custom
  (org-roam-directory (file-truename "~/zk"))
  ;; copied from https://github.com/org-roam/org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
    (setq org-roam-capture-templates
      '(
        ("p" "permanent" plain "%?"
         :target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n") :unnarrowed t)
	;; my blog posts
        ("a" "article" plain "%?"
         :target (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n") :unnarrowed t)
	;; my book references
	("b" "book" plain "%?"
         :target (file+head "books/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n") :unnarrowed t)
	;; topic nodes for general topics
	("t" "topic" plain "%?"
         :target (file+head "topics/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n") :unnarrowed t)))
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template
	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    ;; from https://jethrokuan.github.io/org-roam-guide/
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; exclude dailies and file/links from org roam graph
  (setq org-roam-graph-link-hidden-types '("file"                      ; dont render links to files
					 "http"                        ; - http links
					 "https"                       ; - https links
					 "fuzzy"))                     ; - links to image files, tables etc
  (org-roam-db-autosync-enable))

;;; seq (required update for transient)
;; code copied from https://github.com/progfolio/elpaca/issues/216 to remove seq warning
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))

;;; transient (required update for magit)
(use-package transient :ensure t
  :after seq)

;;; magit
(use-package magit :ensure t
  :after transient)

;;; go
(use-package go-mode :ensure t)

;;; lua
(use-package lua-mode :ensure t)

;;; rust
(use-package rust-mode :ensure t)

;;; c
(use-package clang-format :ensure t)

;;; yaml
(use-package yaml-mode :ensure t)

;;; graphql
(use-package graphql-mode :ensure t)

;;; which-key
(use-package which-key :ensure t
  :config
  (which-key-mode))

(use-package catppuccin-theme :ensure t
  :config
  (load-theme 'catppuccin :no-confirm))

;; info (for info path)
(use-package info :ensure nil
  :config
  (add-to-list 'Info-default-directory-list "~/.local/share/info"))


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
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
  ;; add default font
  (add-to-list 'default-frame-alist
	       '(font . "IosevkaTermSlab Nerd Font Mono 11")))


;;; Local Variables:
;;  outline-regexp: ";;; "
;;  eval: (outline-minor-mode t)
;;  byte-compile-warnings: (not free-vars)
;;  End:
