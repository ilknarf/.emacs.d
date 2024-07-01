;; disable package.el for alpaca
(setq package-enable-at-startup nil)

;; try to prevent white box when emacs is starting up
(setq inhibit-x-resources t)
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default inhibit-redisplay nil)
	    (redisplay))
	  100)
