(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-message t)
(require 'cl)

(defvar prelude-packages
  '(molokai-theme evil evil-org intero)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (cl-every 'package-installed-p prelude-packages))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
            (package-install p))))

;; Color Theme
(load-theme 'molokai t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
			      (interactive)
			      (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
			      (interactive)
			      (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; Vim Keybindings
(evil-mode t)

;; Install Intero (Haskell)
(add-hook 'haskell-mode-hook 'intero-mode)
