(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Color Theme
(package-install 'molokai-theme)
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
(package-install 'evil)
(evil-mode t)

;; Install Intero (Haskell)
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
