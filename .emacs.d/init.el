(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(require 'cl)

(defvar prelude-packages
  '(molokai-theme evil evil-org intero exec-path-from-shell benchmark-init
    company)
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

;; Init Benchmark
(benchmark-init/activate)

;; Fix PATH
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; Color Theme
(load-theme 'molokai t)

;; line and column numbers
(global-linum-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

;remote (and sudo) editing
(setq tramp-default-method "ssh")

;whitespace
(setq whitespace-style '(face trailing lines-tail tabs))
(global-whitespace-mode 1)

;; Vim Keybindings
(evil-mode t)

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

;; Autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Haskell
(add-hook 'haskell-mode-hook (lambda () (setq whitespace-line-column 120)))
(add-hook 'haskell-mode-hook 'intero-mode)
