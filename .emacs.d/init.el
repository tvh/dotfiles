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
    company tide ensime scala-mode yaml-mode rust-mode racer)
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

;; Fix !!!STUPID!!! default Keybindings
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

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
(add-hook 'haskell-mode-hook
  (lambda ()
    (setq whitespace-line-column 100)
    ))
(add-hook 'haskell-mode-hook 'intero-mode)

;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; JS
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; Rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (0xc yaml-mode tide molokai-theme intero exec-path-from-shell evil-org ensime benchmark-init web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
