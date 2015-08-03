;shut up
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
;Disable Toolbar
(tool-bar-mode -1)

;package management
(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
  '("org" . "http://orgmode.org/elpa/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))
(package-initialize)

;exec-path
(add-to-list 'exec-path "~/bin")

;load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;multiple-cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;completion
(when (not (package-installed-p 'company))
  (package-refresh-contents)
  (package-install 'company))
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;fix path
(let ((path (shell-command-to-string ". ~/.zshrc.local; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; case insensitive sorting
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; line and column numbers
(global-linum-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

;cua-mode, gives ctrl-(c/v/x)
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;store backup files in different location
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;remote (and sudo) editing
(setq tramp-default-method "ssh")

;whitespace
(setq whitespace-style '(face trailing lines-tail tabs))
(global-whitespace-mode 1)

(when (not (package-installed-p 'helm))
  (package-refresh-contents)
  (package-install 'helm))
(helm-mode t)

;fullscreen mode
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;smooth scrolling
(when (not (package-installed-p 'smooth-scrolling))
  (package-refresh-contents)
  (package-install 'smooth-scrolling))
(require 'smooth-scrolling)

;flycheck
(when (not (package-installed-p 'flycheck))
  (package-refresh-contents)
  (package-install 'flycheck))
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;haskell
(when (not (package-installed-p 'haskell-mode))
  (package-refresh-contents)
  (package-install 'haskell-mode))
(when (not (package-installed-p 'flycheck-haskell))
  (package-refresh-contents)
  (package-install 'flycheck-haskell))

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(flycheck-define-checker haskell-stack
  "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
  :command ("stack" "ghc" "--" "-Wall" "-fno-code"
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`literate-haskell-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode literate-haskell-mode)
  :next-checkers ((warning . haskell-hlint)))

(defun haskell-mode-stack-hook ()
  (interactive)
  (progn
    (flycheck-select-checker 'haskell-stack)))
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(haskell-ghc)))
(add-hook 'haskell-mode-hook 'haskell-mode-stack-hook)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq haskell-indentation-layout-offset 4)
(setq haskell-indentation-left-offset 4)

;ProofGeneral
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

;auctex
(when (not (package-installed-p 'auctex))
  (package-refresh-contents)
  (package-install 'auctex))
(add-hook 'LaTeX-mode-hook (lambda () (TeX-global-PDF-mode t)))
(add-hook 'LaTeX-mode-hook 'enable-evince-sync)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook (lambda () (whitespace-mode 0)))
(add-hook 'LaTeX-mode-hook 'predictive-mode)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)
;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;llvm
(setq load-path
    (cons (expand-file-name "/home/tvh/.emacs.d/llvm") load-path))
  (require 'llvm-mode)
  (require 'tablegen-mode)

;markdown
(when (not (package-installed-p 'markdown-mode))
  (package-refresh-contents)
  (package-install 'markdown-mode))
(require 'markdown-mode)
(when (not (package-installed-p 'pandoc-mode))
  (package-refresh-contents)
  (package-install 'pandoc-mode))
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;rust
(add-to-list 'load-path "/home/tvh/.emacs.d/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;scala
(when (not (package-installed-p 'ensime))
  (package-refresh-contents)
  (package-install 'ensime))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
