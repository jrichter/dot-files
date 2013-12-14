;;; Load undo-tree
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode)

;; Load evil-mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Load ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/ace-jump-mode")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (defvar my-packages '(flx-ido
                        helm
                        projectile
                        sr-speedbar
                        color-theme-sanityinc-solarized
                        fill-column-indicator
                        auto-complete
                        js2-mode
                        js2-refactor
                        lua-mode
                        flymake-python-pyflakes
                        flymake-ruby
                        flymake-lua
                        flymake-haml
                        flymake-json
                        flymake-css
                        flymake-sass
                        flymake-jslint
                        robe
                        ))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  )

;; Helm Mode
(helm-mode 1)

;; Projectile Mode
(projectile-global-mode)

;; sr-speedbar stuff
(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-update-flag t
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; Auto complete mode
(require 'auto-complete)
(add-to-list 'ac-modes 'ruby-mode 'javascript-mode)
(setq ac-sources '(ac-source-semantic ac-source-yasnippet))
(global-auto-complete-mode)

;; Ruby - robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-robe)
            (setq completion-at-point-functions '(auto-complete))))

;; load some js2-mode defaults from magnars
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'html-mode '(require 'setup-html-mode))

;; flymake ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; flymake lua
(require 'flymake-lua)
(add-hook 'ruby-mode-hook 'flymake-lua-load)

;; Show line at 90 char
(require 'fill-column-indicator)
(setq-default fill-column 90)
(setq-default fci-rule-width 1)
(setq-default fci-rule-color "#859900")
(add-hook 'ruby-mode-hook 'fci-mode)
(add-hook 'js-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'markdown-mode-hook 'fci-mode)

;; Turn on flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Tune emacs garbage collector
(setq gc-cons-threshold 20000000)

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;; Don't put backups in current directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t) ;; set how emacs backs up
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t) ;; let it be versioned!

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

;; Load a custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; Hide welcome screen
(setq inhibit-splash-screen t)

;; Hide Menubar and Toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; default font Hermit for Powerline
(set-face-attribute 'default nil :font "Hermit for Powerline")

;; sort colors in list-colors-display by hue
(setq list-colors-sort 'hsv )

;; Set initial layout
(if (string= system-name "justin-Studio-1537")
    (setq default-frame-alist
          '((width . 151) (height . 90)))
  (setq default-frame-alist
        '((width . 101) (height . 90))))

(fringe-mode (cons 8 4))

;; turn on line numbers
(global-linum-mode t)

;; disable those annoying tooltips
(tooltip-mode -1)

;; Turn on winner mode
(winner-mode t)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Rainbow delimiters
(add-to-list 'load-path "~/.emacs.d/rainbow")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; theme those matching parens
(require 'paren)
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#fdf6e3")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; show matching parens
(show-paren-mode 1)

;; After yank, indent region
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode sh-mode js-mode js2-mode
                           c-mode c++-mode objc-mode ruby-mode slim-mode lua-mode clojure-mode
                           LaTeX-mode TeX-mode html-mode scss-mode css-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; When splitting a buffer move point to new buffer
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; Xah Lee elisp mode
(add-to-list 'load-path "~/.emacs.d/xah-elisp-mode")
(require 'xah-elisp-mode)
(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

(setq enable-recursive-minibuffers t )

;; majority of code formatting conventions do no recommend mixed tabs and spaces. So, here.
(setq-default indent-tabs-mode nil)     ; emacs 23.1 default is t
(setq tab-width 4)   ; width for display tabs. emacs 23.1 default is 8

;; make buffer names unique when files of the same name of diff dir are opened
(require 'uniquify) ; bundled with GNU emacs 23.2.1 or before
(setq uniquify-buffer-name-style 'forward)

;; Personal Stuff
(setq user-full-name "Justin Richter")
(setq user-login-name "justin")
(setq user-mail-address "jrichter@jetfive.com")

;; My Keymaps
(require 'my-keymaps)

(provide 'my-settings)
