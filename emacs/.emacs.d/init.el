(require 'cl)

(message "%s I'm booting up." (getenv "USER"))

;; Always load newest byte code
(setq load-prefer-newer t)

;; set custom directories
(defvar my-emacs-dir (file-name-directory load-file-name)
  "The root directory.")
(defvar lisp-dir (expand-file-name "lisp" my-emacs-dir)
  "My lisp files.")
(defvar custom-dir (expand-file-name "custom" my-emacs-dir)
  "Location of custom.el")

(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path custom-dir)

;; Load packages not installed
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(ace-jump-mode
                      ack-and-a-half
                      color-theme-approximate
                      company
                      company-inf-ruby
                      diminish
                      elixir-mode
                      expand-region
                      evil
                      evil-leader
                      evil-matchit
                      evil-nerd-commenter
                      evil-numbers
                      evil-surround
                      fill-column-indicator
                      flycheck
                      flymake-ruby
                      flymake-lua
                      flymake-haml
                      flymake-json
                      flymake-css
                      flymake-sass
                      flx-ido
                      haml-mode
                      helm
                      helm-ack
                      helm-projectile
                      inf-ruby
                      markdown-mode
                      powerline-evil
                      ruby-mode
                      ruby-tools
                      rubocop
                      ;; rvm install from git instead
                      smartparens
                      ;; smooth-scrolling
                      zenburn-theme
                      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; My Settings
(require 'my-settings)

;; My Keymaps
(require 'my-keymaps)

;; Custom.el
(setq custom-file (expand-file-name "custom.el" custom-dir))

(message "All done %s. I'm ready to work." (getenv "USER"))
