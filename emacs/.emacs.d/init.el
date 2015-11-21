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
(defvar from-git-dir (expand-file-name "from-git" my-emacs-dir)
  "Location of files from git")

(defun add-git-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (add-git-subfolders-to-load-path name)))))

;; add directories to Emacs's `load-path'
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path custom-dir)
(add-git-subfolders-to-load-path from-git-dir)

;; Load packages not installed
(require 'package)
(setq package-check-signature nil)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(ace-jump-mode
                      color-theme-approximate
                      company
                      company-inf-ruby
                      diminish
                      elixir-mode
                      enh-ruby-mode
                      expand-region
                      evil
                      evil-leader
                      evil-matchit
                      evil-nerd-commenter
                      evil-numbers
                      evil-surround
                      fill-column-indicator
                      flycheck
                      flymake-css
                      flymake-haml
                      flymake-json
                      flymake-lua
                      flymake-php
                      flymake-ruby
                      flymake-sass
                      flymake-yaml
                      flx-ido
                      haml-mode
                      helm
                      helm-ack
                      helm-projectile
                      inf-ruby
                      latex-preview-pane
                      magit
                      markdown-mode
                      php-mode
                      php-refactor-mode
                      powerline-evil
                      request
                      ruby-mode
                      ruby-tools
                      rubocop
                      simpleclip
                      smartparens
                      web-mode
                      which-key
                      yaml-mode
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
