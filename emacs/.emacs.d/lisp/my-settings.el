;; Set my defaults
(setq user-full-name "Justin Richter")
(setq user-login-name "justin")
(setq user-mail-address "jrichter@jetfive.com")

;; ERC
(setq erc-nick "jrichter") ;; ERC login
(setq erc-hide-list '("JOIN" "PART" "QUIT")) ;; Ignore these messages
(setq erc-autojoin-channels-alist '(("freenode.net" "#church.io"))) ;; strings seperated by spaces

;; (erc-scrolltobottom-mode t)

;; Allow emacs to translate colors to the best terminal match
(color-theme-approximate-on)

;; Helm Mode
(helm-mode 1)

(set 'helm-ack-use-ack-grep t)

;; Projectile Mode
(require 'projectile)
(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-enable-caching t)

;; flx-ido flex matching
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Load evil-mode
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-mode 1)
(global-evil-surround-mode 1)
(setq-default evil-cross-lines t)

;; ;; Powerline
;; (require 'powerline)
;; (powerline-evil-vim-color-theme)
(display-time-mode t)

;; Diminish keeps things clean
(require 'diminish)
(eval-after-load "Helm" '(diminish 'helm-mode))

;; helm settings (TAB in helm window for actions over selected items,
;; C-SPC to select items)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-projectile)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)

;; (after 'projectile
;;        (package 'helm-projectile))
(global-set-key (kbd "M-x") 'helm-M-x)

(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; Load ace-jump-mode
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; Load company
(add-hook 'after-init-hook 'global-company-mode)

;; Ruby stuff
;; (add-hook 'ruby-mode-hook 'flycheck-mode)
;; (add-hook 'ruby-mode-hook 'rubocop-mode)

(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Ruby
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'enh-ruby-mode)
(flycheck-define-checker ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool."
  :command ("rubocop" "--format" "emacs"
            (config-file "--config" flycheck-rubocoprc)
            source)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
            line-end)
   (error line-start
          (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
          line-end))
   :modes (enh-ruby-mode))


(require 'rvm)
(rvm-use-default)
(add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

;; ;; Show line at 90 char
;; (require 'fill-column-indicator)
;; (setq-default fill-column 90)
;; (setq-default fci-rule-width 1)
;; (setq-default fci-rule-color "#859900")
;; (add-hook 'ruby-mode-hook 'fci-mode)
;; (add-hook 'clojure-mode-hook 'fci-mode)
;; (add-hook 'markdown-mode-hook 'fci-mode)

;; Tune emacs garbage collector
(setq gc-cons-threshold 20000000)

;; Ask for confirmation before quitting Emacs
;; (add-hook 'kill-emacs-query-functions
          ;; (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          ;; 'append)

;; Remember last postion in files
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;; Don't put backups in current directory
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq backup-by-copying t) ;; set how emacs backs up
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t) ;; let it be versioned!
(setq auto-save-file-name-transforms
      `((".*" ,"~/.saves" t)))

; Undo/Redo
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.saves/emacs-undo")))

;; Change the newline-mark 'paragraph mark' to the paragraph symbol
(setq whitespace-display-mappings '((newline-mark 10 [182 10])))

;; Make C-n add newlines so I don't have to hit enter at the end of a
;; buffer
(setq next-line-add-newlines t)

;; Turn on dired-x
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;; Hide welcome screen
(setq inhibit-splash-screen t)

;; Hide Menubar and Toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; disable those annoying tooltips
(tooltip-mode -1)

;; default font
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Consolas"))

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
;; (global-linum-mode t)
;; (nlinum-mode t)

;; Turn on winner mode
(winner-mode t)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; show matching parens
(show-paren-mode 1)

;; theme those matching parens
(require 'paren)
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#fdf6e3")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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

;; majority of code formatting conventions do not recommend mixed tabs and spaces. So, here.
(setq-default indent-tabs-mode nil)     ; emacs 23.1 default is t
(setq tab-width 4)   ; width for display tabs. emacs 23.1 default is 8

;; make buffer names unique when files of the same name of diff dir are opened
(require 'uniquify) ; bundled with GNU emacs 23.2.1 or before
(setq uniquify-buffer-name-style 'forward)

;; nice scrolling
(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use zenburn as the default theme
(load-theme 'zenburn t)

;; show env var named path
(getenv "PATH")
;; example of setting env var named “path”
;; by prepending new paths to existing paths
(setenv "PATH"
        (concat
         "/usr/local/nmh/bin" ":"
         (getenv "PATH") ; inherited from OS
         )
        )
;; enable mouse support
(xterm-mouse-mode)

;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
;;;  find-tag that automagically reruns etags when it cant find a
;;;  requested item and then makes a new try to locate it.
;;;  Fri Mar 15 09:52:14 2002
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;; simpleclip-mode
(require 'simpleclip)
(simpleclip-mode 1)

;; Enable final newline
(setq require-final-newline t)

;; which-key
(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Org Mode
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;; Setup some templates
(setq org-capture-templates
      (quote (("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
               "* TODO %?\n  %i\n %t\n  %a")
              ("n" "note" entry (file (concat org-directory "/notes.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (concat org-directory "/notes.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              )))
;; custom timestamps
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%b %e, %Y>" . "<%b %e, %Y %H:%M>"))

(provide 'my-settings)
