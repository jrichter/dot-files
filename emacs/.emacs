(message "%s I'm booting up." (getenv "USER"))

;; Load undo-tree
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode)

;; Load evil-mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

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

;; default font Hermit for Powerline
(set-face-attribute 'default nil :font "Hermit for Powerline")

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

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Clean up buffers before save
(add-hook 'before-save-hook 'whitespace-cleanup)

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

;; Personal Stuff
(setq user-full-name "Justin Richter")
(setq user-login-name "justin")
(setq user-mail-address "jrichter@jetfive.com")

(message "All done %s. I'm ready to work." (getenv "USER"))
