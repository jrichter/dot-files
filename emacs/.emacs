(message "%s I'm booting up." (getenv "USER"))

;; Add .emacs.d to load path
(add-to-list 'load-path "~/.emacs.d/my-stuff")

;; My Settings
(require 'my-settings)

;; My Keymaps
(require 'my-keymaps)

(message "All done %s. I'm ready to work." (getenv "USER"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#268bd2" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-comment-face ((t (:foreground "#268bd2" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "#cb4b16" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#b58900" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-string-face ((t (:foreground "#dc322f" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "#2aa198" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(paren-face ((t (:background "#586e75" :foreground "#fdf6e3"))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#d33682")))))
