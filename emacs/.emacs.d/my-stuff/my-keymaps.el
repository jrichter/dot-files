;; Global Keymaps
;; Normal Mode Keymaps
(define-key evil-normal-state-map ",s" 'split-window-vertically)
(define-key evil-normal-state-map ",v" 'split-window-horizontally)
(define-key evil-normal-state-map ",h" 'windmove-left)
(define-key evil-normal-state-map ",j" 'windmove-down)
(define-key evil-normal-state-map ",k" 'windmove-up)
(define-key evil-normal-state-map ",l" 'windmove-right)

(define-key evil-normal-state-map ",." 'next-buffer)
(define-key evil-normal-state-map ",/" 'previous-buffer)
(define-key evil-normal-state-map ",b" 'helm-buffers-list)

(define-key evil-normal-state-map ",r" 'helm-mini)
(define-key evil-normal-state-map ",P" 'projectile-find-file)
(define-key evil-normal-state-map ",p" 'sr-speedbar-toggle)

(define-key evil-normal-state-map ",q" 'delete-window)
(define-key evil-normal-state-map ",o" 'delete-other-windows)
(define-key evil-normal-state-map ",,o" 'winner-undo)
(define-key evil-normal-state-map ",,O" 'winner-redo)

(define-key evil-normal-state-map ", " 'ace-jump-mode)

;; Visual Mode keymaps
(define-key evil-visual-state-map "." 'eval-region)

;; Esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

;; sr-speedbar maps
(add-hook 'speedbar-mode-hook '(lambda ()
                                 (define-key speedbar-mode-map ",p" 'sr-speedbar-toggle)
                                 (define-key speedbar-mode-map ",h" 'windmove-left)
                                 (define-key speedbar-mode-map ",j" 'windmove-down)
                                 (define-key speedbar-mode-map ",k" 'windmove-up)
                                 (define-key speedbar-mode-map ",l" 'windmove-right)
                                 ))

(provide 'my-keymaps)
