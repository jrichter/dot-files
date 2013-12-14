;; Global Keymaps
(define-key evil-normal-state-map ",s" 'split-window-vertically)
(define-key evil-normal-state-map ",v" 'split-window-horizontally)
(define-key evil-normal-state-map ",h" 'windmove-left)
(define-key evil-normal-state-map ",j" 'windmove-down)
(define-key evil-normal-state-map ",k" 'windmove-up)
(define-key evil-normal-state-map ",l" 'windmove-right)
(define-key evil-normal-state-map ",." 'next-buffer)
(define-key evil-normal-state-map ",/" 'previous-buffer)
(define-key evil-normal-state-map ",r" 'helm-mini)
(define-key evil-normal-state-map ",p" 'projectile-find-file)

;; Esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'my-keymaps)
