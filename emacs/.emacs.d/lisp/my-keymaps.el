;; non-evil keymaps
(global-set-key (kbd "C-c =") 'er/expand-region)
(global-set-key (kbd "C-c /") 'ack-and-a-half)
(global-set-key (kbd "C-c .") 'helm-ack)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c q") 'delete-frame)

(require 'bible-verse)
(global-set-key (kbd "C-c b") 'bible-verse)

;; obliterate unwanted emacs default key bindings.
(define-key evil-normal-state-map (kbd "g /") nil)
(define-key evil-normal-state-map (kbd "g w") nil)
(define-key evil-normal-state-map (kbd "RET") nil)
(define-key evil-visual-state-map (kbd "RET") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "C-l"))

(evil-leader/set-key
  "v" 'evil-window-vsplit
  "s" 'evil-window-split
  "q" 'delete-window
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "H" 'evil-window-move-far-left
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "L" 'evil-window-move-far-right

  "." 'next-buffer
  "/" 'previous-buffer
  "b" 'helm-mini
  "<SPC>" 'helm-show-kill-ring

  "cc" 'org-capture
  "cl" 'org-store-link
  "ca" 'org-agenda

  "r" 'helm-recentf
  "F" 'helm-find-files
  "f" 'helm-projectile
  "p" 'helm-projectile-switch-project
  "u" 'undo-tree-visualize

  "gs" 'magit-status

  "," 'evil-ace-jump-char-mode

  "Q" 'kill-buffer
  "o" 'winner-undo
  "O" 'winner-redo)

;; Normal Mode keymaps
;;(define-key evil-normal-state-map " " 'some-package)

;; Magit
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-diff-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-define-key 'normal magit-mode-map
  "s" 'magit-stage-item
  "S" 'magit-stage-all
  "p" 'magit-push
  "c" 'magit-commit
  "d" 'magit-diff
  "D" 'magit-diff-working-tree
  "l" 'magit-log-mode
  "u" 'magit-unstage-item
  "U" 'magit-unstage-all
  "r" 'magit-refresh-all
  "q" 'magit-mode-quit-window
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-process-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-commit-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-log-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-diff-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section)

;; Packages
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-define-key 'normal package-menu-mode-map
  "i" 'package-menu-mark-install
  "d" 'package-menu-mark-delete
  "u" 'package-menu-mark-unmark
  "U" 'package-menu-mark-upgrades
  "x" 'package-menu-execute)

;; Visual Mode keymaps
(define-key evil-visual-state-map "." 'eval-region)
(define-key evil-visual-state-map "c" 'comment-or-uncomment-region)

;; Esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(provide 'my-keymaps)
