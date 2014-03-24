;;; package --- Summary
;;; Commentary:
;;; init.el --- Where all the magic begins
;;; Code:

(require 'saveplace)
;;(require 'ffap)
(require 'uniquify)
;;(require 'ansi-color)
(require 'recentf)
;; パッケージ設定
(setq package-archives
      '(
        ;;("gnu"         . "http://elpa.gnu.org/packages/")
        ;;("original"    . "http://tromey.com/elpa/")
        ;; ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/")

(dolist (package '(magit))
  (unless (package-installed-p package)
    (package-install package)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1)
  (when (require 'mwheel nil 'no-error) (mouse-wheel-mode t)))

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      ;; color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ;; ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file "~/places")

;; diff色設定
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))


(recentf-mode 1)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1))

(menu-bar-mode -1)

(setq auto-save-default nil)
(setq make-backup-files nil)

;; フォントと背景色
(set-face-attribute 'default nil
                    :family "Ricty" ;; font
                    :height 130)    ;; font size
;; (set-background-color "#98bc98")
;; (set-foreground-color "black")
(load-theme 'zenburn t)
(global-hl-line-mode) ; 現在行をハイライト
(add-to-list 'default-frame-alist '(alpha . 85)) ;透明度
;; 日本語
(set-language-environment "Japanese")
;;(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8) ; エンコーディングの優先順位の一番をutf-8にする
(setq default-file-name-coding-system 'sjis)
;; 拡張子関連付け
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php?\\'" . web-mode))
(setq web-mode-disable-auto-pairing t)
;;(setq web-mode-enable-current-element-highlight t)
;; ido
(require 'ido)
(ido-mode t)
(require 'ido-hacks)
(ido-hacks-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; flycheck
;;(global-flycheck-mode)
;;(delete 'php-phpcs flycheck-checkers)
;; migemo設定
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(setq migemo-dictionary "c:/emacs-24.3/bin/dict/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(setq migemo-accept-process-output-timeout-msec 80)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
(load-library "migemo")
(migemo-init)
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(add-to-list 'ac-sources 'ac-source-gtags)
;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
;; highlights matching pairs
(show-smartparens-global-mode t)
;; keybinding management
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;; moccur-edit
;; (require 'moccur-edit)
;; undo-tree
(global-undo-tree-mode)
;; magit
(global-set-key (kbd "C-c m") 'magit-status)
;; git-gutter
(global-git-gutter+-mode)
;; helm
(require 'helm-config)
(require 'helm-utils)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-.") 'helm-gtags-find-tag)
(global-set-key (kbd "M-C-.") 'helm-gtags-find-rtag)
(global-set-key (kbd "M-C-:") 'helm-gtags-find-symbol)
(global-set-key (kbd "M-*") 'helm-gtags-pop-stack)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-z") 'helm-mini)
(global-set-key (kbd "C-c f") 'helm-git-files)
(global-set-key (kbd "C-c g") 'helm-ag)
(global-set-key (kbd "M-r") 'helm-resume)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c s") 'helm-google-suggest)
;;(global-set-key (kbd "M-x") 'helm-M-x)
;; org-mode
(setq org-log-done 'time)
(setq org-todo-keyword-faces '(("TODO" . "red") ("SKIP" . "light sky blue")))
(setq org-todo-keywords '((sequence "TODO" "SKIP" "WIP" "DONE")))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
;; yasnippet
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
;; recentf
(setq recentf-max-saved-items 200)
(setq recentf-exclude '(".recentf" ".ido.last" ".gitconfig" ".smex-items" ".todo-ido"))
;; ace-jump-mode
(setq ace-jump-mode-scope 'window)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; auto-save-buffers
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers) 
;; key-mapping
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f6>") 'toggle-alpha)
;; 透明度を切りかえる関数
(defun toggle-alpha ()
  "透明度を切り替える."
  (interactive)
  (if (= (frame-parameter nil 'alpha) 85)
      (set-frame-parameter nil 'alpha 0)
    (set-frame-parameter nil 'alpha 85)))

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)

;; other
(setq comment-style 'extra-line)
(setq kill-whole-line t)

(add-hook 'php-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (local-set-key (kbd "C-.") 'other-window)
            ;;(flycheck-mode)
            ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case nil)
 '(anzu-minimum-input-length 2)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(anzu-use-migemo t)
 '(diff-switches "-u")
 '(global-anzu-mode t)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(php-search-url "http://www.php.net/ja/")
 '(tab-width 4)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
