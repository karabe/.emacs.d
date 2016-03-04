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
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ;;("original"    . "http://tromey.com/elpa/")
        ;;("org"         . "http://orgmode.org/elpa/")
        ;; ("marmalade"   . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(dolist (package '(magit smex zenburn-theme web-mode ido-hacks
                      flycheck s undo-tree git-gutter+
                      helm yasnippet editorconfig helm-gtags projectile
                      phpunit toggle-test expand-region php-mode js2-mode
                      helm-projectile volatile-highlights move-text
                      comment-dwim-2 company company-web))
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
      xterm-mouse-mode t
      save-place-file "~/places")

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
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Ricty" :height 130)
    (set-face-attribute 'default nil :family "Ricty")
    )
;; (set-background-color "#98bc98")
;; (set-foreground-color "black")
(load-theme 'hc-zenburn t)
(global-hl-line-mode) ; 現在行をハイライト
(add-to-list 'default-frame-alist '(alpha . 85)) ;透明度
;; 日本語
(set-language-environment "Japanese")
;;(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8) ; エンコーディングの優先順位の一番をutf-8にする

;; windowsのときのみsjis設定
(if (eq system-type 'windows-nt)
    (setq default-file-name-coding-system 'sjis)
  nil
  )

;; 拡張子関連付け
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; smartparens
;; (require 'smartparens-config)
;; (smartparens-global-mode t)
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
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
;; web-modeとsmartparensを両方使う場合
;; (defun my-web-mode-hook () 
;;   (setq web-mode-enable-auto-pairing nil))
;; (add-hook 'web-mode-hook 'my-web-mode-hook)
;; (defun sp-web-mode-is-code-context (id action context)
;;   (when (and (eq action 'insert)
;;              (not (or (get-text-property (point) 'part-side)
;;                       (get-text-property (point) 'block-side))))
;;     t))
;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
;; ido
(require 'ido)
(ido-mode t)
(require 'ido-hacks)
(ido-hacks-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'web-mode)
;; (add-to-list 'ac-sources 'ac-source-yasnippet)
;; (add-to-list 'ac-sources 'ac-source-gtags)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-web-html)

;; company-statistics
(require 'company-statistics)
(company-statistics-mode)

;; projectile
(projectile-global-mode)

;; highlights matching pairs
;; (show-smartparens-global-mode t)
;; keybinding management
;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
;; (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
;; (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
;; (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
;; (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
;; (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
;; (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
;; (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
;; (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
;; (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
;; (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;; (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
;; (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
;; (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;; (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
;; (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
;; (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
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
(require 'helm-gtags)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-.") 'helm-gtags-find-tag)
(global-set-key (kbd "M-C-.") 'helm-gtags-find-rtag)
(global-set-key (kbd "M-C-:") 'helm-gtags-find-symbol)
(global-set-key (kbd "M-*") 'helm-gtags-pop-stack)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-z") 'helm-mini)
(global-set-key (kbd "C-c f") 'helm-projectile)
(global-set-key (kbd "C-c g") 'helm-ag)
(global-set-key (kbd "M-r") 'helm-resume)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
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

;; ace-jump-mode
(setq ace-jump-mode-scope 'window)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
;; move-text
(require 'move-text)
(move-text-default-bindings)
;; toggle-test
(add-to-list 'tgt-projects '(
                             (:root-dir "/home/user/Files/new-king/app")
                             (:src-dirs "./")
                             (:test-dirs "tests")
                             (:test-suffixes "Test")
                             ))
(add-to-list 'tgt-projects '(
                             (:root-dir "/home/user/Files/itec-system/app")
                             (:src-dirs "./")
                             (:test-dirs "tests")
                             (:test-suffixes "Test")
                             ))
;; auto-save-buffers
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers) 
;; key-mapping
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f6>") 'toggle-alpha)
(global-set-key (kbd "C-c t") 'phpunit-current-class)
(global-set-key (kbd "C-c s") 'tgt-toggle)
(global-set-key (kbd "<f12>") 'remember)

;; 透明度を切りかえる関数
(defun toggle-alpha ()
  "透明度を切り替える."
  (interactive)
  (if (= (frame-parameter nil 'alpha) 85)
      (set-frame-parameter nil 'alpha 0)
    (set-frame-parameter nil 'alpha 85)))

;; インデントをタブかスペースか切り替える関数
(defun toggle-indent-tab ()
  "インデントをタブかスペースか切り替える"
  (interactive)
  (if indent-tabs-mode
      (progn
        (setq indent-tabs-mode nil))
      (progn 
        (setq indent-tabs-mode t)))
  )

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; other
(setq comment-style 'extra-line)
(setq kill-whole-line t)

(add-hook 'php-mode-hook
    (lambda ()
        (local-set-key (kbd "C-.") 'other-window)
        (helm-gtags-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case nil)
 '(ange-ftp-try-passive-mode t)
 '(anzu-minimum-input-length 2)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(company-idle-delay 0)
 '(diff-switches "-u")
 '(editorconfig-mode t)
 '(electric-pair-mode t)
 '(git-commit-fill-column 80)
 '(git-gutter+-lighter "")
 '(global-anzu-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(global-git-gutter+-mode t)
 '(helm-google-suggest-search-url "http://www.google.co.jp/search?ie=utf-8&oe=utf-8&q=")
 '(helm-google-suggest-url "http://google.com/complete/search?output=toolbar&q=")
 '(helm-gtags-auto-update t)
 '(js2-strict-missing-semi-warning nil)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(magit-revision-headers-format
   "Author:     %aN <%aE>
AuthorDate: %ai
Commit:     %cN <%cE>
CommitDate: %ci
")
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(package-selected-packages
   (quote
    (company-statistics ac-php company-web company japanese-holidays zenburn-theme yasnippet web-mode volatile-highlights undo-tree toggle-test smex pt phpunit php-refactor-mode php-mode move-text markdown-mode magit less-css-mode js2-mode ido-hacks helm-pt helm-projectile helm-gtags hc-zenburn-theme gitignore-mode gitconfig-mode gitattributes-mode git-gutter+ flycheck-tip expand-region editorconfig comment-dwim-2 color-theme coffee-mode anzu)))
 '(php-lineup-cascaded-calls t)
 '(php-search-url "http://www.php.net/ja/")
 '(recentf-exclude
   (quote
    (".recentf" ".ido.last" ".gitconfig" ".smex-items" ".todo-do" ".history" "COMMIT_EDITMSG" "autoloads.el")))
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(show-paren-mode t)
 '(sp-autoescape-string-quote nil)
 '(tab-width 4)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(vc-display-status nil)
 '(web-mode-code-indent-offset 4)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
