(setq package-archives
      '(
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ;; ("org"          . "https://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("melpa"        . "https://melpa.org/packages/")
        ))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :family "Ricty")
(set-fontset-font nil 'ascii (font-spec :family "Ricty" :size 15))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty" :size 15))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(use-package mozc
  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8) ; エンコーディングの優先順位の一番をutf-8にする
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
  (setq default-input-method "japanese-mozc")
  :custom
  (mozc-candidate-style 'echo-area))

;; (use-package mozc-popup
;;   :custom
;;   (mozc-candidate-style 'popup))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-^" . vertico-directory-up)))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-c f" .  project-find-file)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")
 )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package magit
  :bind (("C-c m" . magit-status)
         ("C-c b" . magit-blame-addition))
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-diff-section-arguments '("--no-ext-diff"))
  (magit-revision-headers-format
   "Author:     %aN <%aE>\nAuthorDate: %ai\nCommit:     %cN <%cE>\nCommitDate: %ci\n")
  (magit-stage-all-confirm nil)
  (magit-unstage-all-confirm nil))

(use-package git-gutter+
  :custom
  (git-gutter+-diff-options '("-b"))
  (git-gutter+-lighter "")
  (global-git-gutter+-mode t))

(use-package recentf
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 200)
  (recentf-exclude '("bookmarks$" "-autoloads\\.el$")))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-Ahl")
  (dired-recursive-copies 'always))

(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-r" . ivy-resume))
  :custom
  (org-agenda-files '("~/org/memo.org" "~/org/todo.org"))
  (org-capture-templates
   '(("t" "Todo" entry (file "~/org/todo.org") "* TODO %?")
     ("d" "Tel" entry (file "~/org/tel.org") "* %T\n%?")))
  (org-log-done 'time)
  (org-todo-keyword-faces
   '(("TODO" . "red")
     ("SKIP" . "light sky blue")
     ("FIXED" . "lime green")))
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "FIXED(f!)" "DONE"))))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package rg
  :init
  (rg-enable-default-bindings "\M-s")
  :custom
  (rg-custom-type-aliases '(("ctp" . "*.ctp") ("vue" . "*.js *.ts *.vue")))
  (ripgrep-arguments '("-s")))

(use-package undo-tree
 :init
 (global-undo-tree-mode))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

(use-package migemo
  :custom
  (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package expand-region
  :bind (("C-@" . er/expand-region)
         ("C-M-@" . er/contract-region)))

(use-package smartparens
  :config
  (require 'smartparens-config)
  ;; (sp-with-modes 'web-mode
  ;;   (sp-local-pair "{{" "}}"
  ;;                  :post-handlers '((:add " | ")))
  ;;   (sp-local-pair "{!!" "!!}"
  ;;                  :post-handlers '((:add " | ")))
  ;;   (sp-local-pair "@if" "@endif"
  ;;                  :post-handlers '(("(|)\n\n[i]" "SPC")))
  ;;   (sp-local-pair "@foreach" "@endforeach"
  ;;                  :post-handlers '(("(|)\n\n[i]" "SPC")))
  ;;   (sp-local-pair "<!--{" "}-->"
  ;;                  :post-handlers '((:add " | "))))
  ;; (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  ;; (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
  :custom
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package php-mode
  :mode "\\.php?\\'"
  :bind (:map php-mode-map
              ("C-c t" . phpunit-current-class)
              ("C-c w" . web-mode))
  :custom
  (php-mode-lineup-cascaded-calls t)
  (php-search-url "http://www.php.net/ja/")
  (php-mode-coding-style 'psr2))

(use-package web-mode
  :mode ("\\.tpl\\'" "\\.html?\\'" "\\.blade\\.php?\\'" "\\.erb\\'" "\\.twig\\'" "\\.vue\\'" "\\.aspx\\'")
  :custom
  ;; (web-mode-enable-auto-pairing nil)    
  (web-mode-code-indent-offset 4)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 4)
  (web-mode-enable-auto-pairing nil))

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package typescript-mode)

(use-package markdown-mode)

(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(global-set-key (kbd "M-.") 'lsp-bridge-find-def)
(global-set-key (kbd "M-,") 'lsp-bridge-find-def-return)
(global-set-key (kbd "M-?") 'lsp-bridge-find-references)

;; key-mapping
(global-set-key (kbd "<f12>") 'remember)
(global-set-key (kbd "C-z") 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(acm-enable-tabnine nil)
 '(ange-ftp-try-passive-mode t)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(compilation-scroll-output 'first-error)
 '(diff-switches "-u")
 '(echo-keystrokes 0.1)
 '(enable-recursive-minibuffers t)
 '(global-hl-line-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(network-security-level 'high)
 '(org-agenda-files '("~/org/todo.org"))
 '(package-selected-packages
   '(smartparens zenburn-theme yasnippet web-mode vertico undo-tree typescript-mode rg php-mode orderless mozc markdown-mode marginalia magit git-gutter+ expand-region embark-consult editorconfig dockerfile-mode docker-compose-mode))
 '(scroll-bar-mode nil)
 '(shift-select-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(undo-tree-auto-save-history nil)
 '(use-package-always-defer t)
 '(use-package-enable-imenu-support t)
 '(vc-display-status nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
