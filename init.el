;;; package --- Summary
;;; Commentary:
;;; init.el --- Where all the magic begins
;;; Code:

;; パッケージ設定
(setq package-archives
      '(
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ;; ("org"          . "https://orgmode.org/elpa/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
	))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :family "Ricty")
(set-fontset-font nil 'ascii (font-spec :family "Ricty"))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(use-package mozc
  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8) ; エンコーディングの優先順位の一番をutf-8にする
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
  (setq default-input-method "japanese-mozc"))

(use-package mozc-popup
  :custom
  (mozc-candidate-style 'popup))

(use-package web-mode
  :mode ("\\.tpl\\'" "\\.html?\\'" "\\.blade\\.php?\\'" "\\.vue\\'")
  :bind (:map web-mode-map
             ("C-c C-r" . ivy-resume))
  :custom
  (web-mode-code-indent-offset 4)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 4))

(use-package php-mode
  :mode "\\.php?\\'"
  :bind (:map php-mode-map
              ("C-." . other-window)
              ("C-c t" . phpunit-current-class)
              ("C-c C-r" . ivy-resume)
              ("C-c w" . web-mode))
  :custom
  (php-mode-lineup-cascaded-calls t)
  (php-search-url "http://www.php.net/ja/"))

(use-package company
  :hook ((php-mode web-mode css-mode js-mode) . company-mode)
  :custom
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package projectile
  :init
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-register-project-type 'laravel
                                    '("composer.json" "app" "bootstrap" "config" "database" "public" "resources" "storage" "tests" "vendor")
                                    :test "phpunit"
                                    :test-suffix "Test"
                                    )
  (projectile-register-project-type 'webextention
                                    '("addon" "src" "test" "package.json")
                                    :compile "yarn run prod"
                                    :run "yarn start"
                                    :src-dir "src"
                                    :test-dir "test"
                                    :test "yarn run jest"
                                    :test-suffix ".test"
                                    )
  :custom
  (projectile-completion-system 'ivy))

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

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

(use-package counsel
  :bind (("C-c s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c i" . counsel-imenu)
         ("C-c y" . counsel-yank-pop)
         ("C-z" . ivy-switch-buffer))
  :custom
  (ivy-mode t)
  (counsel-yank-pop-separator "\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n")
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full))

(use-package ivy-xref
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-historian
  :hook (ivy-mode . ivy-historian-mode))

(use-package counsel-projectile
  :bind ("C-c f" . counsel-projectile-find-file))

(use-package eglot
  :disabled
  :hook (((php-mode js-mode c-mode)
          . (lambda ()
              (eglot-ensure)
              (setq-local company-backends '((company-capf company-files company-yasnippet :with company-dabbrev-code))))))
  :config
  (setq eglot-server-programs '((rust-mode . (eglot-rls "rls"))
                                (python-mode . ("pyls"))
                                ((js-mode
                                  js2-mode
                                  typescript-mode)
                                 . ("typescript-language-server" "--stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                (php-mode . ("php" "/home/lubuntu/.composer/vendor/bin/php-language-server.php"))
                                ((c++-mode c-mode) . ("clangd"))
                                ((caml-mode tuareg-mode reason-mode)
                                 . ("ocaml-language-server" "--stdio"))
                                (ruby-mode
                                 . ("solargraph" "socket" "--port"
                                    :autoport))
                                (haskell-mode . ("hie-wrapper"))
                                (kotlin-mode . ("kotlin-language-server"))
                                (go-mode . ("go-langserver" "-mode=stdio"
                                            "-gocodecompletion"))
                                ((R-mode ess-r-mode) . ("R" "--slave" "-e"
                                                        "languageserver::run()"))
                                (java-mode . eglot--eclipse-jdt-contact)
                                (dart-mode . ("dart_language_server")))))

(use-package lsp-mode
  :disabled
  :hook ((php-mode js-mode c-mode) . (lambda ()
                                        (require 'lsp-clients)
                                        (lsp)))
  :custom
  (lsp-auto-configure nil)
  (lsp-eldoc-render-all t)
  (lsp-enable-completion-at-point nil))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-after-open . lsp-enable-imenu))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-imenu-enable nil))

(use-package company-lsp
  :hook (lsp-mode . (lambda ()
                      (setq-local company-backends '((company-lsp company-files company-yasnippet :with company-dabbrev-code))))))

(use-package flymake-diagnostic-at-point
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package counsel-gtags
  :hook ((php-mode web-mode js-mode) . (lambda ()
                      (setq-local company-backends '((company-gtags company-files company-yasnippet company-css :with company-dabbrev-code)))
                      (counsel-gtags-mode)))
  :bind (:map counsel-gtags-mode-map
              ("M-." . counsel-gtags-dwim)
              ("M-?" . counsel-gtags-find-reference)
              ("M-," . counsel-gtags-go-backward))
  :custom
  (counsel-gtags-auto-update t))

(use-package flycheck
  :hook (counsel-gtags-mode . flycheck-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
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

(use-package yasnippet-snippets)

(use-package rg
  :init
  (rg-enable-default-bindings "\M-s")
  :custom
  (rg-custom-type-aliases '(("ctp" . "*.ctp")))
  (rg-show-columns t)
  (ripgrep-arguments '("-s")))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(use-package move-text
  :init
  (move-text-default-bindings))

(use-package anzu
  :bind ("M-%" . anzu-query-replace)
  :custom
  (anzu-minimum-input-length 2)
  (anzu-mode-lighter "")
  (anzu-search-threshold 1000))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package expand-region
  :bind (("C-@" . er/expand-region)
         ("C-M-@" . er/contract-region)))

;; key-mapping
(global-set-key (kbd "<f12>") 'remember)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package smart-mode-line
  :init
  (sml/setup)
  :custom
  (sml/no-confirm-load-theme t)
  (rm-blacklist
   (format "^ \\(%s\\)$"
           (mapconcat #'identity
                      '("EditorConfig" "ARev" "Undo-Tree" "VHl" "yas" "ivy"
                        "Abbrev" "ElDoc" "company" "Projectile.*" "CounselGtags")
                      "\\|"))))

(use-package japanese-holidays
  :hook ((calendar-today-visible . japanese-holiday-mark-weekend)
         (calendar-today-invisible . japanese-holiday-mark-weekend))
  :config
  (setq calendar-holidays (append japanese-holidays))
  :custom
  (calendar-mark-holidays-flag t)
  (japanese-holiday-weekend-marker '(holiday nil nil nil nil nil holiday)))

(use-package add-node-modules-path
  :hook js-mode)

(use-package emmet-mode
  :hook web-mode)

(use-package editorconfig
  :custom
  (editorconfig-mode t))

(use-package volatile-highlights
  :custom
  (volatile-highlights-mode t))

(use-package apache-mode)

(use-package gitignore-mode)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package phpunit)

(use-package migemo
  :custom
  (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-try-passive-mode t)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(dired-listing-switches "-Ahl")
 '(dired-recursive-copies (quote always))
 '(dired-use-ls-dired t)
 '(echo-keystrokes 0.1)
 '(electric-pair-inhibit-predicate (quote ignore))
 '(electric-pair-mode t)
 '(electric-pair-pairs (quote ((39 . 39) (34 . 34))))
 '(electric-pair-text-pairs (quote ((39 . 39) (34 . 34))))
 '(enable-recursive-minibuffers t)
 '(git-commit-fill-column 80)
 '(global-hl-line-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(max-mini-window-height 3)
 '(menu-bar-mode nil)
 '(network-security-level (quote high))
 '(package-selected-packages
   (quote
    (flycheck counsel-gtags company-lsp lsp-ui magit zenburn-theme yasnippet-snippets web-mode volatile-highlights use-package undo-tree smart-mode-line rg phpunit php-mode mozc-popup move-text migemo markdown-mode japanese-holidays ivy-xref ivy-historian gitignore-mode git-gutter+ flymake-diagnostic-at-point expand-region emmet-mode eglot editorconfig dockerfile-mode docker-compose-mode counsel-projectile company-statistics comment-dwim-2 apache-mode anzu add-node-modules-path)))
 '(recentf-exclude
   (quote
    (".recentf" ".ido.last" ".gitconfig" ".smex-items" ".todo-do" ".history" "COMMIT_EDITMSG" "autoloads.el")))
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
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
