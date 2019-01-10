;;; package --- Summary
;;; Commentary:
;;; init.el --- Where all the magic begins
;;; Code:

(require 'uniquify)
;;(require 'ansi-color)
(require 'recentf)
;; パッケージ設定
(setq package-archives
      '(
        ("gnu"         . "https://elpa.gnu.org/packages/")
        ("org"         . "https://orgmode.org/elpa/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa-latest"       . "https://melpa.org/packages/")
	))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(dolist (package '(magit zenburn-theme web-mode mozc mozc-popup
                   flycheck s undo-tree git-gutter+ anzu smart-mode-line
                   counsel yasnippet editorconfig projectile
                   phpunit expand-region php-mode js2-mode rg
                   counsel-projectile move-text volatile-highlights
                   comment-dwim-2 company company-statistics
                   lsp-mode company-lsp apache-mode gitignore-mode ivy-historian
                   japanese-holidays org add-node-modules-path dockerfile-mode
                   ivy-xref
                   ))
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
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      xterm-mouse-mode t
      save-place-file "~/places")

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1))

(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq auto-save-default nil)
(setq make-backup-files nil)

;; フォントと背景色
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Ricty" :height 130)
    (set-face-attribute 'default nil :family "Ricty")
    )
;; (set-background-color "#98bc98")
;; (set-foreground-color "black")

(set-face-attribute 'default nil :family "Ricty")
(set-fontset-font nil 'ascii (font-spec :family "Ricty"))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))

(load-theme 'zenburn t)
(global-hl-line-mode) ; 現在行をハイライト
(add-to-list 'default-frame-alist '(alpha . 85)) ;透明度
;; 日本語
(set-language-environment "Japanese")
;;(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8) ; エンコーディングの優先順位の一番をutf-8にする
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(require 'mozc-popup)
(setq mozc-candidate-style 'popup)

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
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
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
;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'web-mode)
;; (add-to-list 'ac-sources 'ac-source-yasnippet)
;; (add-to-list 'ac-sources 'ac-source-gtags)

;; company-mode
(require 'company)
(add-hook 'php-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'css-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'company-mode)

;; company-statistics
(require 'company-statistics)
(company-statistics-mode)

;; projectile
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
(global-set-key (kbd "C-c b") 'magit-blame-addition)
;; git-gutter
(global-git-gutter+-mode)
;; counsel
(ivy-mode 1)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c y") 'counsel-yank-pop)
(global-set-key (kbd "C-z") 'ivy-switch-buffer)
;; counsel-projectile
(global-set-key (kbd "C-c f") 'counsel-projectile-find-file)
;; lsp-mode
(require 'lsp-clients)
(add-hook 'php-mode-hook #'lsp)
;; ivy-xref
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
;; yasnippet
(yas-global-mode 1)
;; rg
(rg-enable-default-bindings "\M-s")
;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; move-text
(require 'move-text)
(move-text-default-bindings)
;; auto-save-buffers
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers) 
;; key-mapping
(global-set-key (kbd "<f12>") 'remember)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)
(global-set-key (kbd "M-;") 'comment-dwim-2)
;; smart-mode-line
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; eglot
(defvar eglot-server-programs '((rust-mode . (eglot-rls "rls"))
                                (python-mode . ("pyls"))
                                ((js-mode
                                  js2-mode
                                  rjsx-mode) . ("javascript-typescript-stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                ((c++-mode
                                  c-mode) . (eglot-cquery "cquery"))
                                (ruby-mode
                                 . ("solargraph" "socket" "--port"
                                    :autoport))
                                (php-mode . ("php" "/home/lubuntu/.config/composer/vendor/bin/php-language-server.php"))
                                (haskell-mode . ("hie-wrapper"))))
;; japanese-holiday
(eval-after-load "holidays"
  '(progn
     (require 'japanese-holidays)
     (setq calendar-holidays (append japanese-holidays))
     (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
     (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
   )
  )
;; add-node-modules-path
(add-hook 'js2-mode-hook #'add-node-modules-path)
;; other
(setq comment-style 'extra-line)
(setq kill-whole-line t)

(add-hook 'php-mode-hook
  (lambda ()
    (setq-local company-backends '((company-capf company-files company-yasnippet :with company-dabbrev-code)))
    (local-set-key (kbd "C-.") 'other-window)
    (local-set-key (kbd "C-c t") 'phpunit-current-class)
    (local-set-key (kbd "C-c C-r") 'ivy-resume)
    (local-set-key (kbd "C-c w") 'web-mode)))

(add-hook 'web-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-r") 'ivy-resume)
   (setq-local company-backends '(company-css company-dabbrev-code))
   (setq-local electric-pair-inhibit-predicate
               `(lambda (c)
                  (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c))))))

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
 '(calendar-mark-holidays-flag t)
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-statistics-mode t)
 '(company-transformers
   (quote
    (company-sort-by-statistics company-sort-by-backend-importance)))
 '(counsel-gtags-auto-update t)
 '(counsel-yank-pop-separator
   "
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
")
 '(default-input-method "japanese-mozc")
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(dired-listing-switches "-Ahl")
 '(dired-recursive-copies (quote always))
 '(dired-use-ls-dired t)
 '(editorconfig-mode t)
 '(electric-pair-inhibit-predicate (quote ignore))
 '(electric-pair-mode t)
 '(electric-pair-pairs (quote ((39 . 39) (34 . 34))))
 '(electric-pair-text-pairs (quote ((39 . 39) (34 . 34))))
 '(enable-recursive-minibuffers t)
 '(git-commit-fill-column 80)
 '(git-gutter+-diff-options (quote ("-b")))
 '(git-gutter+-lighter "")
 '(global-anzu-mode t)
 '(global-flycheck-mode t)
 '(global-git-gutter+-mode t)
 '(imenu-auto-rescan t)
 '(ivy-historian-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(japanese-holiday-weekend-marker (quote (holiday nil nil nil nil nil holiday)))
 '(js2-strict-missing-semi-warning nil)
 '(lsp-auto-configure nil)
 '(lsp-prefer-flymake nil)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(magit-revision-headers-format
   "Author:     %aN <%aE>
AuthorDate: %ai
Commit:     %cN <%cE>
CommitDate: %ci
")
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(network-security-level (quote high))
 '(org-agenda-files (quote ("~/org/memo.org" "~/org/todo.org")))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file "~/org/todo.org")
      "* TODO %?")
     ("d" "Tel" entry
      (file "~/org/tel.org")
      "* %T
%?"))))
 '(org-log-done (quote time))
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "red")
     ("SKIP" . "light sky blue")
     ("FIXED" . "lime green"))))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "WIP(w)" "FIXED(f!)" "DONE"))))
 '(package-selected-packages
   (quote
    (eglot dockerfile-mode magit add-node-modules-path yaml-mode ivy-historian lsp-mode mozc-popup mozc volatile-highlights diminish smart-mode-line counsel counsel-gtags counsel-projectile wgrep-ag rg php-mode dotenv-mode apache-mode csv-mode rainbow-mode yasnippet-snippets org apib-mode elixir-mode pug-mode kotlin-mode flycheck yasnippet editorconfig zenburn-theme web-mode undo-tree sudo-edit pt phpunit move-text less-css-mode js2-mode japanese-holidays hc-zenburn-theme gitignore-mode gitconfig-mode gitattributes-mode git-gutter+ flycheck-tip expand-region company-web company-statistics comment-dwim-2 color-theme coffee-mode anzu)))
 '(php-lineup-cascaded-calls t)
 '(php-search-url "http://www.php.net/ja/")
 '(projectile-completion-system (quote ivy))
 '(recentf-exclude
   (quote
    (".recentf" ".ido.last" ".gitconfig" ".smex-items" ".todo-do" ".history" "COMMIT_EDITMSG" "autoloads.el")))
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(rg-custom-type-aliases (quote (("ctp" . "*.ctp"))))
 '(rg-show-columns t)
 '(ripgrep-arguments (quote ("-s")))
 '(rm-blacklist
   (quote
    (" EditorConfig" " ARev" " CounselGtags" " Undo-Tree" " VHl" " yas" " ivy" " Abbrev")))
 '(show-paren-mode t)
 '(sp-autoescape-string-quote nil)
 '(tab-width 4)
 '(truncate-partial-width-windows nil)
 '(vc-display-status nil)
 '(volatile-highlights-mode t)
 '(web-mode-code-indent-offset 4)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
