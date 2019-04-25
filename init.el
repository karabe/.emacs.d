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

(require 'bind-key)

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

(use-package php-mode
  :mode "\\.php?\\'"
  :bind (:map php-mode-map
              ("C-." . other-window)
              ("C-c t" . phpunit-current-class)
              ("C-c C-r" . ivy-resume)
              ("C-c w" . web-mode))
  :custom
  (php-mode-lineup-cascaded-calls t)
  (php-search-url "http://www.php.net/ja/")
  (php-mode-coding-style 'psr2))

(use-package web-mode
  :mode ("\\.tpl\\'" "\\.html?\\'" "\\.blade\\.php?\\'" "\\.vue\\'" "\\.erb\\'" "\\.twig\\'")
  :bind (:map web-mode-map
             ("C-c C-r" . ivy-resume))
  :custom
  (web-mode-code-indent-offset 4)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 4))

(use-package js
  :bind (:map js-mode-map
              ("M-." . xref-find-definitions)))

(use-package elixir-mode)

(use-package company
  :hook ((web-mode css-mode emacs-lisp-mode sql-mode lsp-mode graphviz-dot-mode)
         . company-mode)
  :custom
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package projectile
  :commands projectile-mode projectile-register-project-type
  :init
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-register-project-type 'laravel
                                    '("composer.json" "app" "bootstrap" "config" "database" "public" "resources" "storage" "tests" "vendor")
                                    :test "phpunit"
                                    :test-suffix "Test")
  (projectile-register-project-type 'webextention
                                    '("addon" "src" "test" "package.json")
                                    :compile "yarn run prod"
                                    :run "yarn start"
                                    :src-dir "src"
                                    :test-dir "test"
                                    :test "yarn run jest"
                                    :test-suffix ".test")
  (projectile-register-project-type 'eccube
                                    '("data" "html"))
  :custom
  (projectile-completion-system 'ivy))

(use-package undo-tree
  :commands global-undo-tree-mode
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
  :commands ivy-xref-show-xrefs
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-historian
  :hook (ivy-mode . ivy-historian-mode))

(use-package counsel-projectile
  :bind ("C-c f" . counsel-projectile-find-file))

(use-package lsp-mode
  :commands lsp lsp--imenu-create-index lsp-enable-imenu lsp-register-client make-lsp-client lsp-stdio-connection
  :hook ((js-mode c-mode php-mode elixir-mode ruby-mode)
         . (lambda ()
             (require 'lsp-clients)
             (lsp)))
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-?" . lsp-find-references))
  :config
  (defun lsp-auto-enable-imenu ()
    (when (and lsp-mode (not (eq imenu-create-index-function #'lsp--imenu-create-index)))
      (lsp-enable-imenu)))
  (advice-add 'counsel-imenu :before #'lsp-auto-enable-imenu)
  :custom
  (lsp-auto-configure nil)
  (lsp-enable-completion-at-point nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-imenu-enable nil))

(use-package company-lsp
  :hook (lsp-mode . (lambda ()
                      (setq-local company-backends '((company-lsp company-yasnippet :with company-dabbrev-code)))))
  :custom
  (company-lsp-cache-candidates 'auto))

(use-package counsel-gtags
  :disabled
  :hook (php-mode . (lambda ()
                      (setq-local company-backends '((company-gtags company-files company-yasnippet company-css :with company-dabbrev-code)))
                      (counsel-gtags-mode)))
  :custom
  (counsel-gtags-auto-update t))

(use-package flycheck
  :custom
  (global-flycheck-mode t))

(use-package phpactor)

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
  :commands yas-global-mode
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package rg
  :commands rg-enable-default-bindings
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
  :commands move-text-default-bindings
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
  :commands sml/setup
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

(use-package csv-mode)

(use-package elec-pair
  :custom
  (electric-pair-mode t)
  (show-paren-mode t))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-Ahl")
  (dired-recursive-copies 'always)
  (dired-use-ls-dired t))

(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package recentf
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 200)
  (recentf-exclude '("bookmarks$" "-autoloads\\.el$")))

(use-package sql
  :hook (sql-mode . (lambda ()
                      (push `(sql-mode .
                                       ,(apply 'company-keywords-upper-lower
                                               (append
                                                sql-mysql-functions
                                                sql-mysql-keywords
                                                sql-mysql-data-types)))
                            company-keywords-alist)
                      (setq-local company-backends '((company-keywords :with company-dabbrev-code)))))
  :config
  (defvar sql-mysql-functions
    '("ascii" "avg" "bdmpolyfromtext" "bdmpolyfromwkb" "bdpolyfromtext"
      "bdpolyfromwkb" "benchmark" "bin" "bit_and" "bit_length" "bit_or"
      "bit_xor" "both" "cast" "char_length" "character_length" "coalesce"
      "concat" "concat_ws" "connection_id" "conv" "convert" "count"
      "curdate" "current_date" "current_time" "current_timestamp" "curtime"
      "elt" "encrypt" "export_set" "field" "find_in_set" "found_rows" "from"
      "geomcollfromtext" "geomcollfromwkb" "geometrycollectionfromtext"
      "geometrycollectionfromwkb" "geometryfromtext" "geometryfromwkb"
      "geomfromtext" "geomfromwkb" "get_lock" "group_concat" "hex" "ifnull"
      "instr" "interval" "isnull" "last_insert_id" "lcase" "leading"
      "length" "linefromtext" "linefromwkb" "linestringfromtext"
      "linestringfromwkb" "load_file" "locate" "lower" "lpad" "ltrim"
      "make_set" "master_pos_wait" "max" "mid" "min" "mlinefromtext"
      "mlinefromwkb" "mpointfromtext" "mpointfromwkb" "mpolyfromtext"
      "mpolyfromwkb" "multilinestringfromtext" "multilinestringfromwkb"
      "multipointfromtext" "multipointfromwkb" "multipolygonfromtext"
      "multipolygonfromwkb" "now" "nullif" "oct" "octet_length" "ord"
      "pointfromtext" "pointfromwkb" "polyfromtext" "polyfromwkb"
      "polygonfromtext" "polygonfromwkb" "position" "quote" "rand"
      "release_lock" "repeat" "replace" "reverse" "rpad" "rtrim" "soundex"
      "space" "std" "stddev" "substring" "substring_index" "sum" "sysdate"
      "trailing" "trim" "ucase" "unix_timestamp" "upper" "user" "variance")
    "MySQL Functions")
  (defvar sql-mysql-keywords
    '("action" "add" "after" "against" "all" "alter" "and" "as" "asc"
      "auto_increment" "avg_row_length" "bdb" "between" "by" "cascade"
      "case" "change" "character" "check" "checksum" "close" "collate"
      "collation" "column" "columns" "comment" "committed" "concurrent"
      "constraint" "create" "cross" "data" "database" "default"
      "delay_key_write" "delayed" "delete" "desc" "directory" "disable"
      "distinct" "distinctrow" "do" "drop" "dumpfile" "duplicate" "else" "elseif"
      "enable" "enclosed" "end" "escaped" "exists" "fields" "first" "for"
      "force" "foreign" "from" "full" "fulltext" "global" "group" "handler"
      "having" "heap" "high_priority" "if" "ignore" "in" "index" "infile"
      "inner" "insert" "insert_method" "into" "is" "isam" "isolation" "join"
      "key" "keys" "last" "left" "level" "like" "limit" "lines" "load"
      "local" "lock" "low_priority" "match" "max_rows" "merge" "min_rows"
      "mode" "modify" "mrg_myisam" "myisam" "natural" "next" "no" "not"
      "null" "offset" "oj" "on" "open" "optionally" "or" "order" "outer"
      "outfile" "pack_keys" "partial" "password" "prev" "primary"
      "procedure" "quick" "raid0" "raid_type" "read" "references" "rename"
      "repeatable" "restrict" "right" "rollback" "rollup" "row_format"
      "savepoint" "select" "separator" "serializable" "session" "set"
      "share" "show" "sql_big_result" "sql_buffer_result" "sql_cache"
      "sql_calc_found_rows" "sql_no_cache" "sql_small_result" "starting"
      "straight_join" "striped" "table" "tables" "temporary" "terminated"
      "then" "to" "transaction" "truncate" "type" "uncommitted" "union"
      "unique" "unlock" "update" "use" "using" "values" "when" "where"
      "with" "write" "xor")
    "MySQL Keywords")
  (defvar sql-mysql-data-types
    '("bigint" "binary" "bit" "blob" "bool" "boolean" "char" "curve" "date"
      "datetime" "dec" "decimal" "double" "enum" "fixed" "float" "geometry"
      "geometrycollection" "int" "integer" "line" "linearring" "linestring"
      "longblob" "longtext" "mediumblob" "mediumint" "mediumtext"
      "multicurve" "multilinestring" "multipoint" "multipolygon"
      "multisurface" "national" "numeric" "point" "polygon" "precision"
      "real" "smallint" "surface" "text" "time" "timestamp" "tinyblob"
      "tinyint" "tinytext" "unsigned" "varchar" "year" "year2" "year4"
      "zerofill")
    "MySQL Data Types")
  :custom
  (sql-product 'mysql))

(use-package graphviz-dot-mode
  :hook (graphviz-dot-mode
         . (lambda ()
             (setq-local show-trailing-whitespace t)
             (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
             (setq-local indent-line-function 'my-graphviz-dot-indent-line)))
  :bind (:map graphviz-dot-mode-map
              ("{" . self-insert-command)
              ("}" . self-insert-command)
              (";" . self-insert-command)
              ("RET" . newline))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("dot-language-server" "--stdio"))
                    :major-modes '(graphviz-dot-mode)
                    :priority -1
                    :server-id 'dot))
  (defun my--calc-indent-column ()
    (forward-line -1)
    (cond
     ((looking-at "^.*[{[]$")
      (+ (current-indentation) graphviz-dot-indent-width))
     ((looking-at "^[ \t]*$")
      (my--calc-indent-column))
     (t
      (current-indentation))))
  (defun my-graphviz-dot-indent-line ()
    (let ((current-point (point))
          (current-indent (current-indentation))
          (new-indent (save-excursion
                             (beginning-of-line)
                             (cond
                              ((bobp)
                               ;; simple case, indent to 0
                               0)
                              ((looking-at "^[ \t]*\\(}\\|]\\);?$")
                               ;; block closing, deindent relative to previous line
                               (forward-line -1)
                               (while (looking-at "^[ \t]*$")
                                 (forward-line -1))
                               (if (looking-at "^.*[{\\[]$")
                                   (current-indentation)
                                 (max 0 (- (current-indentation) graphviz-dot-indent-width))))
                              (t
                               ;; other cases need to look at previous lines
                               (my--calc-indent-column))))))
      (indent-line-to new-indent)
      (goto-char (+ current-point (- new-indent current-indent))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-try-passive-mode t)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
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
 '(network-security-level (quote high))
 '(package-selected-packages
   (quote
    (elixir-mode csv-mode yasnippet yasnippet-snippets phpactor flycheck counsel-gtags company-lsp lsp-ui magit zenburn-theme web-mode volatile-highlights use-package undo-tree smart-mode-line rg phpunit php-mode mozc-popup move-text migemo markdown-mode japanese-holidays ivy-xref ivy-historian gitignore-mode git-gutter+ expand-region emmet-mode editorconfig dockerfile-mode docker-compose-mode counsel-projectile company-statistics comment-dwim-2 apache-mode anzu add-node-modules-path)))
 '(scroll-bar-mode nil)
 '(shift-select-mode nil)
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
