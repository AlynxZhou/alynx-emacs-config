;;; init.el --- Alynx's major configurations.

;;; Commentary:
;; I have nothing to say here, just configurations.

;;; Code:

;; Internal tweaks.

;; Disable startup message.
(setq inhibit-startup-message t)

;; Start every frame maximized.
;; See <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Create cache dir to redirect package-generated files.
;; `:parents` makes `make-directory` silience if dir exists.
(make-directory (locate-user-emacs-file ".cache") :parents)

;; Disable menu bar, tool bar, scroll bar and cursor blink.
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Scroll bar is too narrow to click, better to disable it.
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Display line number and highlight current line.
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

;; Display line number and column number in mode line.
(line-number-mode t)
(column-number-mode t)

;; Set column ruler at 80 columns.
(setq-default display-fill-column-indicator-column 80)
;; Display it globally.
(global-display-fill-column-indicator-mode t)
;; Or maybe only display it in prog-mode.
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Built in minor mode to open files at last-edited position.
(save-place-mode t)
(customize-set-variable 'save-place-file
                        (locate-user-emacs-file ".cache/places"))
;; Built in minor mode to save recent files.
(recentf-mode t)
(customize-set-variable 'recentf-save-file
                        (locate-user-emacs-file ".cache/recentf"))

;; Use y or n instead yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set cursor to underline.
(setq-default cursor-type 'hbar)

;; Highlight trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Disable line spacing.
;; Line space makes highlight-indent-guides wired.
;; (setq-default line-spacing nil)

;; Disable backup files.
(setq make-backup-files nil)
;; Move backup files and list into backup dir.
(make-directory (locate-user-emacs-file ".backup") :parents)
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file ".backup"))))
(setq auto-save-list-file-prefix (locate-user-emacs-file ".backup/.saves-"))

;; Set customization data in a specific file, without littering my init files.
;; `locate-user-emacs-file` will create file if it does not exist.
(setq custom-file (locate-user-emacs-file "custom-file.el"))
(load-file custom-file)

;; Fonts.

;; (setq font-use-system-font t)
;; Set default font.
;; Emacs's fill-column-indicator and highlight-indent-guide uses box drawing
;; characters to draw bars, but the default characters in Monaco is not so good,
;; it has padding before and after it. To fix this I used my patched Monaco
;; which merges Menlo's characters into it.
(set-face-attribute 'default nil
                    :family "Monaco"
                    ;; :slant 'normal
                    :width 'normal
                    :weight 'normal
                    ;; 1 height is 1/10 pt.
                    :height 140)

;; Set CJK font. 中文
;; Don't set size here, otherwise when scaling Chinese won't scale.
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font t charset (font-spec :family "Noto Sans Mono CJK SC"
                                         ;; :slant 'normal
                                         :width 'normal
                                         :weight 'normal)))

;; Make Monaco and Noto Sans CJK SC the same line height.
(setq face-font-rescale-alist '(("Noto Sans Mono CJK SC" . 0.85)))

;; Keymaps.

;; I probably need to make those functions handle region.
;; Atom will handle region for those, and if no region,
;; region should be current line.
(defmacro save-column (&rest body)
  "I am not sure what this is, copied from Internet BODY."
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)
(defun move-line-up ()
  "Move current line up."
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))
(defun move-line-down ()
  "Move current line down."
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
;; Atom style indent left or right.
;; See <https://dougie.io/emacs/indent-selection/>.
(global-set-key (kbd "M-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-]") 'indent-rigidly-right-to-tab-stop)
;; Need a better key for help, `C-h` is backspace and `C-?` is undo-tree-redo.
;; (global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h")  'delete-backward-char)
(global-set-key (kbd "C-j") 'join-line)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z n") 'windmove-down)
(global-set-key (kbd "C-z p") 'windmove-up)
(global-set-key (kbd "C-z f") 'windmove-right)
(global-set-key (kbd "C-z b") 'windmove-left)
;; Shift+Arrow to move between windows.
(windmove-default-keybindings)
;; (global-unset-key (kbd "C-x 0"))
;; (global-set-key (kbd "C-x C-0") 'delete-windows)
;; (global-unset-key (kbd "C-x 1"))
;; (global-set-key (kbd "C-x C-1") 'delete-other-windows)
;; (global-unset-key (kbd "C-x 1"))
;; (global-set-key (kbd "C-x C-2") 'split-window-below)
;; (global-unset-key (kbd "C-x 2"))
;; (global-set-key (kbd "C-x C-3") 'split-window-right)
;; (global-unset-key (kbd "C-x o"))
;; (global-set-key (kbd "C-x C-o") 'other-window)
;; (global-unset-key (kbd "C-x h"))
;; (global-set-key (kbd "C-x C-h") 'mark-whole-buffer)
;; (global-unset-key (kbd "C-x b"))
;; (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
;; (global-unset-key (kbd "C-x k"))
;; (global-set-key (kbd "C-x C-k") 'kill-buffer)

;; Indentations.

;; Making tab other length than 8 sounds like define PI to 3, if you don't
;; want 8, you should also not use tabs, but use spaces.
(setq-default tab-width 8)

;; This is the default value, which means if major mode does not set those
;; value, I'll use tabs and it's length should be 8 chars.
;; `indent-tabs-mode` does not mean use tabs only, it means if the indent level
;; can be divided by tab-width, use tabs, and use spaces for the remaining.
(setq-default indent-tabs-mode t)

;; Emacs have different indent variables for different modes.
;; I'd like to control them with one variable, and set it via different hooks.
;; Having one variable that holds different values for different buffers is
;; better than setting different variables for different buffers,
;; so just make aliases between `indent-offset` and mode-specific variables.
;; If installed more modes, add their indent-offset here.
(defvar mode-indent-offsets '(c-basic-offset
                              js-indent-level
                              css-indent-offset
                              sgml-basic-offset
                              python-indent-offset
                              lua-indent-level
                              web-mode-code-indent-offset
                              web-mode-css-indent-offset
                              web-mode-markup-indent-offset
                              markdown-list-indent-width))

(dolist (mode-indent-offset mode-indent-offsets)
  (defvaralias mode-indent-offset 'indent-offset))

(defvar-local indent-offset tab-width)

;; If you quote a list, not only itself, but it's elements will not be eval.
;; Using `list` will eval elements and return a list.
;; `interactive` wants a list of integers to fill arguments,
;; so we cannot quote here, because we need to evaluate `read-number`,
;; and quote will prevent list and it's elements to be evaluated.
;; Using `\`` with `,` can also evaluate selected elements.
;; Don't use `setq-default`, because every time we start a new major mode we
;; set those values, and if we open two files with different modes,
;; the latter one will cover the former one's value with `setq-default`.
(defun indent-tabs (num)
  "Mark this buffer to indent with tabs and set indent offset to NUM chars."
  (interactive `(,(read-number "Indent offset (chars): " 8)))
  (setq indent-tabs-mode t)
  (setq indent-offset num))

(defun indent-spaces (num)
  "Mark this buffer to indent with spaces and set indent offset to NUM chars."
  (interactive `(,(read-number "Indent offset (chars): " 8)))
  (setq indent-tabs-mode nil)
  (setq indent-offset num))

;; Most projects saying that they are using 2 as tab-width actually means
;; they are using 2 as indent-offset. If you don't use spaces to indent,
;; tab-width has no meaning for you.
;; You should call `(indent-spaces 2)` for those projects.
;;
;; There are also other projects like GTK using 2 as indent-offset, and they
;; actually also assume tab-width is 8 and use tabs to indent. If you set
;; tab-width to 2, you'll find some 4-level code become 1-level, which is wrong.
;; You should call `(indent-tabs 2) for those projects.
;;
;; So most of time you should not change tab-width,
;; but maybe some crazy projects use tabs for indent and they don't want
;; tab-width to be 8, then call this.
;; I personally think they should use 2 spaces instead.
(defun set-tab-width (num)
  "Mark this buffer to set tab width to NUM chars."
  (interactive `(,(read-number "Tab width (chars): " 8)))
  (setq tab-width num))

;; If installed more modes, add them here as `(mode-name . indent-offset)`.
(defvar indent-tabs-modes '((prog-mode . 8)
                            ;; markdown-mode is not a prog-mode.
                            (markdown-mode . 8)
                            (gfm-mode . 8)))

(defvar indent-spaces-modes '((lisp-mode . 2)
                              (emacs-lisp-mode . 2)
                              (js-mode . 2)
                              (css-mode . 2)
                              (html-mode . 2)
                              (yaml-mode . 2)
                              (lua-mode . 3)
                              (python-mode . 4)))

;; `intern` returns symbol by string. `symbol-name` returns string by symbol.
;; In dynamic binding, lambda is self-quoting, and there is no closure.
;; so we need to evaluate `(cdr pair)` first.
;; Some modes set tab-width to other value, correct them to 8.
(dolist (pair indent-tabs-modes)
  (add-hook (intern (concat (symbol-name (car pair)) "-hook"))
            `(lambda () (indent-tabs ,(cdr pair)) (set-tab-width 8))))

(dolist (pair indent-spaces-modes)
    (add-hook (intern (concat (symbol-name (car pair)) "-hook"))
              `(lambda () (indent-spaces ,(cdr pair)) (set-tab-width 8))))

;; Add a indentation indicator on mode line.
;; Must use `:eval`, mode line constructor does not work for numbers.
(setq mode-line-misc-info '(:eval (format "%s %d %d"
					  (if indent-tabs-mode "TAB" "SPC")
					  indent-offset
					  tab-width)))

;; See <https://dougie.io/emacs/indentation/>.
;; Don't auto-indent line when pressing enter.
(setq-default electric-indent-inhibit t)
;; By default if you press backspace on indentations, Emacs will turn a tab into
;; spaces and delete one space, I think no one will like this.
(setq backward-delete-char-untabify-method 'nil)

;; Packages.

(require 'package)
(setq-default package-enable-at-startup nil)
(setq-default package-archives
              '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Load use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install packages.

;; Used by doom-modeline.
;; Don't forget to run `M-x all-the-icons-install-fonts`.
;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p))

;; My favorite theme.
(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package better-shell
    :ensure t
    :bind (("C-\"" . better-shell-shell)
           ("C-:" . better-shell-remote-open)))

(use-package company
  :ensure t
  :config (global-company-mode t)
  :custom (debug-on-error nil)
  (lsp-completion-provider :capf)
  :hook (js2-mode . company-mode))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)
	 ("C-S-o" . counsel-rhythmbox))
  :bind (:map minibuffer-local-map
	    ("C-r" . counsel-minibuffer-history)))

;; (use-package doom-modeline
;;   :ensure t
;;   :config (doom-modeline-mode t)
;;   ;; I don't use this,
;;   ;; I have a custom one that can show both indent-offset and tab-width.
;;   :custom (doom-modeline-indent-info nil))

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode)))

;; Not sure if I need this.
;; (use-package ggtags
;;   :ensure t
;;   :hook ((c-mode c++-mode java-mode) . ggtags-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?│)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

;; Highlight FIXME or TODO.
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; Not so useful.
;; (use-package hungry-delete
;;   :ensure t
;;   :config (global-hungry-delete-mode t))

;; I even hardly use this in Atom.
;; (use-package iedit
;;   :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume))
  :config (ivy-mode t)
  :custom (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  ;; I don't want ivy to add `/` after I press `~`.
  (ivy-magic-tilde nil))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :config (js2r-add-keybindings-with-prefix "C-c C-r")
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map ("C-k" . js2r-kill)))

(use-package json-mode
  :ensure t
;;  :custom (json-reformat:indent-width 2)
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)
         ("\\.json_schema\\'" . json-mode)
         ("\\.json\\'" . json-mode))
  :bind (:map json-mode-map ("C-c <tab>" . json-mode-beautify)))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom (lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references))
  :hook (((c-mode
           c++-mode
           c-or-c++-mode
           css-mode
           cuda-mode
	   objc-mode
           html-mode
           java-mode
           js-mode
	   js2-mode
           python-mode
	   ;; Don't enable lsp for web-mode, I only use this for templates,
	   ;; lsp cannot understand nunjucks.
           ;; web-mode
           ) . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  ;; Move lsp session file into cache dir.
  :custom (lsp-session-file (locate-user-emacs-file ".cache/lsp-session")))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.fuse\\'" . lua-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-command "marked")
  (markdown-gfm-use-electric-backquote nil)
  (markdown-indent-on-enter nil))

;; Not good, I only use minimap as scroll bar, but it cannot do this well.
;; https://github.com/jandamm/doom-emacs-minimap/blob/master/blockfont.ttf
;; (use-package minimap
;;   :ensure t
;;   :custom
;;   (minimap-window-location 'right)
;;   ;; Enlarge breaks BlockFont.
;;   (minimap-enlarge-certain-faces nil)
;;   :custom-face
;;   (minimap-font-face ((t (:height 30 :family "BlockFont"))))
;;   :config (progn (add-to-list 'minimap-major-modes 'markdown-mode)
;; 		 (minimap-mode t)))

;; doom-modeline is good, but mood-line is enough for me.
(use-package mood-line
  :ensure t
  :config (mood-line-mode t)
  :custom (mood-line-show-encoding-information t) (mood-line-show-eol-style t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package projectile
  :ensure t
  :config (projectile-mode t)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)
	      ("C-c p s" . projectile-ripgrep))
  :custom
  (projectile-cache-file
   (locate-user-emacs-file ".cache/projectile.cache"))
  (projectile-known-projects-file
   (locate-user-emacs-file ".cache/projectile-bookmarks.eld")))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package treemacs
  :ensure t)

(use-package treemacs-projectile
  :ensure t)

;; I hardly try.
;; (use-package try
;;   :ensure t)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t))

(use-package web-mode
  :ensure t
  :mode (("\\.njk\\'" . web-mode) ("\\.j2\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package xref-js2
  :ensure t
  :custom (xref-js2-search-program 'rg)
  :hook (js2-mode . (lambda ()
                      (add-hook 'xref-backend-functions
                                #'xref-js2-xref-backend nil t)))
  :bind (:map js-mode-map ("M-." . nil)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode)))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;;; init.el ends here.
