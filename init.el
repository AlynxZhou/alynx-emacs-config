;;; init.el --- Alynx's major configurations.

;;; Commentary:
;; I have nothing to say here, just configurations.

;;; Code:

;; Internal tweaks.

;; Make startup faster by reducing the frequency of garbage collection.
;; The default is 800 kilobytes. Measured in bytes.
;; See <http://blog.lujun9972.win/emacs-document/blog/2019/03/15/%E9%99%8D%E4%BD%8Eemacs%E5%90%AF%E5%8A%A8%E6%97%B6%E9%97%B4%E7%9A%84%E9%AB%98%E7%BA%A7%E6%8A%80%E6%9C%AF/index.html>.
;; Move this into `early-init.el` can reduce more 10 gcs and 0.07 second.
;; But I don't want more file.
(setq gc-cons-threshold (* 50 1024 1024))

;; Disable startup message.
(setq inhibit-startup-message t)

;; Start every frame maximized.
;; See <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show loading time after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

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

;; Use y or n instead yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set cursor to underline.
(setq-default cursor-type 'hbar)

;; Highlight trailing whitespace in prog-mode only.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Disable line spacing.
;; Line space makes highlight-indent-guides wired.
;; (setq-default line-spacing nil)

;; By default Emacs will jump a half screen if your cursor is out of screen.
;; This makes it behave like other editors, but sometimes it still jumps.
(setq scroll-margin 3)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq-default scroll-up-aggressively 0.01)
(setq-default scroll-down-aggressively 0.01)

;; Disable backup files.
(setq make-backup-files nil)
;; Move backup files and list into backup dir.
(make-directory (locate-user-emacs-file ".backup") :parents)
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file ".backup"))))
(setq auto-save-list-file-prefix (locate-user-emacs-file ".backup/.saves-"))

;; Set customization data in a specific file, without littering my init files.
;; `locate-user-emacs-file` will create file if it does not exist.
(setq custom-file (locate-user-emacs-file "custom-file.el"))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load-file custom-file)

;; Fonts.

;; (setq font-use-system-font t)
;; Set default font.
;; `fill-column-indicator` and `highlight-indent-guide` uses box-drawing
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
;; This is not perfect, since font size is always integer,
;; same line height makes Chinese too small.
;; Better way is to custom Monaco's ascent and descent in its OS/2 table,
;; to make it have the same ratio as Noto Sans Mono CJK SC.
;; However it will break box-drawing characters, which needs to be stretched.
;; If Emacs allows user to set a custom min line height, this will be solved.
(setq face-font-rescale-alist '(("Noto Sans Mono CJK SC" . 0.85)))

;; Keymaps.

;; Atom style indent left or right.
;; See <https://dougie.io/emacs/indent-selection/>.
(global-set-key (kbd "M-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-]") 'indent-rigidly-right-to-tab-stop)

;; I use this in Atom, but by default `M-;` is used in Emacs.
;; So I may use this keybinding for others in future.
(global-set-key (kbd "C-;") 'comment-dwim)

;; Need a better key for help, `C-h` is backspace and `C-?` is undo-tree-redo.
;; Seems counsel use `<F1>` as prefix.
;; (global-set-key (kbd "C-?") 'help-command)
(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h")  'delete-backward-char)

;; By default, `join-line` will join current line into previous line.
;; In Atom, I typically join next line into current line.
;; See <https://emacsredux.com/blog/2013/05/30/joining-lines/>.
(defun join-next-line ()
  "Join the current line with the next line."
  (interactive)
  (join-line t))
(global-set-key (kbd "C-j") 'join-next-line)

;; See <https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap `C-a`, `Home` to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z n") 'windmove-down)
(global-set-key (kbd "C-z p") 'windmove-up)
(global-set-key (kbd "C-z f") 'windmove-right)
(global-set-key (kbd "C-z b") 'windmove-left)
;; Shift+Arrow to move between windows.
(windmove-default-keybindings)

;; Cannot find a keybinding for this.
;; Just call it.
(defun show-file-path ()
  "Show the full file path of current buffer in the minibuffer."
  (interactive)
  (message "%s" buffer-file-name))

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
;; You should call `(indent-tabs 2)` for those projects.
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
(setq backward-delete-char-untabify-method nil)

;; Packages.

(require 'package)
(setq-default package-enable-at-startup nil)
(setq-default package-archives
              '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Load use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install packages.

;; If a package is not needed since startup and has no bind or mode or hook
;; to make `use-package` auto load it, add `:defer 1` to load it after 1 second.
;; Difference between `:defer t` and `:defer 1`: `:defer 1` will load package
;; after 1 second, but `:defer t` does not load package, expects other options
;; load it.

;; My favorite theme.
;; Don't defer this, I need it all time.
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package avy
  :ensure t
  :bind (("M-g a" . avy-goto-char)))

(use-package better-shell
  :ensure t
  :bind (("C-\"" . better-shell-shell)
         ("C-:" . better-shell-remote-open)))

(use-package company
  :ensure t
  :defer t
  :hook ((prog-mode . company-mode))
  :custom
  (debug-on-error nil)
  (lsp-completion-provider :capf))

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
	 ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

;; Built in minor mode to display column ruler.
(use-package display-fill-column-indicator
  :ensure t
  :defer t
  ;; I only use this in prog-mode.
  :hook ((prog-mode . display-fill-column-indicator-mode))
  :custom
  ;; Set column ruler at 80 columns.
  (display-fill-column-indicator-column 80))

(use-package flycheck
  :ensure t
  :defer t
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode)))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?│)
  (highlight-indent-guides-bitmap-function
   'highlight-indent-guides--bitmap-line))

;; Highlight FIXME or TODO.
(use-package hl-todo
  :ensure t
  :defer t
  :hook ((prog-mode . hl-todo-mode)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :config
  (ivy-mode t)
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  ;; I don't want ivy to add `/` after I press `~`.
  (ivy-magic-tilde nil))

(use-package js2-mode
  :ensure t
  :hook ((js2-mode . js2-imenu-extras-mode))
  :mode (("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  :hook ((js2-mode . js2-refactor-mode))
  :bind (:map js2-mode-map ("C-k" . js2r-kill)))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map ("C-c <tab>" . json-mode-beautify))
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)
         ("\\.json_schema\\'" . json-mode)
         ("\\.json\\'" . json-mode)))

(use-package lsp-ivy
  :ensure t
  :defer 1
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (cuda-mode . lsp-deferred)
	 (objc-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (js-mode . lsp-deferred)
	 (js2-mode . lsp-deferred)
         (python-mode . lsp-deferred)
	 ;; Don't enable lsp for web-mode, I only use this for templates,
	 ;; lsp cannot understand nunjucks.
         ;; (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references))
  ;; Move lsp session file into cache dir.
  :custom
  (lsp-session-file (locate-user-emacs-file ".cache/lsp-session"))
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-treemacs
  :ensure t
  :defer 1
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :ensure t
  :defer 1
  :commands lsp-ui-mode)

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ;; DaVinci Resolve's fuse scripts, it uses lua.
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
;;   :defer 1
;;   :config
;;   (add-to-list 'minimap-major-modes 'markdown-mode)
;;   (minimap-mode t)
;;   :custom
;;   (minimap-window-location 'right)
;;   ;; Enlarge breaks BlockFont.
;;   (minimap-enlarge-certain-faces nil)
;;   :custom-face
;;   (minimap-font-face ((t (:height 30 :family "BlockFont")))))

;; doom-modeline is good, but mood-line is enough for me.
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode t)
  :custom
  (mood-line-show-encoding-information t)
  (mood-line-show-eol-style t))

;; Atom-like move regine/current line up and down.
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))

(use-package org-bullets
  :ensure t
  :defer t
  :hook ((org-mode . org-bullets-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)
	      ("M-s" . projectile-ripgrep))
  :custom
  (projectile-cache-file
   (locate-user-emacs-file ".cache/projectile.cache"))
  (projectile-known-projects-file
   (locate-user-emacs-file ".cache/projectile-bookmarks.eld")))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Built in minor mode to save recent files.
;; This is not needed in startup so we defer it for 1 second.
;; Only ivy virtual buffers need it.
(use-package recentf
  :ensure t
  :defer 1
  :config
  (recentf-mode t)
  :custom
  (recentf-save-file (locate-user-emacs-file ".cache/recentf")))

;; Built in minor mode to open files at last-edited position.
(use-package saveplace
  :ensure t
  ;; Don't defer this if you want it to work on the first file you opened.
  ;; :defer 1
  :config
  (save-place-mode t)
  :custom
  (save-place-file (locate-user-emacs-file ".cache/places")))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package treemacs
  :ensure t
  :defer 1)

(use-package treemacs-projectile
  :ensure t
  :defer 1)

;; I hardly try.
;; (use-package try
;;   :ensure t
;;   :defer 1)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

(use-package web-mode
  :ensure t
  :mode (("\\.njk\\'" . web-mode) ("\\.j2\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode t))

;; Atom-like default region.
;; If there is no region, behave like current line is current region.
;; Works on indent/outdent, comment block, cut (kill), copy, paste (yank).
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

(use-package xref-js2
  :ensure t
  :hook ((js2-mode . (lambda ()
                      (add-hook 'xref-backend-functions
                                #'xref-js2-xref-backend nil t))))
  :bind (:map js-mode-map ("M-." . nil))
  :custom (xref-js2-search-program 'rg))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode)))

(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :ensure t
  :defer 1)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here.
