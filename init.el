;;; init.el --- Alynx's init configurations.

;;; Commentary:
;; Most configurations should be in this file.

;;; Code:

;; Internal tweaks.

;; Disable startup message.
(setq inhibit-startup-message t)

;; Show loading time after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; Create local dir to redirect package-generated files.
;; It's better not to move `eln-cache` and `elpa` into local dir, `eln-cache` is
;; defined in `native-comp-eln-load-path`, which is a list contains user one and
;; system one, I am even not sure whether this is used to save compiled cache,
;; maybe it's only used to load. `elpa` is defined in `package-user-dir`, I
;; don't want to modify it, either.
;; `:parents` makes `make-directory` silience if dir exists.
(make-directory (locate-user-emacs-file ".local") :parents)
;; Cache dir which contains package caches that you can safely remove it.
(make-directory (locate-user-emacs-file ".local/cache") :parents)
;; Backup dir which contains backup files and auto save lists.
(make-directory (locate-user-emacs-file ".local/backup") :parents)

;; Disable menu bar, tool bar, scroll bar and cursor blink.
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Scroll bar is too narrow to click, better to disable it.
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Display line number (gutter) and highlight current line.
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Display line number and column number of cursor in mode line.
(line-number-mode 1)
(column-number-mode 1)

;; Use y or n instead yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Set cursor to underline.
(setq-default cursor-type 'hbar)

;; Highlight trailing whitespace in `prog-mode` only.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Disable line spacing.
;; Line space makes `highlight-indent-guides` wired.
;; (setq-default line-spacing nil)

;; Enable `pixel-scroll-precision-mode` added in Emacs 29.
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))
;; By default Emacs will jump a half screen if your cursor is out of screen,
;; this makes it behave like other editors, but sometimes it still jumps.
(setq scroll-margin 3)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq-default scroll-up-aggressively 0.01)
(setq-default scroll-down-aggressively 0.01)

;; Disable backup files.
(setq make-backup-files nil)
;; Move backup files and list into backup dir.
(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file ".local/backup"))))
(setq auto-save-list-file-prefix
      (locate-user-emacs-file ".local/backup/.saves-"))

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
;; This is not perfect, since font size is always integer, same line height
;; makes Chinese too small.
;; Better way is to custom Monaco's ascent and descent in its OS/2 table to make
;; it have the same ratio as Noto Sans Mono CJK SC.
;; However it will break box-drawing characters, which needs to be stretched.
;; If Emacs allows user to set a custom min line height, this will be solved.
(setq face-font-rescale-alist '(("Noto Sans Mono CJK SC" . 0.85)))

;; Indentations.

;; Making tab other length than 8 sounds like define PI to 3, if you don't want
;; 8, you should also not use tabs, but use spaces.
(setq-default tab-width 8)

;; This is the default value, which means if major mode does not set those
;; value, I'll use tabs and it's length should be 8 chars.
;; `indent-tabs-mode` does not mean use tabs only, it means if the indent level
;; can be divided by tab-width, use tabs, and use spaces for the remaining.
(setq-default indent-tabs-mode t)

;; Emacs have different indent variables for different modes. I'd like to
;; control them with one variable, and set it via different hooks.
;; Having one variable that holds different values for different buffers is
;; better than setting different variables for different buffers, so just make
;; aliases between `indent-offset` and mode-specific variables.
;; If installed more modes, add their indent variables here.
;; Maybe I can get all known modes' indent variables from `doom-modeline`.
;; See <https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-core.el#L310-L418>.
(defconst mode-indent-offsets '(c-basic-offset
                                js-indent-level
                                css-indent-offset
                                sgml-basic-offset
                                python-indent-offset
                                lua-indent-level
                                web-mode-code-indent-offset
                                web-mode-css-indent-offset
                                web-mode-markup-indent-offset
                                markdown-list-indent-width)
  "Different modes' indent variables to make alias to indent-offset.")

(dolist (mode-indent-offset mode-indent-offsets)
  (defvaralias mode-indent-offset 'indent-offset))

;; Make `indent-offset` a buffer-local variable.
(defvar-local indent-offset tab-width)

;; If you quote a list, not only itself, but it's elements will not be eval.
;; Using `list` will eval elements and return a list.
;; `interactive` wants a list of integers to fill arguments, so we cannot quote
;; here, because we need to evaluate `read-number`, and quote will prevent list
;; and it's elements to be evaluated.
;; Using `\`` with `,` can also evaluate selected elements.
;; Don't use `setq-default`, because every time we start a new major mode we set
;; those values, and if we open two files with different modes, the latter one
;; will cover the former one's value with `setq-default`.
(defun indent-tabs (num)
  "Mark this buffer to indent with tabs and set indent offset to NUM chars."
  (interactive `(,(read-number "Indent offset (chars): " 8)))
  (indent-tabs-mode 1)
  (setq indent-offset num))

(defun indent-spaces (num)
  "Mark this buffer to indent with spaces and set indent offset to NUM chars."
  (interactive `(,(read-number "Indent offset (chars): " 8)))
  (indent-tabs-mode -1)
  (setq indent-offset num))

;; Most projects saying that they are using 2 as `tab-width` actually means they
;; are using 2 as `indent-offset`. If you don't use spaces to indent,
;; `tab-width` has no meaning for you.
;; You should call `(indent-spaces 2)` for those projects.
;;
;; There are also other projects like GTK using 2 as `indent-offset`, and they
;; actually also assume `tab-width` is 8 and use tabs to indent. If you set
;; `tab-width` to 2, you'll find some 4-level code is 1-level, which is wrong.
;; You should call `(indent-tabs 2)` for those projects.
;;
;; So most of time you should not change `tab-width`, but maybe some crazy
;; projects use tabs for indent and they don't want `tab-width` to be 8, then
;; call this via `M-x` manually.
;; I personally think they should use 2 spaces instead.
(defun set-tab-width (num)
  "Mark this buffer to set tab width to NUM chars."
  (interactive `(,(read-number "Tab width (chars): " 8)))
  (setq tab-width num))

;; If installed more modes, add them here as `(mode-name . indent-offset)`.
(defconst indent-tabs-modes '((prog-mode . 8)
                              ;; `markdown-mode` is not a `prog-mode`.
                              (markdown-mode . 8)
                              (gfm-mode . 8))
  "Modes that will use tabs to indent.")

(defconst indent-spaces-modes '((lisp-mode . 2)
                                (emacs-lisp-mode . 2)
                                (js-mode . 2)
                                (css-mode . 2)
                                (html-mode . 2)
                                (yaml-mode . 2)
                                (lua-mode . 3)
                                (python-mode . 4))
  "Modes that will use spaces to indent.")

;; `intern` returns symbol by string. `symbol-name` returns string by symbol.
;; In dynamic binding, lambda is self-quoting, and there is no closure, so we
;; need to evaluate `(cdr pair)` first.
;; Some modes set `tab-width` to other value, correct them to 8.
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

;; Keymaps.

;; Keybindings for setting indentations.
(global-set-key (kbd "C-c i TAB") 'indent-tabs)
(global-set-key (kbd "C-c i SPC") 'indent-spaces)
(global-set-key (kbd "C-c i w") 'set-tab-width)
;; Atom style indent left or right.
;; See <https://dougie.io/emacs/indent-selection/>.
(global-set-key (kbd "M-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-]") 'indent-rigidly-right-to-tab-stop)

;; I use this in Atom, but by default `M-;` is used in Emacs, so I may use this
;; keybinding for others in future.
(global-set-key (kbd "C-;") 'comment-dwim)

;; See <https://www.gnu.org/software/emacs/manual/html_node/efaq/Backspace-invokes-help.html>.
;; A long story: old terminals make Backspace generate the same code as `C-h`,
;; and make Delete generate `DEL` code.
;; Emacs binds backward delete to `DEL` code and help to `C-h` (Maybe that's
;; why by default HHKB has a Delete key in Backspace's place).
;; But in GUI, Emacs makes Backspace generate `DEL` code, and make Delete
;; generate another code which is bound to forward delete.
;; Also GNOME Terminal makes Backspace generate `DEL` code by default, too.
;; Another problem is that GNOME Terminal makes Delete generate escape sequence
;; by default, Emacs cannot handle it, but there are also conflict with other
;; keybindings, so I just suggest not to use Emacs in terminal.
;; It's not good to translate Delete to `DEL` code and bind Backspace to
;; functions like `backward-delete-char`, since many backward delete related
;; functions are actually bound to `DEL` code.
;; So the only thing to do is to make `C-h` generate `DEL` code to make it the
;; same behavior as Backspace like `C-i` for `TAB`. We have `F1` for help.
;; See <https://www.emacswiki.org/emacs/BackspaceKey>.
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;; And don't forget to make `M-h` the same as `M-DEL`.
;; I am not using `keyboard-translate` here, since it only accept 1 char (or a
;; key code), `M-DEL` does not generate a single key code, but a sequence.
;; I use `kbd` because it's easy to read, char constant or vector is also OK.
;; See <https://ftp.gnu.org/old-gnu/Manuals/emacs-20.7/html_node/emacs_451.html>.
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))

;; By default, `join-line` will join current line into previous line.
;; In Atom, I typically join next line into current line.
;; See <https://emacsredux.com/blog/2013/05/30/joining-lines/>.
(defun join-next-line ()
  "Join the current line with the next line."
  (interactive)
  (join-line 1))
;; `C-j` is used to insert evaluated value in `lisp-interaction-mode`, maybe I
;; should find another keybinding for this.
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

  ;; Move lines first.
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; Remap `C-a`, `Home` to `smarter-move-beginning-of-line`.
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c b") 'windmove-left)
;; `S-<arrow>` to move between windows (`S` means Shift).
(windmove-default-keybindings)
;; The default Buffer List is ugly, replace it with IBuffer.
;; However I don't use them both.
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;; Cannot find a keybinding for this. Just call it.
(defun show-file-path ()
  "Show the full file path of current buffer in the minibuffer."
  (interactive)
  (message "%s" buffer-file-name))

;; I maybe use this as some custom keybindings' prefix, but currently I prefer
;; `C-c`, because `C-z` is hard to press.
(global-unset-key (kbd "C-z"))

;; Packages.

(require 'package)
(setq package-archives
      '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Load `use-package`.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Let `use-package` always install packages.
(setq use-package-always-ensure t)

;; Install packages.

;; If a package is not needed since startup and has no bind or mode or hook
;; to make `use-package` auto load it, add `:defer 1` to load it after 1 second.
;; Difference between `:defer t` and `:defer 1`: `:defer 1` will load package
;; after 1 second, but `:defer t` does not load package, expects other options
;; load it.

;; Simple packages that have no dependencies.

;; I hardly try.
;; (use-package try
;;   :defer 1)

;; My favorite theme.
;; Don't defer this, I need it all time.
(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

;; Or if you like `mood-one-theme`.
;; (use-package mood-one-theme
;;   :config
;;   (load-theme 'mood-one t)
;;   (mood-one-theme-arrow-fringe-bmp-enable)
;;   (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable))

;; `doom-modeline` is good, but `mood-line` is enough for me.
;; Don't defer this, either.
(use-package mood-line
  :config
  (mood-line-mode 1)
  :custom
  (mood-line-show-encoding-information t)
  (mood-line-show-eol-style t))

;; Atom-like move regine/current line up and down.
(use-package move-text
  :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))

;; Atom-like default region.
;; If there is no region, behave like current line is current region.
;; Works on indent/outdent, comment block, cut (kill), copy, paste (yank).
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package highlight-indent-guides
  :defer t
  ;; I only use this in `prog-mode`.
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?│)
  (highlight-indent-guides-bitmap-function
   'highlight-indent-guides--bitmap-line))

;; Highlight FIXME or TODO.
(use-package hl-todo
  :defer t
  :hook ((prog-mode . hl-todo-mode)))

;; Built in minor mode to display column ruler.
(use-package display-fill-column-indicator
  :defer t
  ;; I only use this in `prog-mode`.
  :hook ((prog-mode . display-fill-column-indicator-mode))
  :custom
  ;; Set column ruler at 80 columns.
  (display-fill-column-indicator-column 80))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Not good, I only use minimap as scroll bar, but it cannot do this well.
;; If use this don't forget to install block font.
;; See <https://github.com/jandamm/doom-emacs-minimap/blob/master/blockfont.ttf>.
;; (use-package minimap
;;   :defer 1
;;   :config
;;   (add-to-list 'minimap-major-modes 'markdown-mode)
;;   (minimap-mode 1)
;;   :custom
;;   (minimap-window-location 'right)
;;   ;; Enlarge breaks BlockFont.
;;   (minimap-enlarge-certain-faces nil)
;;   :custom-face
;;   (minimap-font-face ((t (:height 30 :family "BlockFont")))))

(use-package org-bullets
  :defer t
  :hook ((org-mode . org-bullets-mode)))

(use-package avy
  :bind (("M-g a" . avy-goto-char)))

(use-package yasnippet
  :defer t
  :hook ((prog-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer 1)

;; Built in minor mode to save recent files.
;; This is not needed in startup so we defer it for 1 second.
;; Only ivy virtual buffers need it.
(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  :custom
  (recentf-save-file (locate-user-emacs-file ".local/recentf")))

;; Built in minor mode to open files at last-edited position.
(use-package saveplace
  ;; Don't defer this if you want it to work on the first file you opened.
  ;; :defer 1
  :config
  (save-place-mode 1)
  :custom
  (save-place-file (locate-user-emacs-file ".local/places")))

;; I hardly use built in eshell, but I still redirect its data dir.
(use-package eshell
  :custom
  (eshell-directory-name (locate-user-emacs-file ".local/eshell")))

(use-package better-shell
  :bind (("C-\"" . better-shell-shell)
         ("C-:" . better-shell-remote-open)))

;; Complex packages that have dependencies.
;; Don't change the sequence, because the latter needs to obey the former's
;; `:custom`, for example `projectile` and `counsel-projectile`. If a package
;; depends on other package, it should be placed after all of its dependencies.
;; I don't use `:after` here, because we may have keybindings for dependencies,
;; which leads into an autoload, and if you have 2 dependencies in `:after`,
;; you need to first press both keybindings of those 2 dependencies, then the
;; package will be loaded, it won't be loaded before those keybindings. I don't
;; want this, I just want to run dependencies' `:custom` first, so keep the
;; sequence is easiest.
;; See <https://github.com/jwiegley/use-package/issues/976#issuecomment-1056017784>.

(use-package company
  :defer t
  :hook ((prog-mode . company-mode))
  :custom
  (debug-on-error nil)
  (lsp-completion-provider :capf))

;; I remove `:diminish` here because `mood-line` hides minor modes by default.
(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  ;; I don't want ivy to add `/` after I press `~`.
  (ivy-magic-tilde nil))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind (("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-rg)
	 ("C-c l" . counsel-locate)
	 ;; ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package treemacs
  :defer 1
  ;; I never use internal input method so bind this to `treemacs`.
  :bind (("C-\\" . treemacs))
  ;; Disable line number for `treemacs` window.
  :hook ((treemacs-mode . (lambda() (display-line-numbers-mode -1))))
  :custom
  ;; Default `treemacs` window is too wide.
  (treemacs-width 20)
  (treemacs-persist-file (locate-user-emacs-file ".local/treemacs-persist"))
  (treemacs-last-error-persist-file
   (locate-user-emacs-file ".local/treemacs-persist-at-last-error")))

(use-package projectile
  :config
  (projectile-mode 1)
  :bind (("M-s" . projectile-ripgrep))
  ;; See <https://github.com/jwiegley/use-package#binding-to-keymaps>.
  :bind-keymap (("C-x p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-cache-file
   (locate-user-emacs-file ".local/cache/projectile.cache"))
  (projectile-known-projects-file
   (locate-user-emacs-file ".local/projectile-bookmarks.eld")))

(use-package flycheck
  :defer t
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode)))

(use-package lsp-mode
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
	 ;; Don't enable lsp for `web-mode`, I only use this for templates, and
         ;; lsp cannot understand nunjucks.
         ;; (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references))
  ;; Move lsp files into local dir.
  :custom
  (lsp-server-install-dir (locate-user-emacs-file ".local/lsp"))
  (lsp-session-file (locate-user-emacs-file ".local/lsp-session"))
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ivy
  :defer 1
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :defer 1
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :defer 1
  :commands lsp-treemacs-errors-list)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package treemacs-projectile
  :defer 1)

(use-package tree-sitter
  :defer 1
  :config
  ;; Enable `tree-sitter` for all supported major modes.
  (global-tree-sitter-mode 1)
  ;; Use `tree-sitter` for highlight on supported major modes.
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :defer 1)

;; Modes and tools for different languages.

(use-package js2-mode
  :hook ((js2-mode . js2-imenu-extras-mode))
  :mode (("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  :hook ((js2-mode . js2-refactor-mode))
  :bind (:map js2-mode-map ("C-k" . js2r-kill)))

(use-package xref-js2
  :hook ((js2-mode . (lambda ()
                      (add-hook 'xref-backend-functions
                                #'xref-js2-xref-backend nil t))))
  :bind (:map js-mode-map ("M-." . nil))
  :custom (xref-js2-search-program 'rg))

(use-package json-mode
  :bind (:map json-mode-map ("C-c <tab>" . json-mode-beautify))
  :mode (("\\.bowerrc\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)
         ("\\.json_schema\\'" . json-mode)
         ("\\.json\\'" . json-mode)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)
         ;; DaVinci Resolve's fuse scripts, it uses lua.
         ("\\.fuse\\'" . lua-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-command "marked")
  (markdown-gfm-use-electric-backquote nil)
  (markdown-indent-on-enter nil))

(use-package web-mode
  :mode (("\\.njk\\'" . web-mode) ("\\.j2\\'" . web-mode)))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode)))

;; Maybe not good for `lsp-mode` so I am not using this.
;; Make gc pauses faster by decreasing the threshold.
;; (setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here.
