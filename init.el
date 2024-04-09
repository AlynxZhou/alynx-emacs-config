;;; init.el --- Alynx's init configurations. -*- lexical-binding: t; -*-

;;; Commentary:
;; Most configurations should be in this file.

;;; Code:

;; NOTE: If a function / variable is not used for direct calling, add a `alynx/`
;; prefix for it.

;; Tweaks.
;;
;; Those belong to no package and should be done during initialization.

;; Show loading details after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((packages-installed (length load-path)))
              (message
               "Emacs is ready with %s, called %s and took %s."
               (format "%d installed %s"
                       packages-installed
                       (if (> packages-installed 1) "packages" "package"))
               (format "%d garbage %s"
                       gcs-done
                       (if (> gcs-done 1) "collections" "collection"))
               (emacs-init-time "%.2fs")))))

;; Git submodule packages.
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; Create local dir to redirect package-generated files.
;;
;; It's better not to move `eln-cache` and `elpa` into local dir, `eln-cache` is
;; defined in `native-comp-eln-load-path`, which is a list contains user one and
;; system one, I am even not sure whether this is used to save compiled cache,
;; maybe it's only used to load. `elpa` is defined in `package-user-dir`, I
;; don't want to modify it, either.
;;
;; The last non-nil argument makes `make-directory` silience if dir exists.
(make-directory (locate-user-emacs-file ".local/") t)
;; Cache dir which contains package caches that you can safely remove it.
(make-directory (locate-user-emacs-file ".local/cache/") t)
;; Backup dir which contains backup files and auto save lists.
(make-directory (locate-user-emacs-file ".local/backup/") t)

;; Fonts.

;; (setq font-use-system-font t)

;; Set default font.
;;
;; `fill-column-indicator` and `highlight-indent-guide` uses box-drawing
;; characters to draw bars, but the default characters in Monaco is not so good,
;; it has padding before and after it. To fix this I used my patched Monaco
;; which merges Menlo's box-drawing characters into it.
(set-face-attribute 'default nil
                    :family "Monaco"
                    ;; :slant 'normal
                    ;; :width 'normal
                    ;; :weight 'normal
                    ;; 1 height is 1/10 pt.
                    :height 140)

;; Set fallback fonts, like 中文 or 😸.
;;
;; Emacs does not handle fonts fallback with FontConfig directly, instead it has
;; something called `fontset`. While there are more then one `fontset`s, we only
;; need to handle `fontset-default` mostly.
;;
;; You can modify `fontset` by `set-fontset-font`, but it's not a simple list of
;; fonts, it contains different lists for different charsets. If you don't give
;; a charset, you are changing the default charset, however, it's not for every
;; chars, but only for chars that cannot be found in other charsets. So at least
;; we have to modify all charsets we need.
;;
;; See <https://archive.casouri.cc/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/>.
;;
;; Don't set size here, otherwise when scaling Chinese won't scale.
;;
;; NOTE: Noto Sans CJK has no italic version, so if you cannot see CJK in
;; italic, it's not a bug. Maybe Sarasa Fixed has a generated italic version.
;;
;; See <https://github.com/notofonts/noto-fonts/issues/1466#issuecomment-469384694>.
(defun alynx/clear-and-set-fallback-fonts ()
  "Clear fontset and set fallback fonts."
  (dolist (charset '(han kana hangul symbol cjk-misc bopomofo))
    ;; FIXME: Not sure why Emacs prefers Sarasa Fixed, even I add Noto Sans Mono
    ;; CJK SC before it, I have to clear the list before adding fonts.
    (set-fontset-font t charset nil)
    ;; Prepend to the beginning of charset font lists.
    (set-fontset-font t charset
                      (font-spec :family "Noto Color Emoji") nil 'prepend)
    (set-fontset-font t charset
                      (font-spec :family "Noto Sans Mono CJK SC") nil 'prepend)
    (set-fontset-font t charset
                      (font-spec :family "Monaco") nil 'prepend)))

;; HACK: Set fallback fonts directly does not work for `emacsclient`. When
;; client starts a daemon, it firstly does not have GUI and those font related
;; setups are ignored. So we need to call this after frame created.
;;
;; See <https://www.reddit.com/r/emacs/comments/3a5kim/comment/cs9i9qd/?utm_source=share&utm_medium=web2x&context=3>.
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (alynx/clear-and-set-fallback-fonts)))
  (alynx/clear-and-set-fallback-fonts))

;; Noto Sans CJK fonts has larger ascent and descent, which make lines with CJK
;; chars higher than others. So make Monaco and Noto Sans Mono CJK SC the same
;; line height.
;;
;; This is not perfect, since font size is always integer, after rounding it
;; makes Noto Sans Mono CJK SC a little bit small.
;;
;; Better way is to custom Monaco's ascent and descent in its OS/2 table to make
;; it have the same ratio as Noto Sans Mono CJK SC. But it will break
;; box-drawing characters, which needs to be stretched.
;;
;; If Emacs allows user to set a custom min line height, this might be solved.
(setq face-font-rescale-alist '(("Noto Sans Mono CJK SC" . 0.85)))

;; Don't clean font-caches during GC.
(setq inhibit-compacting-font-caches t)

;; Indentations.

;; A total resolution to set different indent offset to different modes, and
;; automatically guess it from file content.

;; Making tab other length than 8 sounds like define PI to 3, if you don't want
;; 8, you should also not use tabs, but use spaces.
(setq-default tab-width 8)

;; This is the default value, which means if major mode does not set those
;; value, I'll use tabs and it's length should be 8 chars.
;;
;; `indent-tabs-mode` does not mean use tabs only, it means if the indent level
;; can be divided by `tab-width`, use tabs, and use spaces for the remaining.
(setq-default indent-tabs-mode t)

;; Based on `editorconfig-indentation-alist` and `doom-modeline-indent-alist`.
;;
;; See <https://github.com/editorconfig/editorconfig-emacs/blob/master/editorconfig.el#L175>.
;;
;; See <https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-core.el#L282>.
(defconst alynx/mode-indent-offsets
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c-mode c-basic-offset)
    (c-ts-mode c-ts-mode-indent-offset)
    (c++-mode c-basic-offset)
    (c++-ts-mode c-ts-mode-indent-offset)
    (cmake-mode cmake-tab-width)
    (cmake-ts-mode cmake-ts-mode-indent-offset)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
    (css-ts-mode css-indent-offset)
    (d-mode c-basic-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (enh-ruby-mode enh-ruby-indent-level)
    (erlang-mode erlang-indent-level)
    (ess-mode ess-indent-offset)
    (f90-mode f90-associate-indent
              f90-continuation-indent
              f90-critical-indent
              f90-do-indent
              f90-if-indent
              f90-program-indent
              f90-type-indent)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (groovy-mode groovy-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (haxor-mode haxor-tab-width)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js-ts-mode js-indent-level)
    (js-jsx-mode js-indent-level sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (json-ts-mode json-ts-mode-indent-offset)
    (julia-mode julia-indent-offset)
    (kotlin-mode kotlin-tab-width)
    (latex-mode tex-indent-basic)
    (lisp-mode lisp-indent-offset)
    (lisp-interaction-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (matlab-mode matlab-indent-level)
    (meson-mode meson-indent-basic)
    (mips-mode mips-tab-width)
    (mustache-mode mustache-basic-offset)
    (nasm-mode nasm-basic-offset)
    (nginx-mode nginx-indent-level)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    (perl-mode perl-indent-level)
    (php-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent-offset)
    (python-ts-mode python-indent-offset)
    (rjsx-mode js-indent-level sgml-basic-offset)
    (ruby-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (rustic-mode rustic-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (slim-mode slim-indent-offset)
    (sml-mode sml-indent-level)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (terra-mode terra-indent-level)
    (typescript-mode typescript-indent-level)
    (typescript-ts-base-mode typescript-ts-mode-indent-offset)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode web-mode-attr-indent-offset
              web-mode-attr-value-indent-offset
              web-mode-code-indent-offset
              web-mode-css-indent-offset
              web-mode-markup-indent-offset
              web-mode-sql-indent-offset
              web-mode-block-padding
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset))
  "Indent offset variables of different major modes.
If a major mode has different indent offset variables, all of them will be set
in order.")

(defun alynx/get-mode-indent-offsets ()
  "Get indent offset variables of current major mode."
  (cdr (assoc major-mode alynx/mode-indent-offsets)))

;; If you quote a list, not only itself, but it's elements won't be evaluated,
;; either, `interactive` wants a list of integers to fill arguments, so we
;; can't quote here, because we need to evaluate `read-number`.
;;
;; Using `list` will evaluate elements and return a list, using `\`` with `,`
;; can quote but evaluate selected elements.
(defun set-indent-offset (num)
  "Set indent offset to NUM chars.
If NUM is negative, indent offset will be nil."
  (interactive `(,(read-number "Indent offset (chars): ")))
  (dolist (mode-indent-offset (alynx/get-mode-indent-offsets))
    ;; `setq` sets buffer-local value automatically, but we need to do this
    ;; manually for `set`.
    (make-local-variable mode-indent-offset)
    (if (< num 0)
        (set mode-indent-offset nil)
      (set mode-indent-offset num))))

(defun indent-tabs (num)
  "Mark this buffer to indent with tabs and set indent offset to NUM chars.
If NUM is negative, indent offset will be nil."
  (interactive `(,(read-number "Indent offset (chars): ")))
  (indent-tabs-mode 1)
  (set-indent-offset num))

(defun indent-spaces (num)
  "Mark this buffer to indent with spaces and set indent offset to NUM chars.
If NUM is negative, indent offset will be nil."
  (interactive `(,(read-number "Indent offset (chars): ")))
  (indent-tabs-mode -1)
  (set-indent-offset num))

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
  (interactive `(,(read-number "Tab width (chars): " tab-width)))
  (when (/= tab-width num)
    (setq tab-width num)))

(defconst alynx/skip-guess-indent-modes '(lisp-mode
                                          emacs-lisp-mode
                                          lisp-interaction-mode)
  "Don't guess indent for those major modes.")

;; This function is not perfect, it is based on that "if the file is indented
;; with spaces, lines are never started with tabs, and the shortest space prefix
;; length except 0 or 1 is the indent offset".
;;
;; For example, projects like GTK uses 2 spaces per indent level, we will have
;; following lines:
;; 	1. 0 space prefix (preprocessors).
;; 	2. 1 space prefix (comment content of the first level comment blocks).
;; 	3. 2 spaces prefix (1 indent level, indent offset we want).
;; 	4. 4 spaces prefix (2 indent levels).
;; 	5. 6 spaces prefix (3 indent levels).
;; 	6. 1 tab prefix (4 indent levels, use tab).
;; 	7. 1 tab and 2 spaces prefix (5 indent levels).
;; 	(more...)
;;
;; This function cannot handle such condition, that first you use tab to indent,
;; and you have the following code:
;;
;; ```c
;; int a(int b,
;;       int c);
;; ```
;;
;; Here you use 5 spaces to align arguments, and this function will use 5 spaces
;; per indent level. A proper but impossible solution is ignore function header,
;; but obviously editors cannot understand every languages. Maybe we can also
;; record space or tab lines and do some comparation, but that's not exact.
;;
;; Anyway, this function is just guessing indentation, so user should correct it
;; manually if it returns wrong value.
(defun guess-indent ()
  "Guess and set indent-offset and tab/space for current buffer."
  (interactive)
  (if (member major-mode alynx/skip-guess-indent-modes)
      (message (format "Skip guess-indent for %s." (symbol-name major-mode)))
    ;; Ideally we should iterate the whole buffer, but that's impossible for big
    ;; files. Let's assume we always have 1-level indented code in the first 200
    ;; lines.
    (let* ((total-lines (count-lines (point-min) (point-max)))
           (detect-lines (min total-lines 200))
           (shortest-spaces 0)
           (has-tab nil))
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (dotimes (i detect-lines)
            (let (current-char)
              (goto-char (line-beginning-position))
              (setq current-char (char-after))
              (cond
               ;; We assume no one uses indent offset which is larger than 1 tab
               ;; width, so there won't be 1 tab plus 2 spaces as 1 level. We
               ;; also assume there is no line that contains only tabs.
               ((= current-char ?\t) (setq has-tab t))
               ((= current-char ?\s)
                ;; Calculate how many spaces in prefix.
                (let ((spaces 0))
                  (while (= current-char ?\s)
                    (setq spaces (+ spaces 1))
                    (forward-char 1)
                    (setq current-char (char-after)))
                  ;; (message "Line %d has %d spaces." i spaces)
                  ;; You may have lines only contains spaces in a file that
                  ;; indented with tabs. Just ignore those lines.
                  ;;
                  ;; Let's assume no one uses 1-char indent offset.
                  (when (and (/= current-char ?\n)
                             (/= current-char ?\r)
                             (/= spaces 0)
                             (/= spaces 1)
                             (or (= shortest-spaces 0)
                                 (< spaces shortest-spaces)))
                    (setq shortest-spaces spaces)))))
              (forward-line 1))
            i)))
      ;; If a file uses tabs to indent, and we cannot get shortest spaces, this
      ;; file is likely to only use tabs, and the indent offset is tab width,
      ;; otherwise the shortest spaces length is indent offset.
      ;; If we don't find tabs, just assume this file does not use tabs, this is
      ;; not correct because we may just not have tabs in first 200 lines but
      ;; that is the best effort we can do. And then if we cannot get shortest
      ;; spaces, it means that there is no indent level in first 200 lines, so
      ;; just skip it.
      (if has-tab
          (if (/= shortest-spaces 0)
              (indent-tabs shortest-spaces)
            (indent-tabs tab-width))
        (when (/= shortest-spaces 0)
          (indent-spaces shortest-spaces))))))

;; If installed more modes, add them here as
;; `(mode-name tab/space indent-offset)`.
(defconst alynx/modes-default-indent '((prog-mode tab 8)
                                       (c-mode tab 8)
                                       (c-ts-mode tab 8)
                                       (java-mode tab 8)
                                       (markdown-mode tab 8)
                                       (gfm-mode tab 8)
                                       ;; By default `lisp-indent-offset` is
                                       ;; `nil`, which works better.
                                       (lisp-mode space -1)
                                       (emacs-lisp-mode space -1)
                                       (lisp-interaction-mode space -1)
                                       (js-mode space 2)
                                       (js2-mode space 2)
                                       (json-ts-mode space 2)
                                       (css-mode space 2)
                                       (html-mode space 2)
                                       (nxml-mode space 2)
                                       (web-mode space 2)
                                       (yaml-mode space 2)
                                       ;; `yaml-ts-mode` is a `text-mode`, it
                                       ;; has no indent offset variable and tab
                                       ;; is used to align, so this just let it
                                       ;; use spaces.
                                       (yaml-ts-mode space -1)
                                       (meson-mode space 2)
                                       (lua-mode space 3)
                                       (python-mode space 4))
  "Default indentation for different modes.")

;; Instead of add hook to different mode, we just use
;; `change-major-mode-after-body-hook`, Emacs will run this hook first when
;; changing major mode, then our function is called to set default value, then
;; we may guess indent from the buffer, and then project's `.editorconfig` is
;; loaded, and then our custom `.dir-local.el` is loaded, then
;; `after-change-major-mode-hook` is called.
;;
;; See <https://github.com/editorconfig/editorconfig-emacs/issues/141>.
;;
;; `add-hook` by default adds function to the beginning of hook, so add
;; `guess-indent` first and it will run last.
(add-hook 'change-major-mode-after-body-hook 'guess-indent)

(add-hook 'change-major-mode-after-body-hook
          (lambda ()
            ;; Check which list our current `major-mode` is inside.
            (let* ((pair (assq major-mode alynx/modes-default-indent))
                   (mode (nth 1 pair))
                   (indent-offset (nth 2 pair)))
              (cond
               ((eq mode 'tab) (indent-tabs indent-offset))
               ((eq mode 'space) (indent-spaces indent-offset))))))

;; Keybindings.

;; Keybindings for setting indentations.
(global-set-key (kbd "C-c i TAB") 'indent-tabs)
(global-set-key (kbd "C-c i SPC") 'indent-spaces)
(global-set-key (kbd "C-c i o") 'set-indent-offset)
(global-set-key (kbd "C-c i w") 'set-tab-width)
(global-set-key (kbd "C-c i g") 'guess-indent)
;; Atom style indent left or right.
;;
;; TODO: Currently they will indent by a `tab-width`, I want to modify them to
;; use `indent-offset`.
;;
;; See <https://dougie.io/emacs/indent-selection/>.
(global-set-key (kbd "M-[") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-]") 'indent-rigidly-right-to-tab-stop)

;; I use this in Atom, but by default `M-;` is used in Emacs, so I may use this
;; keybinding for others in future.
(global-set-key (kbd "C-;") 'comment-dwim)

;; See <https://www.gnu.org/software/emacs/manual/html_node/efaq/Backspace-invokes-help.html>.
;;
;; A long story: Old terminals make Backspace generate the same code as `C-h`,
;; and make Delete generate `DEL` code. Emacs binds backward delete to `DEL`
;; code and help prefix to `C-h` (Maybe that's why by default HHKB has a Delete
;; key in Backspace's place).
;;
;; But in GUI, Emacs makes Backspace generate `DEL` code, and make Delete
;; generate another code which is bound to forward delete. Also GNOME Terminal
;; makes Backspace generate `DEL` code by default, too.
;;
;; Another problem is that GNOME Terminal makes Delete generate escape sequence
;; by default, Emacs cannot handle it. But there are also conflict with other
;; keybindings, so I just suggest not to use Emacs in terminal.
;;
;; It's not good enough to translate Delete to `DEL` code and bind Backspace to
;; functions like `backward-delete-char`, because many backward delete related
;; functions are actually bound to `DEL` code.
;;
;; So what should do is to make `C-h` generate `DEL` code so it has the same
;; behavior as Backspace (like `C-i` for `TAB`), and don't forget to make `M-h`
;; the same as `M-DEL`.
;;
;; See <https://www.emacswiki.org/emacs/BackspaceKey>.
;;
;; I am not using `keyboard-translate` here, since it only accept 1 char (or a
;; key code), `M-DEL` does not generate a single key code, but a sequence.
;;
;; I use `kbd` because it's easy to read, char constant or vector is also OK.
;;
;; See <https://ftp.gnu.org/old-gnu/Manuals/emacs-20.7/html_node/emacs_451.html>.
;;
;; Since we make `C-h` generate `DEL`, it should not be the default help prefix
;; now, by default we have another help perfix key `<f1>`, just unset it so
;; `<f1>` will be used as default help prefix.
(global-unset-key (kbd "C-h"))
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "M-h") (kbd "M-DEL"))

;; By default, `join-line` will join current line into previous line. But in
;; Atom, I typically join next line into current line.
;;
;; See <https://emacsredux.com/blog/2013/05/30/joining-lines/>.
(defun join-next-line ()
  "Join the current line with the next line."
  (interactive)
  (join-line 1))

;; `C-j` is used to insert evaluated value in `lisp-interaction-mode`, maybe I
;; should find another keybinding for this.
(global-set-key (kbd "C-j") 'join-next-line)

;; See <https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>.
;;
;; Maybe `mwim` package is better? But do I really to move between code and
;; trailing comment?
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

;; I found that vim style line insert commands helpful.
(defun open-next-line ()
  "Insert an empty line below the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun open-previous-line ()
  "Insert an empty line above the current line."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)

;; Find sibling file, but do not select it. We need this because we want to show
;; the result in other window. Just replace `find-file` with
;; `find-file-noselect` in `find-sibling-file`.
(defun find-sibling-file-noselect (file)
  "Read a \"sibling\" file of FILE into a buffer and return the buffer."
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (unless find-sibling-rules
    (user-error "The `find-sibling-rules' variable has not been configured"))
  (let ((siblings (find-sibling-file-search (expand-file-name file)
                                            find-sibling-rules)))
    (cond
     ((null siblings)
      (user-error "Couldn't find any sibling files"))
     ((length= siblings 1)
      (find-file-noselect (car siblings)))
     (t
      (let ((relatives (mapcar (lambda (sibling)
                                 (file-relative-name
                                  sibling (file-name-directory file)))
                               siblings)))
        (find-file-noselect
         (completing-read (format-prompt "Find file" (car relatives))
                          relatives nil t nil nil (car relatives))))))))

;; Like `find-file-other-window`, but use `find-sibling-file-noselect`.
(defun find-sibling-file-other-window (file)
  "Visit a \"sibling\" file of FILE, in another window."
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (let ((value (find-sibling-file-noselect file)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-window (car value))
	  (mapc 'switch-to-buffer (cdr value))
	  value)
      (switch-to-buffer-other-window value))))

(global-set-key (kbd "C-c M-f") 'find-sibling-file-other-window)

(defun show-file-path ()
  "Show the full file path of current buffer in the minibuffer."
  (interactive)
  (message "%s" buffer-file-name))

(global-set-key (kbd "C-c s") 'show-file-path)

(defun edit-config ()
  "Open init.el to edit."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(global-set-key (kbd "C-,") 'edit-config)

;; I maybe use this as some custom keybindings' prefix, but currently I prefer
;; `C-c`, because `C-z` is hard to press.
(global-unset-key (kbd "C-z"))

;; Packages.

;; This is needed for first running because we will install `use-package`.
(require 'package)
;; Use TUNA mirrors for faster downloading.
(setq package-archives
      '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; Always prefer MELPA.
(setq package-archive-priorities
      '(("gnu" . 0)
        ("nongnu" . 0)
        ("melpa" . 100)))

;; About `package-initialize`, `package-activate-all` and `package-quickstart`:
;;
;; If I am not wrong, you should never call `package-initialize`, because since
;; Emacs 27.1, the old `package-initialize` is splitted into two parts:
;;
;;   - `package-initialize`: load all lisp files of all packages (expensive) and
;;     call `package-activate-all`.
;;   - `package-activate-all`: load autoloads for all packages (cheap).
;;
;; And what about `package-quickstart`? It just writes all autoloads in a single
;; file when you call `package-quickstart-refresh`, and if you have such a file,
;; `package-activate-all` will load it instead of many files. This reduces only
;; 0.1s for me, but maybe very helpful for HDD users.
;;
;; See <https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=6dfdf0c9e8e4aca77b148db8d009c862389c64d3>.
;;
;; So basically you only need `package-activate-all`, and autoloads will load
;; the actual packages when you call them. Any articles telling you write
;; `package-initialize` before Emacs 27.1 is wrong now.
;;
;; Also, Emacs now call `package-activate-all` between `early-init.el` and
;; `init.el`, so if you want to control package loading in `init.el` like me,
;; you need to set `package-enable-at-startup` to `nil` in `early-init.el`.
(setq package-quickstart t)
;; Put quick start cache into cache dir.
(setq package-quickstart-file
      (locate-user-emacs-file ".local/cache/package-quickstart.el"))
;; Cache will not be used until we call `package-quickstart-refresh` manually.
(unless (file-exists-p package-quickstart-file)
  (package-quickstart-refresh))
;; Make sure quick start cache is refreshed after operations in package menu,
;; for example upgrading packages.
;; See <https://www.manueluberti.eu/emacs/2021/03/08/package/>.
(advice-add 'package-menu-execute :after-while #'package-quickstart-refresh)

(package-activate-all)

;; Install `use-package`.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Not all variables supports `custom-theme-set-variables`, so just let
;; `use-package` use `custom-set-variables`. I really don't use custom file.
(setq use-package-use-theme nil)

;; Setting `use-package-always-ensure` to `t` will also set `:ensure t` for
;; built-in packages.
;;
;; However, you should never set `:ensure t` for built-in packages, it will mess
;; things up when using `package-activate-all` instead of `package-initialize`.
;;
;; See <https://github.com/jwiegley/use-package/issues/977>.
;;
;; Let `use-package` always ensure packages so we don't need add `:ensure t`
;; manually.
;; (setq use-package-always-ensure t)

;; Install and configure packages.

;; NOTE: If you have both `:bind` / `:mode` / `:interpreter` / `:commands` and
;; enabling global mode in `:config`, it will never work, because `use-package`
;; will lazy load packages with those keywords. Add `:demand t` to break lazy
;; loading for this.
;;
;; See <https://sh.alynx.one/posts/Emacs-Lazy-Loading-use-package/>.
;;
;; See <https://github.com/jwiegley/use-package#notes-about-lazy-loading>.
;;
;; `(use-package PACKAGE)` block by default loads a package at startup if no
;; lazy loading generated, but sometimes I only want to set some custom
;; variables for a package, but don't load it (for example, move tramp's cache
;; file, but don't load tramp at startup), just use `:defer t` to prevent
;; loading them, `:custom` will still work.
;;
;; If a package is not needed since startup and has no keyword to make
;; `use-package` auto load it, but is needed later, add `:defer 1` to load it
;; after 1 second.
;;
;; Difference between `:defer t` and `:defer 1`: `:defer 1` will load package
;; after 1 second, but `:defer t` does not load package, expects other code
;; triggers it.

;; Built-in packages.

;; There is actually no `emacs` package, but it is useful if you have some
;; config that does not belong to any package and you want to manage them with
;; `use-package`.
(use-package emacs
  :hook
  ((minibuffer-setup . cursor-intangible-mode))
  ;; Some files does not have a major mode, and I don't want to define a major
  ;; mode for it, so just use some `lambda` for them.
  ;;
  ;; NOTE: `auto-mode-alist` can only run a function for each regex, so just
  ;; add all needed settings here.
  :mode (("COMMIT_EDITMSG\\'" . (lambda ()
                                  (display-fill-column-indicator-mode 1)
                                  (setq display-fill-column-indicator-column 72)
                                  (setq show-trailing-whitespace t)))
         ;; It's a little bit strange that SUSE use 67 in changes files.
         ("\\.changes\\'" . (lambda ()
                              (display-fill-column-indicator-mode 1)
                              (setq display-fill-column-indicator-column 67)
                              (setq show-trailing-whitespace t)))
         ;; `osc` generate temp files for `vc`.
         ("\\.changes\\.vctmp\\." . (lambda ()
                                      (display-fill-column-indicator-mode 1)
                                      (setq display-fill-column-indicator-column 67)
                                      (setq show-trailing-whitespace t))))
  ;; Vertico requires those.
  :config
  ;; Contrary to what many Emacs users have in their configs, you only need this
  ;; to make UTF-8 the default coding system.
  (set-language-environment "UTF-8")
  (put 'narrow-to-region 'disabled nil)
  ;; Add prompt indicator to `completing-read-multiple`.
  ;;
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; (defun crm-indicator (args)
  ;;   (cons (format "[CRM%s] %s"
  ;;                 (replace-regexp-in-string
  ;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                  crm-separator)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  :custom
  ;; Disable startup buffer. It's OK but it's not beautiful and not helpful.
  (inhibit-startup-screen t)
  ;; (fancy-splash-image (locate-user-emacs-file "emacs-e.svg"))
  (initial-scratch-message ";; -*- lexical-binding: t; -*-\n;; Happy hacking here!\n")
  ;; Disable annoying bell.
  (ring-bell-function 'ignore)
  ;; (visible-bell nil)
  ;; `set-language-enviornment` sets `default-input-method`, which is unwanted.
  (default-input-method nil)
  ;; Use y or n instead of yes or no.
  (use-short-answers t)
  ;; Set cursor to underline.
  (cursor-type 'hbar)
  ;; Disable line spacing, it makes `highlight-indent-guides` wired.
  ;; (line-spacing nil)
  ;; FIXME: High CPU usage on scrolling.
  ;;
  ;; By default Emacs will jump a half screen if your cursor is out of screen,
  ;; this makes it behave like other editors, but sometimes it still jumps.
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  ;; Decrease scrolling CPU usage.
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (jit-lock-defer-time 0)
  (minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Enable recursive minibuffers.
  (enable-recursive-minibuffers t)
  ;; Move autosave files and list into backup dir.
  (auto-save-list-file-prefix (locate-user-emacs-file ".local/backup/.saves-"))
  ;; Prevent `.#` lock files, so we won't mess up project if crashed.
  (create-lockfiles nil)
  ;; Emacs GC is not so good now, let's see how many times we trigger it.
  (garbage-collection-messages t)
  ;; See <https://emacs-china.org/t/emacs/21053/14>.
  (process-adaptive-read-buffering nil)
  (fill-column 80)
  (display-raw-bytes-as-hex t)
  (sentence-end-double-space nil)
  ;; Don't show default indicators, they are useless and ugly.
  (fringe-indicator-alist nil))

(use-package server
  :defer t
  :custom
  ;; HACK: If this is `t`, server will try to raise new frames via calling
  ;; `gtk_window_present_with_time()`, which has some bugs like unsetting
  ;; maximized frames and still show window decorations in maximized state, just
  ;; disable it as a workaround. (Emacs itself never raises its frame, only
  ;; Emacs server does this, so I don't think it's necessary.)
  (server-raise-frame nil)
  (server-log nil))

;; List dirs first in dired.
(use-package ls-lisp
  :defer t
  :custom
  (ls-lisp-dirs-first t))

(use-package dired
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first"))

(use-package windmove
  :demand t
  :bind
  (("C-c w C-n" . windmove-down)
   ("C-c w C-p" . windmove-up)
   ("C-c w C-f" . windmove-right)
   ("C-c w C-b" . windmove-left))
  :config
  ;; `S-<arrow>` to move between windows (`S` means Shift).
  (windmove-default-keybindings))

(use-package comp
  :defer t
  :custom
  ;; Silence compiler warnings as they can be pretty disruptive.
  (native-comp-async-report-warnings-errors 'silent))

(use-package mouse
  :defer t
  :custom
  ;; Yank to cursor, not where mouse clicked.
  (mouse-yank-at-point t))

;; Disable menu bar, tool bar, scroll bar and cursor blink.

(use-package menu-bar
  :defer t
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :defer t
  :config
  (tool-bar-mode -1))

;; Scroll bar is too narrow to click, better to disable it.
(use-package scroll-bar
  :defer t
  :config
  (scroll-bar-mode -1))

(use-package frame
  :defer t
  :config
  (blink-cursor-mode -1))

(use-package window
  :bind (("C-c C-v" . scroll-other-window)
         ("C-c M-v" . scroll-other-window-down)
         ("C-c b" . switch-to-buffer-other-window)))

;; Display line number and column number of cursor in mode line.
(use-package simple
  :config
  ;; This only affects the built-in mode line.
  ;; (line-number-mode 1)
  ;; (column-number-mode 1)
  (kill-ring-deindent-mode 1)
  :custom
  ;; By default if you press backspace on indentations, Emacs will turn a tab
  ;; into spaces and delete one space, I think no one likes this.
  (backward-delete-char-untabify-method nil)
  (kill-do-not-save-duplicates t))

;; See <https://dougie.io/emacs/indentation/>.
;;
;; Auto-indent line when pressing enter.
(use-package electric
  :defer t
  :custom
  ;; Looks like `:custom` can handle `setq-default` correctly.
  (electric-indent-inhibit nil))

(use-package files
  :defer t
  :bind (("C-x M-f" . find-sibling-file)
         ("C-c C-f" . find-file-other-window))
  :custom
  (find-file-visit-truename t)
  (find-file-suppress-same-file-warnings nil)
  (find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")
                        ("\\([^/]+\\)\\.h\\'" "\\1.c")
                        ("\\([^/]+\\)\\.rej\\'" "\\1")))
  ;; Backup file is generated when you save file. Autosave file is generated
  ;; every few seconds or every few characters.
  ;;
  ;; See <https://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/>.
  ;;
  ;; Disable backup files.
  (make-backup-files nil)
  ;; Move backup files into backup dir.
  (backup-directory-alist
   `(("." . ,(locate-user-emacs-file ".local/backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(locate-user-emacs-file ".local/backup/") t))))

(use-package tramp-cache
  :defer t
  :custom
  ;; Move tramp cache into cache dir.
  (tramp-persistency-file-name (locate-user-emacs-file ".local/cache/tramp")))

(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'forward))

;; Typing chars will replace selection, this is the default behavior of most
;; editors.
(use-package delsel
  :config
  (delete-selection-mode 1))

;; Display line number (gutter) and highlight current line.

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode 1)
  :custom
  ;; Calculate max number to prevent shaking.
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package whitespace
  ;; Highlight trailing whitespace in `prog-mode` only.
  :hook ((prog-mode . (lambda () (setq show-trailing-whitespace t)))
         (nxml-mode . (lambda () (setq show-trailing-whitespace t)))
         (yaml-ts-mode . (lambda () (setq show-trailing-whitespace t)))))

;; Built-in minor mode to display column ruler.
(use-package display-fill-column-indicator
  :defer t
  ;; I only use this in `prog-mode`.
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (nxml-mode . display-fill-column-indicator-mode)
         (yaml-ts-mode . display-fill-column-indicator-mode))
  :custom
  ;; Set column ruler at 80 columns.
  (display-fill-column-indicator-column 80))

(use-package paren
  :defer t
  :custom
  ;; Using `t` for this is confusing.
  (show-paren-when-point-inside-paren nil))

;; Enable `pixel-scroll-precision-mode` added in Emacs 29.
(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode 1))

;; Always follow symlinks instead of asking, because the minibuffer prompt might
;; be covered by other messages.
(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t))

;; Better keybindings for replace, because `isearch-backward` is hardly used.
(use-package replace
  :bind (("C-r" . query-replace)
         ("C-M-r" . query-replace-regexp))
  :custom
  ;; No, don't do case insensitive replace.
  (case-fold-search nil))

;; Built-in minor mode to save recent files.
(use-package recentf
  ;; `ivy-use-virtual-buffers` and `consult-buffer` need it.
  :config
  (recentf-mode 1)
  :custom
  (recentf-save-file (locate-user-emacs-file ".local/recentf")))

;; Built-in minor mode to open files at last-edited position.
(use-package saveplace
  ;; Don't defer this if you want it to work on the first file you opened.
  :config
  (save-place-mode 1)
  :custom
  (save-place-file (locate-user-emacs-file ".local/places")))

(use-package savehist
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (savehist-mode 1)
  :custom
  (savehist-file (locate-user-emacs-file ".local/history")))

(use-package autorevert
  :config
  (global-auto-revert-mode 1))

(use-package profiler
  :bind (("C-c p b" . profiler-start)
         ("C-c p e" . profiler-stop)
         ("C-c p r" . profiler-report)))

;; See <https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter>.
;;
;; Building <https://github.com/casouri/tree-sitter-module/> is needed, because
;; the release is not up to date sometimes.
;;
;; Articles from the feature author are also helpful.
;;
;; See <https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/>.
;;
;; See <https://archive.casouri.cc/note/2023/tree-sitter-starter-guide/>.
(use-package treesit
  :config
  ;; Well, `c-ts-mode` not works well sometimes.
  ;; (add-to-list 'major-mode-remap-alist
  ;;              '(c-mode . c-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist
  ;;              '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(css-mode . css-ts-mode))
  :custom
  (treesit-extra-load-path `(,(locate-user-emacs-file ".local/treesit"))))

;; Save customization data in a specific file, without littering my init files.
(use-package cus-edit
  :config
  (unless (file-exists-p custom-file)
    (make-empty-file custom-file))
  (load-file custom-file)
  :custom
  (custom-file (locate-user-emacs-file "custom-file.el")))

;; Built-in shell written in Emacs Lisp. I hardly use this, but it has a dir.
(use-package eshell
  :defer t
  :custom
  ;; Redirect its data dir.
  (eshell-directory-name (locate-user-emacs-file ".local/eshell/")))

(use-package ibuffer
  ;; The default Buffer List is ugly, it is said no one uses it except RMS, so
  ;; replace it with IBuffer. However I don't use them either.
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-use-other-window t))

;; I prefer Linux coding style for C, not GNU.
(use-package cc-vars
  :defer t
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux"))))

(use-package cc-mode
  :defer t
  ;; I am a modern guy.
  :hook ((c-mode . (lambda () (setq comment-start "//"
                                    comment-end   "")))))

;; It turns out that if I enable `c-ts-mode` here it will automatically remap
;; all C files to itself, I don't want this.
;; (use-package c-ts-mode
;;   ;; I am a modern guy.
;;   :hook ((c-ts-mode . (lambda () (setq comment-start "//"
;;                                        comment-end   ""))))
;;   :custom
;;   (c-ts-mode-indent-style 'linux))

(use-package json-ts-mode
  :mode (("\\.bowerrc\\'" . json-ts-mode)
         ("\\.jshintrc\\'" . json-ts-mode)
         ("\\.json_schema\\'" . json-ts-mode)
         ("\\.json\\'" . json-ts-mode)))

(use-package yaml-ts-mode
  :mode (("\\.yml\\'" . yaml-ts-mode) ("\\.yaml\\'" . yaml-ts-mode)))

(use-package rust-ts-mode
  :mode (("\\.rs\\'" . rust-ts-mode)))

;; External modes and tools for different languages.

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

(use-package xref-js2
  :ensure t
  :hook ((js2-mode . (lambda ()
                      (add-hook 'xref-backend-functions
                                #'xref-js2-xref-backend nil t))))
  :bind (:map js-mode-map ("M-." . nil))
  :custom (xref-js2-search-program 'rg))

;; Emacs now has internal `json-ts-mode`, should be enough for me.
;; (use-package json-mode
;;   :ensure t
;;   :disabled t
;;   :bind (:map json-mode-map ("C-c <tab>" . json-mode-beautify))
;;   :mode (("\\.bowerrc\\'" . json-mode)
;;          ("\\.jshintrc\\'" . json-mode)
;;          ("\\.json_schema\\'" . json-mode)
;;          ("\\.json\\'" . json-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ;; DaVinci Resolve's fuse scripts, it uses lua.
         ("\\.fuse\\'" . lua-mode)))

(use-package markdown-mode
  :ensure t
  ;; Don't cover my `move-text` keybindings! They are more useful.
  :bind (:map gfm-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  ;; It changes `tab-width` to 4 by default, which is bad.
  :hook ((gfm-mode . (lambda () (set-tab-width 8))))
  :custom
  (markdown-command "marked")
  (markdown-gfm-use-electric-backquote nil)
  (markdown-indent-on-enter nil))

(use-package web-mode
  :ensure t
  :mode (("\\.njk\\'" . web-mode)
         ("\\.j2\\'" . web-mode)
         ;; `web-mode` can highlight JavaScript and CSS inside HTML.
         ("\\.html\\'" . web-mode))
  :custom
  ;; I prefer not to indent control blocks of templating.
  (web-mode-enable-control-block-indentation nil))

;; Emacs now has internal `yaml-ts-mode`, should be enough for me.
;; (use-package yaml-mode
;;   :ensure t
;;   :disabled t
;;   :mode (("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode)))

(use-package meson-mode
  :ensure t
  :mode (("meson\\.build\\'" . meson-mode)
         ("meson\\.options\\'" . meson-mode)
         ("meson_options\\.txt\\'" . meson-mode)))

(use-package cmake-ts-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode)
         ("\\.cmake\\'" . cmake-ts-mode)))

;; See <https://github.com/stigbjorlykke/rpm-spec-mode/issues/16>.
;;
;; Currently this package does not work with Emacs after 28.1, and the repo
;; seems not actively maintained, so I manually bundle it, and apply the patch
;; from Fedora.
;;
;; See <https://src.fedoraproject.org/rpms/emacs-rpm-spec-mode/blob/rawhide/f/fix-define-obsolete-variable-alias.patch>.
(use-package rpm-spec-mode
  ;; Don't ensure it, the MELPA one is old.
  :mode (("\\.spec\\'" . rpm-spec-mode)))

;; Simple packages that have no dependencies.

;; I hardly try.
;; (use-package try
;;   :ensure t
;;   :defer 1)

;; My favorite themes with my own tweaks.
;;
;; Don't defer themes, I need them all time.

(use-package alynx-one-dark-theme
  :config
  (load-theme 'alynx-one-dark t))

;; Or sometimes I want light theme.
(use-package alynx-one-light-theme
  ;; What we bind here is not from our theme, so the theme is loaded manually.
  :demand t
  :config
  (defun alynx/toggle-theme ()
    "Toggle between dark and light variant themes and redisplay everything."
    (interactive)
    (toggle-theme)
    ;; Some modes like `svg-tag-mode` uses color from theme, reset them.
    (font-lock-flush))
  :bind (("C-c t t" . alynx/toggle-theme)))

;; Or if you like `nano-theme`.
;; (use-package nano-theme
;;   :ensure t
;;   :disabled t
;;   :config
;;   (load-theme 'nano-dark t))

;; Finally I decide to write my own mode line so I can modify it easily.
(use-package alynx-mode-line
  ;; Well, Emacs does not generate and call autoloads for local library, and
  ;; `flycheck` will be unhappy because the function is not declared. We could
  ;; use `:commands` to let `use-package` generate and call autoloads here, but
  ;; it will also defer the package and `:demand t` is needed then. Actually I
  ;; don't really need autoloads for this, I just need to declare it, this could
  ;; be done via `:functions`.
  ;; :demand t
  ;; :commands (alynx-mode-line-mode)
  :functions (alynx-mode-line-mode)
  :config
  (alynx-mode-line-mode 1)
  :custom
  (alynx-mode-line-glyph-type 'ascii))

;; (use-package nano-modeline
;;   :ensure t
;;   :disabled t
;;   :config
;;   (nano-modeline-mode 1)
;;   :custom
;;   (nano-modeline-position 'bottom))

(use-package olivetti
  :ensure t
  :bind (("C-c o" . olivetti-mode))
  ;; Just disable eye-attracting things in olivetti mode.
  :hook ((olivetti-mode . (lambda () (if olivetti-mode
                                         (progn
                                           (display-line-numbers-mode -1)
                                           (display-fill-column-indicator-mode -1)
                                           ;; (git-gutter-mode -1)
                                           (diff-hl-mode -1))
                                       (display-line-numbers-mode 1)
                                       (display-fill-column-indicator-mode 1)
                                       ;; (git-gutter-mode 1)
                                       (diff-hl-mode 1))))))

;; Atom-like move regine / current line up and down.
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))

;; If there is no region, behave like current line is current region like Atom.
;;
;; Works on indent / outdent, comment block, cut (kill), copy, paste (yank).
(use-package whole-line-or-region
  :ensure t
  :demand t
  ;; See <https://github.com/purcell/whole-line-or-region/commit/ba193b2034388bbc384cb04093150fca56f7e262>.
  :bind (:map whole-line-or-region-local-mode-map
              ([remap indent-rigidly-left-to-tab-stop] . whole-line-or-region-indent-rigidly-left-to-tab-stop)
              ([remap indent-rigidly-right-to-tab-stop] . whole-line-or-region-indent-rigidly-right-to-tab-stop))
  :config
  (whole-line-or-region-global-mode 1))

;; Redo like most editors.
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :custom
  ;; Why you guys always generate garbage in project dir by default?
  (undo-tree-history-directory-alist
        `(("." . ,(locate-user-emacs-file ".local/undo-tree/")))))

;; FIXME: High CPU usage on scrolling.
(use-package highlight-indent-guides
  :ensure t
  :defer t
  ;; I only use this in `prog-mode`.
  :hook ((prog-mode . highlight-indent-guides-mode)
         (nxml-mode . highlight-indent-guides-mode)
         (rpm-spec-mode . highlight-indent-guides-mode)
         ;; `yaml-mode` should be `prog-mode`, anyway.
         (yaml-ts-mode . highlight-indent-guides-mode))
  :custom
  ;; Highlight indent guides could generate font faces via background
  ;; automatically, but it's too dark. I have color in themes so disable it.
  (highlight-indent-guides-auto-enabled nil)
  ;; Bitmap is modern, but has some issues (wrong color after switching theme,
  ;; wrong size after changing font size), and character is faster. A problem of
  ;; character is that it shows in default face in candidates of `consult-line`,
  ;; I have some hacks for it in `consult`.
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?│)
  ;; (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-bitmap-function
   'highlight-indent-guides--bitmap-line))

;; Currently not working well with PGTK due to a bug. But this is a new thing,
;; it uses stipple face properties and should be fast.
;;
;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=64969>.
;;
;; The author says it does not support `indent-tabs-mode`, I feel depressed, why
;; we cannot have a fast indent guide that supports tabs? Every editor has one,
;; is it so hard???
;;
;; See <https://github.com/jdtsmith/indent-bars/commit/accb7d00d2dd86944909b81dc45391813106ca74>.
(use-package indent-bars
  :load-path "site-lisp/indent-bars"
  :defer t
  :disabled t
  ;; I only use this in `prog-mode`.
  :hook
  ;; ((server-after-make-frame . (lambda ()
  ;;                               (add-hook 'prog-mode-hook  'indent-bars-mode)
  ;;                               (add-hook 'nxml-mode-hook  'indent-bars-mode)
  ;;                               (add-hook 'rpm-spec-mode-hook  'indent-bars-mode)
  ;;                               (add-hook 'yaml-ts-mode-hook  'indent-bars-mode))))
  ;; See <https://github.com/jdtsmith/indent-bars/issues/6>.
  ;;
  ;; Currently hard to use with both daemon and normal Emacs.
  ((prog-mode . indent-bars-mode)
   (nxml-mode . indent-bars-mode)
   (rpm-spec-mode . indent-bars-mode)
   ;; `yaml-mode` should be `prog-mode`, anyway.
   (yaml-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-pattern ".")
  ;; (indent-bars-color 'shadow)
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth nil))

;; Highlight FIXME, TODO, NOTE or HACK.
;; `svg-tag-mode` could also achieve this, it looks fancy, but has some bugs.
(use-package hl-todo
  :ensure t
  :defer t
  :hook ((prog-mode . hl-todo-mode)
         (nxml-mode . hl-todo-mode)
         (rpm-spec-mode . hl-todo-mode)
         (yaml-ts-mode . hl-todo-mode)))

;; `diff-hl` supports more VCS than `git-gutter` and `git-gutter-fringe`.
(use-package diff-hl
  :ensure t
  :config
  (define-fringe-bitmap 'alynx/diff-hl-bmp-insert
    [#b11100000]
    nil nil '(center repeated))
  (define-fringe-bitmap 'alynx/diff-hl-bmp-insert-hi-res
    [#b1111110000000000]
    nil nil '(center repeated))
  (define-fringe-bitmap 'alynx/diff-hl-bmp-change
    [#b11100000]
    nil nil '(center repeated))
  (define-fringe-bitmap 'alynx/diff-hl-bmp-change-hi-res
    [#b1111110000000000]
    nil nil '(center repeated))
  (define-fringe-bitmap 'alynx/diff-hl-bmp-delete
    [#b11110000
     #b11100000
     #b11000000
     #b10000000]
    nil nil 'top)
  (define-fringe-bitmap 'alynx/diff-hl-bmp-delete-hi-res
    [#b1111111100000000
     #b1111111000000000
     #b1111110000000000
     #b1111100000000000
     #b1111000000000000
     #b1110000000000000
     #b1100000000000000
     #b1000000000000000]
    nil nil 'top)
  ;; Don't use `diff-hl`'s background, it's ugly.
  ;;
  ;; I altered those faces in my themes instead of here.
  ;; (set-face-background 'diff-hl-insert nil)
  ;; (set-face-background 'diff-hl-delete nil)
  ;; (set-face-background 'diff-hl-change nil)
  (global-diff-hl-mode 1)
  :custom
  ;; Like `flycheck`, we choose bigger icons if needed.
  (diff-hl-fringe-bmp-function (lambda (type pos)
                                 (let* ((fringe-width
                                         (pcase diff-hl-side
                                           (`left (car (window-fringes)))
                                           (`right (cadr (window-fringes)))))
                                        (hi-res (>= fringe-width 16)))
                                   (intern (format
                                            (if hi-res
                                                "alynx/diff-hl-bmp-%s-hi-res"
                                              "alynx/diff-hl-bmp-%s")
                                            type)))))
  (diff-hl-draw-borders nil))

(use-package svg-lib
  :ensure t
  :custom
  (svg-lib-icons-dir (locate-user-emacs-file ".local/svg-lib/")))

;; I don't enable this, because it makes part of syntax highlighting disappears,
;; and has bug with `emacsclient`.
;;
;; See <https://github.com/rougier/svg-lib/issues/18>.
(use-package svg-tag-mode
  :ensure t
  :functions (global-svg-tag-mode svg-tag-mode)
  ;; A workaround to delay style calculating.
  ;; :init
  ;; (defun first-graphical-frame-hook-function ()
  ;;   (remove-hook 'focus-in-hook #'first-graphical-frame-hook-function)
  ;;   (provide 'my-gui))
  ;; (add-hook 'focus-in-hook #'first-graphical-frame-hook-function)

  ;; (with-eval-after-load 'my-gui
  ;;   (setq svg-lib-style-default (svg-lib-style-compute-default)))
  ;; Maybe use with hooks.
  :config
  ;; Make byte-compiler happy (for flycheck) until `:functions` really works.
  (declare-function global-svg-tag-mode "svg-tag-mode" (&optional arg))
  (declare-function svg-tag-mode "svg-tag-mode" (&optional arg))
  ;; (global-svg-tag-mode 1)
  :custom
  ;; See <https://github.com/rougier/svg-tag-mode#usage-example>.
  (svg-tag-tags
   '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                            (svg-tag-make tag :beg 1 :end -1))))
     ("TODO:" . ((lambda (tag)
                   (svg-tag-make tag
                                 :beg 0 :end -1 :inverse t :face 'warning))))
     ("FIXME:" . ((lambda (tag)
                    (svg-tag-make tag :beg 0 :end -1 :inverse t :face 'error))))
     ("NOTE:" . ((lambda (tag)
                   (svg-tag-make tag :beg 0 :end -1 :inverse t))))
     ("HACK:" . ((lambda (tag)
                   (svg-tag-make tag
                                 :beg 0 :end -1 :inverse t :face 'success))))
     ("XXX:" . ((lambda (tag)
                  (svg-tag-make tag :beg 0 :end -1)))))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package rainbow-mode
  :ensure t
  ;; Defining our functions that calls package functions in `:preface` is a
  ;; solution to make byte-compiler happy, but we still needs `:functions` to
  ;; work when we call functions from packages in `:config` directly.
  ;;
  ;; See <https://github.com/jwiegley/use-package/issues/1032#issuecomment-1397951772>
  ;; :functions (rainbow-x-color-luminance)
  :hook ((prog-mode . rainbow-mode)
         (nxml-mode . rainbow-mode)
         (rpm-spec-mode . rainbow-mode)
         (yaml-ts-mode . rainbow-mode))
  :config
  ;; FIXME: To make `flycheck` happy, before we fix `:functions`.
  (declare-function rainbow-x-color-luminance "rainbow-mode" (color))
  ;; Using overlay so it has higher priority than `hl-line`. After Emacs 29 we
  ;; have fantastic optimizations about overlay so it won't be a problem.
  (defun alynx/rainbow-colorize-with-overlay (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ov-rainbow t)
      (overlay-put ov 'face
                   `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                       "white"
                                     "black"))
                     (:background ,color)))))
  (advice-add 'rainbow-colorize-match :override
              'alynx/rainbow-colorize-with-overlay))

;; FIXME: High CPU usage on scrolling.
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :disabled t
;;   :defer t
;;   :hook ((prog-mode . rainbow-delimiters-mode)))

;; Not good, I only use minimap as scroll bar, but it cannot do this well.
;;
;; If use this don't forget to install block font.
;;
;; See <https://github.com/jandamm/doom-emacs-minimap/blob/master/blockfont.ttf>.
;; (use-package minimap
;;   :ensure t
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
  :ensure t
  :defer t
  :hook ((org-mode . org-bullets-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; I use this to control popup window position and behavior.
(use-package popper
  :ensure t
  ;; Prevent lazy loading so it can manage popup since Emacs starts.
  :demand t
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  ;; Some popup windows are focused, but others are not, let them all focused,
  ;; so I could easily close them.
  (popper-reference-buffers '("\\*Backtrace\\*"
                              "\\*IBuffer\\*"
                              "\\*Warnings\\*"
                              ;; This will shown when you click TAB on `M-:`.
                              "\\*Completions\\*"
                              "\\*Compile-Log\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode))
  ;; Don't add extra segment because it does not follow my segments.
  (popper-mode-line ""))

;; A sticky header, can stick function headers of different languages.
(use-package topsy
  :ensure t
  :hook ((prog-mode . topsy-mode)))

;; Not sure why it does not work for me. It always set to English even I
;; manually switched to RIME. I also want to use Chinese for `find-file` or
;; `consult-line`.
;; (use-package sis
;;   :ensure t
;;   :init
;;   (setq sis-respect-start nil)
;;   :config
;;   (sis-ism-lazyman-config "xkb:us::eng" "rime" 'ibus)
;;   (sis-global-respect-mode t))

;; Complex packages that have dependencies.
;;
;; Don't change the sequence, because the latter needs to obey the former's
;; `:custom`, for example `projectile` and `counsel-projectile`. If a package
;; depends on other package, it should be placed after all of its dependencies.
;;
;; I don't use `:after` here, because we may have keybindings for dependencies,
;; which leads into an autoload, and if you have 2 dependencies in `:after`,
;; you need to first press both keybindings of those 2 dependencies, then
;; configurations of the 2 packages will be run, it won't work before those
;; keybindings. I don't want this, I just want to run `:custom` of dependencies
;; first, so keep the sequence is the easiest.
;;
;; See <https://github.com/jwiegley/use-package/issues/976#issuecomment-1056017784>.

;; Vertico and Consult need this to behave like `ivy--regex-plus`.
(use-package orderless
  :ensure t
  ;; Orderless always returns too many unrelated results, especially for auto
  ;; completing, so only use it in minibuffer.
  ;;
  ;; See <https://emacs-china.org/t/orderless-completion/20455/3>.
  ;;
  ;; Well, that's not a problem after using `lsp-bridge`. `lsp-bridge` has its
  ;; own completion style so it won't be affected by orderless.
  ;; :hook ((minibuffer-setup . (lambda ()
  ;;                              (setq-local completion-styles '(orderless basic)
  ;;                                          completion-category-overrides '((file . ((styles . (partial-completion)))))))))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file . ((styles . (partial-completion))))))
  ;; Allow escape space with backslash.
  (orderless-component-separator 'orderless-escapable-split-on-space))

(use-package marginalia
  :ensure t
  ;; Vertico will load it so we don't load it manually.
  ;; :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-M-s" . consult-ripgrep)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap recentf-open] . consult-recent-file)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap imenu] . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; `M-s` bindings (`search-map`).
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration.
         :map isearch-mode-map
         ([remap isearch-edit-string] . consult-isearch-history)
         ;; Needed by `consult-line` to detect isearch.
         ("M-s l" . consult-line)
         ;; Needed by `consult-line` to detect isearch.
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; `M-s`.
         ([remap next-matching-history-element] . consult-history)
         ;; `M-r`.
         ([remap previous-matching-history-element] . consult-history)
         ;; ("M-g e" . consult-compile-error)
         ;; Alternative: `consult-flymake`.
         ("M-g f" . consult-flycheck)
         ;; Alternative: `consult-outline`.
         ;; ("M-g o" . consult-org-heading)
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ;; `C-c` bindings (`mode-specific-map`)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)
         ;; ("C-M-#" . consult-register)
         )
  ;; Enable automatic preview at point in the `*Completions*` buffer. This is
  ;; relevant when you use the default completion UI.
  :hook ((completion-list-mode . consult-preview-at-point-mode))
  :config
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Press `C-s` twice to search last item.
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-s") #'previous-history-element)
    map))
  (consult-customize consult-line :keymap my-consult-line-map)
  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register`, `consult-register-load`,
  ;; `consult-register-store` and the Emacs built-ins.
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the narrowing key.
  ;; Both `<` and `C-+` work reasonably well.
  (consult-narrow-key "<")
  ;; HACK: `consult-line` cannot handle font face of `highlight-indent-guides`
  ;; properly, and I don't really need syntax highlight in search candidates,
  ;; this just ignores all text properties, so it also hides indent guides.
  (consult-fontify-preserve nil))

;; This depends on `ace-window` and `hydra`, and I hardly use it.
;; (use-package treemacs
;;   :ensure t
;;   :disabled t
;;   :defer 1
;;   ;; I never use internal input method so bind this to `treemacs`.
;;   :bind (("C-\\" . treemacs))
;;   ;; Disable line number for `treemacs` window.
;;   :hook ((treemacs-mode . (lambda () (display-line-numbers-mode -1))))
;;   :custom
;;   ;; Default `treemacs` window is too wide.
;;   (treemacs-width 20)
;;   (treemacs-persist-file (locate-user-emacs-file ".local/treemacs-persist"))
;;   (treemacs-last-error-persist-file
;;    (locate-user-emacs-file ".local/treemacs-persist-at-last-error")))

;; Dependency of `flycheck-posframe` and `lsp-bridge`.
;; (use-package posframe
;;   :ensure t
;;   :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :hook ((prog-mode . flycheck-mode)
         (nxml-mode . flycheck-mode)
         (yaml-ts-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))
  :config
  ;; There is no hi-res version of `flycheck-fringe-bitmap-continuation`, on
  ;; normal screen the lower 8 bits are used.
  (define-fringe-bitmap 'flycheck-fringe-bitmap-continuation
    [#b0111000001110000
     #b0011100000111000
     #b0001110000011100
     #b0000111000001110]
    nil nil '(top repeated))
  ;; By default right double arrow is used, let's draw custom icon.
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [#b00000111]
    nil nil '(center repeated))
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-left-arrow-hi-res
    [#b0000000000111111]
    nil nil '(center repeated))
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;   [#b00000100
  ;;    #b00001100
  ;;    #b00011100
  ;;    #b00111100
  ;;    #b00111100
  ;;    #b00011100
  ;;    #b00001100
  ;;    #b00000100]
  ;;   nil nil 'center)
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-double-left-arrow-hi-res
  ;;   [#b0000000000000100
  ;;    #b0000000000011100
  ;;    #b0000000001111100
  ;;    #b0000000111111100
  ;;    #b0000011111111100
  ;;    #b0001111111111100
  ;;    #b0001111111111100
  ;;    #b0000011111111100
  ;;    #b0000000111111100
  ;;    #b0000000001111100
  ;;    #b0000000000011100
  ;;    #b0000000000000100]
  ;;   nil nil 'center)
  :custom
  ;; Currently `flycheck` is unable to run local `standardx` with `npx`.
  ;;
  ;; See <https://github.com/flycheck/flycheck/issues/1428>.
  (flycheck-javascript-standard-executable "/usr/bin/standardx")
  ;; Leave left fringe to VCS states.
  (flycheck-indication-mode 'right-fringe)
  ;; Prevent check on modifications, it makes Emacs laggy.
  (flycheck-check-syntax-automatically '(save mode-enabled)))

;; `lsp-bridge` also has error popups and looks better than it.
;; (use-package flycheck-posframe
;;   :ensure t
;;   :disabled t
;;   :hook ((flycheck-mode . flycheck-posframe-mode))
;;   :config
;;   (flycheck-posframe-configure-pretty-defaults))

;; Dependency of `lsp-bridge`.
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode))
  :config
  ;; OK, I finally give up, maybe I need to sync snippets so don't exclude them.
  (make-directory (locate-user-emacs-file "snippets/") t))

;; Dependency of `lsp-bridge`.
(use-package yasnippet-snippets
  :ensure t
  :defer t)

;; Looks like that I need to alter JSON files under `langserver` to add
;; parameters to `clangd`, but I can also modify `~/.config/clangd/config.yaml`
;; to let it find `compile_commands.json` under `build/`.
(use-package lsp-bridge
  ;; This is not in MELPA and installed as submodules.
  :load-path "site-lisp/lsp-bridge/"
  :defer t
  :hook ((prog-mode . lsp-bridge-mode)
         (nxml-mode . lsp-bridge-mode)
         (yaml-ts-mode . lsp-bridge-mode))
  :custom
  (lsp-bridge-enable-hover-diagnostic t)
  ;; See <https://github.com/manateelazycat/lsp-bridge/commit/46ff1693558901f7867d9e7ab7dba98efdecd21c>.
  ;;
  ;; lsp-bridge now use acm frame instead of posframe, which also fixed strange
  ;; black rectangle in wrong place for me.
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-signature-show-with-frame-position 'point)
  ;; It is fancy, but noisy.
  (acm-enable-doc nil)
  (acm-enable-tabnine nil))

;;; init.el ends here.
