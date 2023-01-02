;;; unused.el --- Unused config with interesting comments. -*- lexical-binding: t -*-

;;; Commentary:
;; Some config are dropped by some reason, but I have useful comments for them,
;; or maybe one day I'll copy some custom value from them, so I put them here.

;;; Code:

;; `doom-modeline` is good, but `mood-line` is enough for me.
;; Don't defer this because I need it since starting.
;; (use-package mood-line
;;   ;; :ensure t
;;   :load-path "~/Projects/mood-line.alynx/"
;;   :config
;;   (mood-line-mode 1)
;;   :custom
;;   (mood-line-show-indentation-style t)
;;   ;; Indent offset and tab width are different things.
;;   (mood-line-segment-indentation-always-show-offset t)
;;   (mood-line-show-eol-style t)
;;   (mood-line-show-encoding-information t)
;;   (mood-line-show-cursor-point t)
;;   ;; Fancy. But why Fira Code works better than unicode on my system?
;;   (mood-line-glyph-alist mood-line-glyphs-fira-code)
;;   ;; HACK: `mood-line` uses custom faces inherited from shadow, which changes
;;   ;; foreground color and breaks active/inactive switch, so reset them.
;;   :custom-face
;;   (mood-line-encoding ((t (:inherit nil))))
;;   (mood-line-unimportant ((t (:inherit nil)))))

;; The FUCKING EVIL SHITTY VSCode TypeScript language server does auto
;; formatting on indentation, which makes your code looks like a piece of
;; shit. And you have 4 options to solve this:
;; 1. Use a non-evil but silly server, like Eslint.
;; 2. Don't hook `lsp-mode` with `js-mode` and `js2-mode`.
;; 3. Just disable all indentation provided by `lsp-mode`.
;; (lsp-enable-indentation nil)
;; 4. Do you see those crazy codes? VSCode TypeScript language server's
;; formatting options can only be tweaked via
;; `workspace/didChangeConfiguration`.
;; See <https://github.com/typescript-language-server/typescript-language-server#workspacedidchangeconfiguration>.
;; Damn it, there is no properly exported way to set those shits for only one
;; server, unless you modify `lsp-mode` itself.
;; See <https://github.com/emacs-lsp/lsp-mode/issues/167>.
;; Just in case if you want to solve this problem by modify `lsp-mode`.
;; See <https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-javascript.el#L71>.
;; When using Emacs's `json-encode`, keyword arguments will become key in
;; `String` type, `t` will become `true`, but `nil` will become `null`! To get
;; `false`, `:json-false` is needed.
;; (lsp--set-configuration
;;  '(:ts-ls (:javascript.format.insertSpaceBeforeFunctionParenthesis
;;            :json-false
;;            :javascript.format.insertSpaceAfterOpeningAndBeforeClosingEmptyBraces
;;            :json-false
;;            :javascript.format.insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces
;;            :json-false)))
;; Cons I got from VSCode TypeScript language server:
;;   - I prefer CommonJS to ES module, but it warns me to replace `require` with
;;    `import` like I am killing Jesus.
;;   - Asking for type hints for my own JavaScript library.
;;   - Showing type hints with inline completion.
;;   - Doing "formatting" when I just want "indentation".
;; Pros I got from VSCode TypeScript language server:
;;   - Nothing.
;; OK, finally I modified `lsp-mode`'s code and send a PR.
;; See <https://github.com/emacs-lsp/lsp-mode/pull/3409>.
;; I switched to `lsp-bridge` which does not format code via LSP.
(use-package lsp-mode
  :ensure t
  :disabled
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
         ;; Don't enable lsp for `web-mode`, lsp cannot understand nunjucks.
         ;; (web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-," . lsp-find-references))
  :custom
  ;; Move lsp files into local dir.
  (lsp-server-install-dir (locate-user-emacs-file ".local/lsp/"))
  (lsp-session-file (locate-user-emacs-file ".local/lsp-session"))
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  ;; Only enable log for debug.
  ;; This controls `*lsp-log*` buffer.
  (lsp-log-io nil)
  ;; For better performance.
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-lens-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers nil)
  ;; Oh God please don't modify code by default, most open source projects don't
  ;; like this because it will mess up commits.
  (lsp-trim-final-newlines nil)
  (lsp-trim-trailing-whitespace nil)
  ;; JavaScript (ts-ls) settings.
  ;; OMG, the FUCKING EVIL SHITTY VSCode TypeScript language server generates
  ;; log in project dir, can MicroSoft stop to let their software make shit in
  ;; front of users?
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/tsserver-log.txt"))
  (lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)
  ;; Always let clangd look for `compile_commands.json` under build dir so it
  ;; will not make project root dirty.
  (lsp-clients-clangd-args '
   ("--header-insertion-decorators=0" "--compile-commands-dir=./build/" "--enable-config")))

;; High CPU usage on scrolling.
(use-package lsp-ui
  :ensure t
  :disabled
  :defer 1
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-diagnostic-max-lines 3))

(use-package lsp-treemacs
  :ensure t
  :disabled
  :defer 1
  :commands lsp-treemacs-errors-list)

(provide 'unused)

;;; unused,el ends here.
