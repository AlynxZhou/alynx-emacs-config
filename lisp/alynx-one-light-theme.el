;;; alynx-one-light-theme.el --- Alynx's One Light theme. -*- lexical-binding: t -*-

;;; Commentary:
;; Alynx's One Light theme, based on
;; <https://github.com/jonathanchu/atom-one-light-theme/>, tweaked and removed
;; things I never use.
;;
;; To remind me what I did:
;;   - Removed terminal colors, I use Emacs as a GUI app.
;;   - Removed helm/ivy colors, I use Vertico now.
;;   - Removed many packages that I don't use.
;;   - Removed prefix of colors, we use them in `let`.

;;; Code:

(deftheme alynx-one-light
  "Alynx One Light - Alynx's Emacs port of the Atom One Light theme."
  :family 'alynx-one)

(let ((accent "#526EFF")
      (fg "#383A42")
      (bg "#FAFAFA")
      (bg-1 "#E5E5E6")
      (bg-hl "#F0F0F1")
      (gutter "#C2C2C3")
      (guide "#D3D3D5")
      (insert "#2DB448")
      (change "#F2A60D")
      (delete "#FF1414")
      (info "#1492FF")
      (success "#2DB448")
      (warning "#D5880B")
      (error "#F42A2A")
      (mono-1 "#383A42")
      (mono-2 "#696C77")
      (mono-3 "#A0A1A7")
      (cyan "#0184BC")
      (blue "#4078F2")
      (purple "#A626A4")
      (green "#50A14F")
      (red-1 "#E45649")
      (red-2 "#CA1243")
      (orange-1 "#B76B01")
      (orange-2 "#CB7701")
      (gray "#E5E5E6")
      (silver "#EAEAEB")
      (black "#424243")
      (selection "#E5E5E6")
      (ui-fg "#424243")
      (level-3-color "#EAEAEB")
      (border "#DBDBDC"))

  (custom-theme-set-faces
   'alynx-one-light

   ;; Well, actually Atom has UI and syntax colors. But in Emacs we don't have
   ;; so much UI. So mostly we use `syntax-bg`.
   `(default ((t (:foreground ,fg :background ,bg))))
   `(success ((t (:foreground ,success))))
   `(warning ((t (:foreground ,warning))))
   `(error ((t (:foreground ,error :weight bold))))
   `(link ((t (:foreground ,blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,blue :underline t :weight normal))))
   `(cursor ((t (:background ,accent))))
   ;; `gutter` is good for it.
   `(fringe ((t (:foreground ,gutter :background ,bg))))
   ;; Don't set `:distant-foreground` for `region`, it adds this to indent
   ;; guides and fill column indicator, cover this for fill column indicator
   ;; works, but not for indent guides. It's just hard to know when Emacs
   ;; decides to use `:distant-foreground`.
   `(region ((t (:background ,selection :extend t))))
   `(highlight ((t (:background ,gray))))
   `(hl-line ((t (:background ,bg-hl :extend t))))
   ;; For mode line and header line, use UI colors, so they are different from
   ;; editing area.
   `(header-line ((t (:background ,level-3-color))))
   `(vertical-border ((t (:background ,border :foreground ,border))))
   `(secondary-selection ((t (:background ,bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,ui-fg))))
   `(tooltip ((t (:inherit (variable-pitch) :foreground ,fg :background ,bg-1))))

   ;; Maybe those should be updated to match Atom's syntax.
   `(font-lock-builtin-face ((t (:foreground ,cyan))))
   `(font-lock-comment-face ((t (:foreground ,mono-3 :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,mono-2))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,orange-2))))
   `(font-lock-constant-face ((t (:foreground ,cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,red-1))))
   `(font-lock-warning-face ((t (:foreground ,mono-3 :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,cyan :bold t))))

   ;; Mode line.
   `(mode-line ((t (:background ,level-3-color :foreground ,ui-fg :box (:color ,border :line-width 1)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   ;; `mono-3` is good for both dark and light.
   `(mode-line-inactive ((t (:background ,border :foreground ,mono-3 :box (:color ,border :line-width 1)))))

   ;; Window divider.
   `(window-divider ((t (:foreground ,border))))
   `(window-divider-first-pixel ((t (:foreground ,border))))
   `(window-divider-last-pixel ((t (:foreground ,border))))

   ;; Custom.
   `(custom-state ((t (:foreground ,green))))

   ;; Flyspell.
   `(flyspell-duplicate ((t (:underline (:color ,warning :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,error :style wave)))))

   ;; Flymake.
   `(flymake-error ((t (:underline (:color ,error)))))
   `(flymake-warning ((t (:underline (:color ,warning)))))
   `(flymake-note ((t (:underline (:color ,info)))))

   ;; Flycheck.
   `(flycheck-error ((t (:underline (:color ,error)))))
   `(flycheck-warning ((t (:underline (:color ,warning)))))
   `(flycheck-info ((t (:underline (:color ,info)))))

   ;; `lsp-bridge`.
   `(lsp-bridge-diagnostics-error-face ((t (:underline (:color ,error)))))
   `(lsp-bridge-diagnostics-warning-face ((t (:underline (:color ,warning)))))
   `(lsp-bridge-diagnostics-info-face ((t (:underline (:color ,info)))))
   `(lsp-bridge-diagnostics-hint-face ((t (:underline (:color ,fg)))))

   ;; Trailing whitespace.
   ;; This is different from the package called `whitespace`.
   ;; Setting underline is not OK, it blocks region, don't know why.
   `(trailing-whitespace ((t (:background ,warning))))

   ;; Compilation.
   `(compilation-face ((t (:foreground ,fg))))
   `(compilation-line-number ((t (:foreground ,mono-2))))
   `(compilation-column-number ((t (:foreground ,mono-2))))
   `(compilation-mode-line-exit ((t (:inherit (compilation-info) :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit (compilation-error) :weight bold))))

   ;; Isearch.
   `(isearch ((t (:foreground ,bg :background ,cyan))))
   `(isearch-fail ((t (:foreground ,error :background unspecified))))
   `(lazy-highlight ((t (:foreground ,cyan :background ,bg-1 :underline ,cyan))))

   ;; Replace.
   `(match ((t (:foreground ,bg :background ,blue))))

   ;; Consult.
   `(consult-line-number-prefix ((t (:inherit (line-number)))))
   `(consult-line-number-wrapped ((t (:inherit (line-number) :weight bold))))
   `(consult-preview-match ((t (:inherit (isearch)))))
   `(consult-highlight-match ((t (:inherit (isearch)))))
   ;; Don't inherit default, because it changes background.
   `(consult-buffer ((t (:foreground ,fg))))
   ;; Like `diff-mode`.
   `(consult-file ((t (:inherit (font-lock-type-face) :weight bold))))

   ;; `diff-mode`.
   ;; The default styles are too noisy.
   ;; Unchanged text and e-mail content of `git format-patch`.
   `(diff-context ((t (:inherit (default)))))
   ;; Easier to see different files.
   `(diff-header ((t (:inherit (font-lock-preprocessor-face)))))
   ;; Not sure what `diff-index` is.
   `(diff-index ((t (:inherit (diff-header)))))
   ;; File names are important.
   `(diff-file-header ((t (:inherit (font-lock-type-face) :weight bold))))
   `(diff-nonexistent ((t (:inherit (diff-file-header)))))
   ;; Easier to see different hunks, they are less important.
   `(diff-hunk-header ((t (:inherit (font-lock-keyword-face)))))
   `(diff-function ((t (:inherit (font-lock-function-name-face)))))
   `(diff-error ((t (:underline (:color ,error)))))
   `(diff-removed ((t (:foreground ,delete :background unspecified))))
   `(diff-added ((t (:foreground ,insert :background unspecified))))
   `(diff-changed ((t (:foreground ,change :background unspecified))))
   `(diff-changed-unspecified ((t (:inherit (diff-changed)))))
   `(diff-indicator-removed ((t (:inherit (diff-removed)))))
   `(diff-indicator-added ((t (:inherit (diff-added)))))
   `(diff-indicator-changed ((t (:inherit (diff-changed)))))
   ;; Using underline to hint char changes is clear.
   `(diff-refine-removed ((t (:underline t))))
   `(diff-refine-added ((t (:underline t))))
   `(diff-refine-changed ((t (:underline t))))

   ;; `diff-hl`.
   `(diff-hl-change ((t (:foreground ,change :background unspecified))))
   `(diff-hl-delete ((t (:foreground ,delete :background unspecified))))
   `(diff-hl-insert ((t (:foreground ,insert :background unspecified))))

   ;; Dired.
   ;; Isn't it a good idea to use `link` as `dir`?
   `(dired-directory ((t (:inherit (link)))))
   `(dired-flagged ((t (:foreground ,change))))
   `(dired-symlink ((t (:foreground ,cyan))))

   ;; Eshell.
   `(eshell-ls-archive ((t (:foreground ,purple :weight bold))))
   `(eshell-ls-backup ((t (:foreground ,orange-2))))
   `(eshell-ls-clutter ((t (:foreground ,red-2 :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,green :weight bold))))
   `(eshell-ls-missing ((t (:foreground ,red-1 :weight bold))))
   `(eshell-ls-product ((t (:foreground ,orange-2))))
   `(eshell-ls-special ((t (:foreground ,red-2 :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,mono-1))))
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))

   ;; `js2-mode`.
   `(js2-error ((t (:underline (:color ,error :style wave)))))
   `(js2-external-variable ((t (:foreground ,cyan))))
   `(js2-warning ((t (:underline (:color ,warning :style wave)))))
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,purple))))
   `(js2-jsdoc-type ((t (:foreground ,orange-2))))
   `(js2-jsdoc-value ((t (:foreground ,red-1))))
   `(js2-object-property ((t (:foreground ,red-1))))

   ;; `rainbow-delimiters`.
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,orange-1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange-2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,orange-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,orange-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,red-1 :weight bold))))

   ;; `show-paren`.
   `(show-paren-match ((t (:inherit (success) :underline t))))
   `(show-paren-mismatch ((t (:inherit (error) :underline t))))

   ;; `web-mode`.
   `(web-mode-doctype-face ((t (:inherit (font-lock-comment-face)))))
   `(web-mode-error-face ((t (:inherit (error)))))
   `(web-mode-html-attr-equal-face ((t (:inherit (default)))))
   `(web-mode-html-attr-name-face ((t (:foreground ,orange-1))))
   `(web-mode-html-tag-bracket-face ((t (:inherit (default)))))
   `(web-mode-html-tag-face ((t (:foreground ,red-1))))
   `(web-mode-symbol-face ((t (:foreground ,orange-1))))

   ;; `nxml`.
   `(nxml-attribute-local-name ((t (:foreground ,orange-1))))
   `(nxml-element-local-name ((t (:foreground ,red-1))))
   `(nxml-markup-declaration-delimiter ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
   `(nxml-processing-instruction-delimiter ((t (:inherit (nxml-markup-declaration-delimiter)))))

   ;; `rpm-spec-mode`.
   `(rpm-spec-tag-face ((t (:foreground ,blue))))
   `(rpm-spec-obsolete-tag-face ((t (:inherit (warning)))))
   `(rpm-spec-macro-face ((t (:foreground ,orange-2))))
   `(rpm-spec-var-face ((t (:foreground ,red-1))))
   `(rpm-spec-doc-face ((t (:foreground ,purple))))
   `(rpm-spec-dir-face ((t (:foreground ,cyan))))
   `(rpm-spec-package-face ((t (:foreground ,red-2))))
   `(rpm-spec-ghost-face ((t (:foreground ,red-2))))
   `(rpm-spec-section-face ((t (:foreground ,orange-2))))

   ;; `linum`.
   `(linum ((t (:foreground ,gutter :background ,bg :weight normal :slant normal))))
   ;; `hlinum`.
   `(linum-highlight-face ((t (:foreground ,fg :background ,bg :weight normal :slant normal))))
   ;; Native line numbers (version >=26).
   `(line-number ((t (:foreground ,gutter :background ,bg :weight normal :slant normal))))
   `(line-number-current-line ((t (:foreground ,fg :background ,bg :weight normal :slant normal))))

   ;; Fill column indicator.
   ;; Inherit shadow, so no background is set, and then use better color, and
   ;; ignore styles.
   `(fill-column-indicator ((t (:inherit (shadow) :weight normal :slant normal :foreground ,guide))))

   ;; Highlight indent guides.
   ;; Similiar to fill column indicator. I don't use responsive guides so
   ;; those are enough.
   `(highlight-indent-guides-character-face ((t (:inherit (shadow) :weight normal :slant normal :foreground ,guide))))
   `(highlight-indent-guides-odd-face ((t (:inherit (shadow) :weight normal :slant normal :foreground ,guide))))
   `(highlight-indent-guides-even-face ((t (:inherit (shadow) :weight normal :slant normal :foreground ,guide))))

   ;; `regexp-builder`.
   `(reb-match-0 ((t (:background ,level-3-color))))
   `(reb-match-1 ((t (:background ,level-3-color :foreground ,purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,level-3-color :foreground ,green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,level-3-color :foreground ,orange-2 :weight semi-bold))))

   ;; Desktop entry.
   `(desktop-entry-deprecated-keyword-face ((t (:inherit (font-lock-warning-face)))))
   `(desktop-entry-group-header-face ((t (:inherit (font-lock-type-face)))))
   `(desktop-entry-locale-face ((t (:inherit (font-lock-string-face)))))
   `(desktop-entry-unknown-keyword-face ((t (:inherit (font-lock-keyword-face) :underline (:color ,red-1 :style wave)))))
   `(desktop-entry-value-face ((t (:inherit (default)))))

   ;; `latex-mode`.
   `(font-latex-sectioning-0-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,green :slant italic))))
   `(font-latex-warning-face ((t (:inherit (font-lock-warning-face)))))
   `(font-latex-doctex-preprocessor-face ((t (:inherit (font-lock-preprocessor-face)))))
   `(font-latex-script-char-face ((t (:inherit (font-lock-negation-char-face)))))

   ;; `org-mode`.
   `(org-date ((t (:foreground ,cyan))))
   `(org-document-info ((t (:foreground ,mono-2))))
   `(org-document-info-keyword ((t (:inherit (org-meta-line) :underline t))))
   `(org-document-title ((t (:weight bold))))
   `(org-footnote ((t (:foreground ,cyan))))
   `(org-sexp-date ((t (:foreground ,cyan))))

   ;; `calendar`.
   `(diary ((t (:foreground ,orange-2))))
   `(holiday ((t (:foreground ,green))))

   ;; `ruler-mode`
   `(ruler-mode-default ((t (:background ,level-3-color :foreground ,ui-fg :box (:color ,border :line-width 1)))))
   `(ruler-mode-column-number ((t (:inherit (ruler-mode-default)))))
   `(ruler-mode-comment-column ((t (:inherit (ruler-mode-default):foreground ,red-1))))
   ;; Because cursor is always in current column so accent is OK here.
   `(ruler-mode-current-column ((t (:inherit (ruler-mode-default) :foreground ,blue))))
   `(ruler-mode-fill-column ((t (:inherit (ruler-mode-default) :foreground ,orange-1))))
   `(ruler-mode-fringes ((t (:inherit (ruler-mode-default) :foreground ,green))))
   `(ruler-mode-margins ((t (:inherit (ruler-mode-default)))))
   `(ruler-mode-goal-column ((t (:inherit (ruler-mode-default) :foreground ,cyan))))
   `(ruler-mode-tab-stop ((t (:inherit (ruler-mode-default) :foreground ,mono-3))))

   ;; `undo-tree`.
   `(undo-tree-visualizer-current-face ((t (:foreground ,red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,cyan))))

   ;; tab-bar-mode
   `(tab-bar-tab-inactive ((t (:background ,level-3-color :foreground ,ui-fg))))
   `(tab-bar-tab          ((t (:background ,bg :foreground ,purple))))
   `(tab-bar              ((t (:background ,level-3-color))))))

;; Automatically add this theme to the load path.
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'alynx-one-light)

;;; alynx-one-light-theme.el ends here.
