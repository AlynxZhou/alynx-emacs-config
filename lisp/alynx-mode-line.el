;;; alynx-mode-line.el --- Alynx's mode line. -*- lexical-binding: t -*-

;;; Commentary:
;; Just a mode line I want, initially forked from `mood-line`, then removed
;; features I don't use and fixed font face handling.

;;; Code:

(defconst alynx-mode-line-glyphs-ascii
  '((:checker-info . ?i)
    (:checker-issues . ?!)
    (:checker-good . ?+)
    (:checker-checking . ?-)
    (:checker-errored . ?x)
    (:checker-interrupted . ?=)

    (:vc-added . ?+)
    (:vc-needs-merge . ?>)
    (:vc-needs-update . ?v)
    (:vc-conflict . ?x)
    (:vc-good . ?-)

    (:buffer-narrowed . ?v)
    (:buffer-modified . ?*)
    (:buffer-read-only . ?#)

    (:count-separator . ?*))
  "Set of ASCII glyphs.")

(defconst alynx-mode-line-glyphs-fira-code
  '((:checker-info . ?↳)
    (:checker-issues . ?→)
    (:checker-good . ?✓)
    (:checker-checking . ?⟳)
    (:checker-errored . ?x)
    (:checker-interrupted . ?=)

    (:vc-added . ?+)
    (:vc-needs-merge . ?⟷)
    (:vc-needs-update . ?↓)
    (:vc-conflict . ?x)
    (:vc-good . ?✓)

    (:buffer-narrowed . ?◢)
    (:buffer-modified . ?●)
    (:buffer-read-only . ?■)

    (:count-separator . ?×))
  "Set of Fira Code-compatible glyphs.")

(defconst alynx-mode-line-glyphs-unicode
  '((:checker-info . ?🛈)
    (:checker-issues . ?⚑)
    (:checker-good . ?✔)
    (:checker-checking . ?🗘)
    (:checker-errored . ?✖)
    (:checker-interrupted . ?⏸)

    (:vc-added . ?🞤)
    (:vc-needs-merge . ?⟷)
    (:vc-needs-update . ?↓)
    (:vc-conflict . ?✖)
    (:vc-good . ?✔)

    (:buffer-narrowed . ?▼)
    (:buffer-modified . ?●)
    (:buffer-read-only . ?■)

    (:count-separator . ?✕))
  "Set of Unicode glyphs.")

(defgroup alynx-mode-line nil
  "Configuration of alynx-mode-line."
  :group 'mode-line)

(defgroup alynx-mode-line-faces nil
  "Faces used by alynx-mode-line."
  :group 'alynx-mode-line
  :group 'faces)

(defcustom alynx-mode-line-show-indentation-style t
  "When non-nil, show the indentation style of the current buffer."
  :group 'alynx-mode-line
  :type 'boolean)

(defcustom alynx-mode-line-show-eol-style t
  "When non-nil, show the EOL style of the current buffer."
  :group 'alynx-mode-line
  :type 'boolean)

(defcustom alynx-mode-line-show-encoding t
  "When non-nil, show the encoding format of the current buffer."
  :group 'alynx-mode-line
  :type 'boolean)

(defcustom alynx-mode-line-show-cursor-position t
  "When non-nil, show the cursor position of the current buffer."
  :group 'alynx-mode-line
  :type 'boolean)

(defcustom alynx-mode-line-show-major-mode t
  "When non-nil, show the name of the major mode of the current buffer."
  :group 'alynx-mode-line
  :type 'boolean)

(defcustom alynx-mode-line-segment-sperator " "
  "Sperator shows before and after segments."
  :group 'alynx-mode-line
  :type 'string)

(defcustom alynx-mode-line-glyph-alist alynx-mode-line-glyphs-ascii
  "Alist mapping glyph names to characters used to draw some mode line segments.

alynx-mode-line includes several sets of glyphs by default:

`alynx-mode-line-glyphs-ascii'     | Basic ASCII character glyphs.
`alynx-mode-line-glyphs-fira-code' | Fira Code-compatible glyphs.
`alynx-mode-line-glyphs-unicode'   | Fancy unicode glyphs.

Note that if a character provided by a glyph set is not included in your default
font, the editor will render it with a fallback font.  If your fallback font is
not the same height as your default font, the mode line may unexpectedly grow
or shrink.

Keys are names for different mode line glyphs, values are characters for that
glyph.  Glyphs used by alynx-mode-line include:

`:checker-info'        | Syntax checker reports notes.
`:checker-issues'      | Syntax checker reports issues.
`:checker-good'        | Syntax checker reports no issues.
`:checker-checking'    | Syntax checker is running.
`:checker-errored'     | Syntax checker is stopped due to an error.
`:checker-interrupted' | Syntax checker is paused.

`:vc-added'            | VC backend reports additions/changes.
`:vc-needs-merge'      | VC backend reports required merge.
`:vc-needs-update'     | VC backend reports upstream is ahead of local.
`:vc-conflict'         | VC backend reports conflict.
`:vc-good'             | VC backend has nothing to report.

`:buffer-narrowed'     | File-backed buffer is narrowed.
`:buffer-modified'     | File-backed buffer is modified.
`:buffer-read-only'    | File-backed buffer is read-only.

`:count-separator'     | Separates some indicator names from numerical counts.

`alynx-mode-line-glyphs-ascii' will be used as a fallback wherever the a glyph
may be found to be missing in `alynx-mode-line-glyph-alist'."
  :group 'alynx-mode-line
  :type `(alist :tag "Character map alist"
                :key-type (symbol :tag "Glyph name")
                :value-type (character :tag "Character to use")))

;; Based on `editorconfig-indentation-alist` and `doom-modeline-indent-alist`.
;; See <https://github.com/editorconfig/editorconfig-emacs/blob/b8043702f3d977db0e030c6c64ee4a810cad5f45/editorconfig.el#L175>.
;; See <https://github.com/seagle0128/doom-modeline/blob/fe9ee5a2a950f9ded10261a05a12adc577ae9e36/doom-modeline-core.el#L284>.
(defcustom alynx-mode-line-mode-indent-offset-alist
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c-mode c-basic-offset)
    (c-ts-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c++-ts-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
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
  "Alist that mapping major modes to their indent offset variables.

When multiple variables are specified for a given mode, the offset value will
be retrieved from the first variable that resolves to a value (evaluated in the
order provided)."
  :group 'alynx-mode-line
  :type '(alist :key-type symbol :value-type sexp))

(defface alynx-mode-line-face-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for displaying the value of `buffer-name'."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-buffer-status-modified
  '((t (:inherit (error) :weight normal)))
  "Face used for the ':buffer-modified' buffer status indicator."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-buffer-status-read-only
  '((t (:inherit (shadow) :weight normal)))
  "Face used for the ':buffer-read-only' buffer status indicator."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-buffer-status-narrowed
  '((t (:inherit (warning) :weight normal)))
  "Face used for the ':buffer-narrowed' buffer status indicator."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-major-mode
  '((t (:inherit (mode-line-emphasis))))
  "Face used for the major mode indicator."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-status-info
  '((t (:inherit (link) :weight normal :underline nil)))
  "Face used for generic status indicators."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-status-success
  '((t (:inherit (success) :weight normal)))
  "Face used for success status indicators."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-status-warning
  '((t (:inherit (warning) :weight normal)))
  "Face for warning status indicators."
  :group 'alynx-mode-line-faces)

(defface alynx-mode-line-face-status-error
  '((t (:inherit (error) :weight normal)))
  "Face for error status indicators."
  :group 'alynx-mode-line-faces)

(defun alynx-mode-line--get-glyph (glyph)
  "Return character from `alynx-mode-line-glyph-alist' for GLYPH.

If a character could not be found for the requested glyph, a fallback will be
returned from `alynx-mode-line-glyphs-ascii'."
  (char-to-string (or (alist-get glyph
                                 alynx-mode-line-glyph-alist)
                      (alist-get glyph
                                 alynx-mode-line-glyphs-ascii))))

(defmacro alynx-mode-line--concat-with-sperator (&rest sequences)
  "Concatenate speartor, SEQUENCES and sperator and make the result a string."
  `(concat alynx-mode-line-segment-sperator
           ,@sequences
           alynx-mode-line-segment-sperator))

;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html>.
(defun alynx-mode-line--align (left right)
  "Align a mode line with a LEFT and RIGHT justified list of elements.

The mode line should fit the `window-width' with space between the lists."
  (let ((reserve (length right)))
    (concat " "
            left
            (propertize " "
                        'display `((space :align-to (- (+ right right-margin)
                                                       ,reserve))))
            right
            " ")))

(defun alynx-mode-line--segment-buffer-status ()
  "Return an indicator representing the status of the current buffer."
  (alynx-mode-line--concat-with-sperator
          (if (buffer-file-name (buffer-base-buffer))
              (cond
               ((buffer-narrowed-p)
                (propertize (alynx-mode-line--get-glyph :buffer-narrowed)
                            'face 'alynx-mode-line-face-buffer-status-narrowed))
               ((buffer-modified-p)
                (propertize (alynx-mode-line--get-glyph :buffer-modified)
                            'face 'alynx-mode-line-face-buffer-status-modified))
               (buffer-read-only
                (propertize (alynx-mode-line--get-glyph :buffer-read-only)
                            'face 'alynx-mode-line-face-buffer-status-read-only))
               (t " "))
            (if (buffer-narrowed-p)
                (propertize (alynx-mode-line--get-glyph :buffer-narrowed)
                            'face 'alynx-mode-line-face-buffer-status-narrowed)
              " "))))

(defun alynx-mode-line--segment-buffer-name ()
  "Display the name of the current buffer."
  (alynx-mode-line--concat-with-sperator
          (propertize "%b" 'face 'alynx-mode-line-face-buffer-name)))

(defun alynx-mode-line--segment-cursor-position ()
  "Display the position of the cursor in the current buffer."
  (when alynx-mode-line-show-cursor-position
    (alynx-mode-line--concat-with-sperator
            "%l:%c"
            " "
            (number-to-string (point))
            " "
            "%p%%")))

(defun alynx-mode-line--segment-indentation-style ()
  "Display the indentation style of the current buffer."
  (when alynx-mode-line-show-indentation-style
    (let* ((mode-offset (symbol-value
                         (seq-some #'identity
                                   (cdr (assoc major-mode
                                               alynx-mode-line-mode-indent-offset-alist))))))
      (alynx-mode-line--concat-with-sperator
              (if indent-tabs-mode "TAB" "SPC")
              (alynx-mode-line--get-glyph :count-separator)
              (if (null mode-offset) "?" (number-to-string mode-offset))
              ":"
              (number-to-string tab-width)))))

(defun alynx-mode-line--segment-eol-style ()
  "Display the EOL type for the coding system of the current buffer."
  (when (and alynx-mode-line-show-eol-style buffer-file-coding-system)
    (alynx-mode-line--concat-with-sperator
            (pcase (coding-system-eol-type buffer-file-coding-system)
              (0 "LF")
              (1 "CRLF")
              (2 "CR")))))

(defun alynx-mode-line--segment-encoding ()
  "Display the name of the coding system of the current buffer."
  (when (and alynx-mode-line-show-encoding
             buffer-file-coding-system)
    (alynx-mode-line--concat-with-sperator
            (let ((coding-system
                   (coding-system-plist buffer-file-coding-system)))
              (cond
               ((memq (plist-get coding-system :category)
                      '(coding-category-undecided coding-category-utf-8))
                "UTF-8")
               (t
                (upcase (symbol-name (plist-get coding-system :name)))))))))

(defun alynx-mode-line--segment-major-mode ()
  "Display the name of the major mode of the current buffer."
  (when alynx-mode-line-show-major-mode
    (alynx-mode-line--concat-with-sperator
            ;; Call `format-mode-line` here because we want to process the
            ;; string by ourselves.
            (propertize (substring-no-properties (format-mode-line mode-name))
                        'face 'alynx-mode-line-face-major-mode))))

(defvar-local alynx-mode-line--vc-text nil)

(defun alynx-mode-line--vc-update-segment (&rest _)
  "Update `alynx-mode-line--vc-text' with the current VCS state."
  (setq alynx-mode-line--vc-text
        (when (and vc-mode
                   buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (branch (substring-no-properties vc-mode
                                                  (+ (if (eq backend 'Hg) 2 3)
                                                     2)))
                 (state (vc-state buffer-file-name
                                  (vc-backend buffer-file-name)))
                 (face 'alynx-mode-line-face-status-success)
                 (glyph :vc-good))
            (cond
             ((memq state '(edited added))
              (setq face 'alynx-mode-line-face-status-info
                    glyph :vc-added))
             ((eq state 'needs-merge)
              (setq face 'alynx-mode-line-face-status-warning
                    glyph :vc-needs-merge))
             ((eq state 'needs-update)
              (setq face 'alynx-mode-line-face-status-warning
                    glyph :vc-needs-update))
             ((memq state '(removed conflict unregistered))
              (setq face 'alynx-mode-line-face-status-error
                    glyph :vc-conflict))
             (t
              (setq face 'alynx-mode-line-face-status-success
                    glyph :vc-good)))
            (alynx-mode-line--concat-with-sperator
                    (propertize (concat (alynx-mode-line--get-glyph glyph)
                                        " "
                                        branch)
                                'face face))))))

(defun alynx-mode-line--segment-vc ()
  "Display color-coded version control information."
  alynx-mode-line--vc-text)

(defvar-local alynx-mode-line--checker-flycheck-text nil)

(defvar flycheck-current-errors nil)

(declare-function flycheck-count-errors "flycheck" (errors))

(defun alynx-mode-line--checker-flycheck-count ()
  "Return alist with count of all types in `flycheck-current-errors'.

Counts will be returned in an alist as the `cdr' of the following keys:
`'info-count'    | All notes reported by checker.
`'error-count'   | All errors reported by checker.
`'warning-count' | All warnings reported by checker.
`'issues-count'  | All errors and warnings reported by checker.
`'all-count'     | Everything reported by checker."
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (let ((info-count (+ (or .info 0)))
          (error-count (+ (or .error 0)))
          (warning-count (+ (or .warning 0))))
      `((info-count . ,info-count)
        (error-count . ,error-count)
        (warning-count . ,warning-count)
        (issues-count . ,(+ warning-count
                            error-count))
        (all-count . ,(+ info-count
                         warning-count
                         error-count))))))

(defun alynx-mode-line--checker-flycheck-update-segment (&optional status)
  "Update `alynx-mode-line--checker-flycheck-text' with provided STATUS."
  (setq alynx-mode-line--checker-flycheck-text
        (pcase status
          ('finished
           (alynx-mode-line--concat-with-sperator
                   (let-alist (alynx-mode-line--checker-flycheck-count)
                     (cond
                      ((> .error-count 0)
                       (propertize (concat (alynx-mode-line--get-glyph :checker-issues)
                                           " "
                                           "Error: "
                                           (number-to-string .all-count))
                                   'face 'alynx-mode-line-face-status-error))
                      ((> .warning-count 0)
                       (propertize (concat (alynx-mode-line--get-glyph :checker-issues)
                                           " "
                                           "Issue: "
                                           (number-to-string .all-count))
                                   'face 'alynx-mode-line-face-status-warning))
                      ((> .info-count 0)
                       (propertize (concat (alynx-mode-line--get-glyph :checker-info)
                                           " "
                                           "Info: "
                                           (number-to-string .all-count))
                                   'face 'alynx-mode-line-face-status-info))
                      ((zerop .all-count)
                       (propertize (concat (alynx-mode-line--get-glyph :checker-good)
                                           " "
                                           "Good")
                                   'face 'alynx-mode-line-face-status-success))))))
          ('running
           (alynx-mode-line--concat-with-sperator
            (alynx-mode-line--get-glyph :checker-checking)
            " "
            "Checking"))
          ('errored
           (alynx-mode-line--concat-with-sperator
                   (propertize (concat (alynx-mode-line--get-glyph :checker-errored)
                                       " "
                                       "Error")
                               'face 'alynx-mode-line-face-status-error)))
          ('interrupted
           (alynx-mode-line--concat-with-sperator
            (alynx-mode-line--get-glyph :checker-interrupted)
            " "
            "Paused"))
          ;; Hide this and its sperators if not running.
          ('no-checker ""))))

(defun alynx-mode-line--segment-checker-flycheck ()
  "Display the current status of flycheck."
  alynx-mode-line--checker-flycheck-text)

(defvar-local alynx-mode-line--checker-flymake-text nil)

(declare-function flymake-running-backends "flymake" ())
(declare-function flymake-reporting-backends "flymake" ())

(defun alynx-mode-line--checker-flymake-count-report-type (type)
  "Return count of current flymake reports of TYPE."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (eq (cl-struct-slot-value 'flymake--diag 'type d) type)
        (cl-incf count)))
    count))

(defun alynx-mode-line--checker-flymake-count ()
  "Return alist with count of all current flymake diagnostic reports.

Counts will be returned in an alist as the cdr of the following keys:
`'info-count'    | All notes reported by checker.
`'error-count'   | All errors reported by checker.
`'warning-count' | All warnings reported by checkero.
`'issues-count'  | All errors and warnings reported by checker.
`'all-count'     | Everything reported by checker."
  (let ((info-count (alynx-mode-line--checker-flymake-count-report-type :note))
        (error-count (alynx-mode-line--checker-flymake-count-report-type :error))
        (warning-count (alynx-mode-line--checker-flymake-count-report-type :warning)))
    `((info-count . ,info-count)
      (error-count . ,error-count)
      (warning-count . ,warning-count)
      (issues-count . ,(+ warning-count
                          error-count))
      (all-count . ,(+ info-count
                       warning-count
                       error-count)))))

(defun alynx-mode-line--checker-flymake-update-segment (&rest _)
  "Update `alynx-mode-line--checker-flymake-text' against the state of flymake."
  (setq alynx-mode-line--checker-flymake-text
        (if (and (fboundp 'flymake-is-running)
                   (flymake-is-running))
          (let-alist (alynx-mode-line--checker-flymake-count)
            (cond
             ((seq-difference (flymake-running-backends)
                              (flymake-reporting-backends))
              (alynx-mode-line--concat-with-sperator
               (alynx-mode-line--get-glyph :checker-checking)
               " "
               "Checking"))
             ((> .error-count 0)
              (alynx-mode-line--concat-with-sperator
                      (propertize (concat (alynx-mode-line--get-glyph :checker-issues)
                                          " "
                                          "Errors: "
                                          (number-to-string .all-count))
                                  'face 'alynx-mode-line-face-status-error)))
             ((> .warning-count 0)
              (alynx-mode-line--concat-with-sperator
                      (propertize (concat (alynx-mode-line--get-glyph :checker-issues)
                                          " "
                                          "Issues: "
                                          (number-to-string .all-count))
                          	  'face 'alynx-mode-line-face-status-warning)))
             ((> .info-count 0)
              (alynx-mode-line--concat-with-sperator
                      (propertize (concat (alynx-mode-line--get-glyph :checker-info)
                                          " "
                                          "Info: "
                                          (number-to-string .all-count))
                                  'face 'alynx-mode-line-face-status-info)))
             (t
              (alynx-mode-line--concat-with-sperator
                      (propertize (concat (alynx-mode-line--get-glyph :checker-good)
                                          " "
                                          "Good")
                                  'face 'alynx-mode-line-face-status-success))))))))

(defun alynx-mode-line--segment-checker-flymake ()
  "Display the current status of flymake."
  alynx-mode-line--checker-flymake-text)

(defun alynx-mode-line--segment-checker ()
  "Return the correct mode line segment for the first active checker found.

Checkers checked, in order: `flycheck', `flymake'."
  (cond
   ((bound-and-true-p flycheck-mode)
    (alynx-mode-line--segment-checker-flycheck))
   ((bound-and-true-p flymake-mode)
    (alynx-mode-line--segment-checker-flymake))))

(defun alynx-mode-line--segment-process ()
  "Display the current value of `mode-line-process'."
  ;; Call `format-mode-line` here because we want to process the string.
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string-blank-p process-info)
      (alynx-mode-line--concat-with-sperator
              (string-trim process-info)))))

(defun alynx-mode-line--segment-misc-info ()
  "Display the current value of `mode-line-misc-info'."
  ;; Call `format-mode-line` here because we want to process the string.
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (alynx-mode-line--concat-with-sperator
              (string-trim misc-info)))))

(defvar-local alynx-mode-line--original-mode-line mode-line-format)

(defun alynx-mode-line--activate ()
  "Activate alynx-mode-line."

  ;; Add flycheck hooks.
  (add-hook 'flycheck-status-changed-functions
            #'alynx-mode-line--checker-flycheck-update-segment)
  (add-hook 'flycheck-mode-hook
            #'alynx-mode-line--checker-flycheck-update-segment)

  ;; Add flymake hooks.
  (advice-add 'flymake-start :after
              #'alynx-mode-line--checker-flymake-update-segment)
  (advice-add 'flymake--handle-report :after
              #'alynx-mode-line--checker-flymake-update-segment)

  ;; Add VC hooks.
  (add-hook 'find-file-hook
            #'alynx-mode-line--vc-update-segment)
  (add-hook 'after-save-hook
            #'alynx-mode-line--vc-update-segment)
  (advice-add 'vc-refresh-state :after
              #'alynx-mode-line--vc-update-segment)

  ;; Save previous value of `mode-line-format`, it's a buffer-local variable and
  ;; we only save global value here.
  (setq alynx-mode-line--original-mode-line (default-value 'mode-line-format))

  ;; `format-mode-line` processes mode line interestingly:
  ;;   1. String will inhert `mode-line` / `mode-line-inactive` face initially.
  ;;   2. Then I call `propertize` to attace faces to some string, properties
  ;;      added via those faces have higher priority.
  ;;   3. You can use the third `face` argument to attach faces at last:
  ;;     1. Integer value clears all properties.
  ;;     2. You can pass a face here to cover properties, which means properties
  ;;        of this face have higher priority then I added via `propertize`.
  ;;     3. `nil` means don't cover properties here.
  ;; The doc says `format-mode-line` only attach face to characters has no face.
  ;; I guess it only means `(:propertize ELT PROPS...)` and treats string
  ;; returned by `propertize` as no face.
  ;; What I want is:
  ;;   1. Inherit `mode-line` / `mode-line-inactive` from theme.
  ;;   2. Add different colors to segments.
  ;;   3. If window is not selected, all color should covered by
  ;;     `mode-line-inactive`, so I can easily see which window is selected.
  ;; (Well, my theme only has color for `mode-line` / `mode-line-inactive`.)
  ;; 1 and 2 are easy, for 3, if you pass `nil` to `format-mode-line`, it won't
  ;; remove color when window is inactive. But if pass `'mode-line-inactive` or
  ;; `t`, you will lose color in active window, because `mode-line` /
  ;; `mode-line-inactive` both have colors and will cover your color. So the
  ;; solution is set it to `nil` when window is selected, and set it to
  ;; `mode-line-inactive` when window is not selected (doc says you can pass `t`
  ;; but you will get errors), `mode-line-window-selected-p` is useful to this.
  (setq-default mode-line-format
                '((:eval
                   (alynx-mode-line--align
                    ;; Left.
                    (format-mode-line
                     '((:eval (alynx-mode-line--segment-buffer-status))
                       (:eval (alynx-mode-line--segment-buffer-name))
                       (:eval (alynx-mode-line--segment-cursor-position)))
                     (if (mode-line-window-selected-p) nil 'mode-line-inactive))
                    ;; Right.
                    (format-mode-line
                     '((:eval (alynx-mode-line--segment-indentation-style))
                       (:eval (alynx-mode-line--segment-eol-style))
                       (:eval (alynx-mode-line--segment-encoding))
                       (:eval (alynx-mode-line--segment-major-mode))
                       (:eval (alynx-mode-line--segment-vc))
                       (:eval (alynx-mode-line--segment-checker))
                       (:eval (alynx-mode-line--segment-process))
                       (:eval (alynx-mode-line--segment-misc-info)))
                     (if (mode-line-window-selected-p) nil 'mode-line-inactive)))))))

(defun alynx-mode-line--deactivate ()
  "Deactivate alynx-mode-line."

  ;; Remove flycheck hooks.
  (remove-hook 'flycheck-status-changed-functions
               #'alynx-mode-line--checker-flycheck-update-segment)
  (remove-hook 'flycheck-mode-hook
               #'alynx-mode-line--checker-flycheck-update-segment)

  ;; Remove flymake hooks.
  (advice-remove 'flymake-start
                 #'alynx-mode-line--checker-flymake-update-segment)
  (advice-remove 'flymake--handle-report
                 #'alynx-mode-line--checker-flymake-update-segment)

  ;; Remove VC hooks.
  (remove-hook 'file-find-hook
               #'alynx-mode-line--vc-update-segment)
  (remove-hook 'after-save-hook
               #'alynx-mode-line--vc-update-segment)
  (advice-remove 'vc-refresh-state
                 #'alynx-mode-line--vc-update-segment)

  ;; Restore the original value of `mode-line-format`, we still only handle
  ;; global value.
  (setq-default mode-line-format alynx-mode-line--original-mode-line))

;;;###autoload
(define-minor-mode alynx-mode-line-mode
  "Toggle alynx-mode-line on or off."
  :group 'alynx-mode-line
  :global t
  :lighter nil
  :init-value nil
  (if alynx-mode-line-mode
      (alynx-mode-line--activate)
    (alynx-mode-line--deactivate)))

(provide 'alynx-mode-line)

;;; alynx-mode-line.el ends here.
