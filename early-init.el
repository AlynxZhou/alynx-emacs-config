;;; early-init.el --- Alynx's early-init configurations.

;;; Commentary:
;; This file will be run before GUI start, only put configurations that really
;; need to be done before GUI here.

;;; Code:

;; Initial memory tweaks, put here to reduce more garbage collections.

;; Make startup faster by reducing the frequency of garbage collections.
;; The default is 800 KB which is too small. Measured in bytes.
;; See <http://blog.lujun9972.win/emacs-document/blog/2019/03/15/%E9%99%8D%E4%BD%8Eemacs%E5%90%AF%E5%8A%A8%E6%97%B6%E9%97%B4%E7%9A%84%E9%AB%98%E7%BA%A7%E6%8A%80%E6%9C%AF/index.html>.
;; Also good for `lsp-mode`.
;; See <https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold>.
(setq gc-cons-threshold (* 128 1024 1024))
;; See <https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process>.
(setq read-process-output-max (* 2 1024 1024))
;; Maybe not good for `lsp-mode`.
;; Make GC pauses faster by decreasing the threshold after loading packages.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 2 1024 1024))))

;; Initial GUI states, put here to prevent it flash in color or shape.

;; Set colors before window showing up to avoid white screen flash, those values
;; match atom-one-dark theme. Should keep updated with theme.
;; See <https://github.com/jonathanchu/atom-one-dark-theme/blob/master/atom-one-dark-theme.el#L36-L37>.
;; Should only used if `(display-grapic-p)` is `t`, but I am not sure why it not
;; work with that. Anyway, I don't use Emacs in terminal.
(set-face-attribute 'default nil
                    :foreground "#ABB2BF"
                    :background "#282C34")
;; Hide default mode line until `mood-line` is loaded, because the default style
;; is ugly and it's useless during startup.
(setq-default mode-line-format nil)
;; Start every frame maximized.
;; See <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Initial package settings, put here because package manager are called before
;; `init.el`.
;; See <https://www.masteringemacs.org/article/whats-new-in-emacs-27-1#startup-changes-in-emacs-27.1>.

;; Since Emacs 27, `package-activate-all` is called automatically before
;; `init.el` is loaded, but after `early-init.el`. I prefer to call it
;; explicitly, and prevent this behavior here before the automatically calling.
(setq package-enable-at-startup nil)

;;; early-init.el ends here.
