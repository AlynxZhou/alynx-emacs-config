;;; early-init.el --- Alynx's early-init configurations. -*- lexical-binding: t; -*-

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
;; 16 MB is the default value of doom-emacs.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 16 1024 1024))))

;; Initial GUI states, put here to prevent it flash in color or shape.

;; Set colors before window showing up to avoid white screen flash, those values
;; match atom-one-dark theme. Should keep updated with theme.
;; See <https://github.com/jonathanchu/atom-one-dark-theme/blob/master/atom-one-dark-theme.el#L36-L37>.
;; Should only used if `(display-graphic-p)` is `t`, but I am not sure why it
;; does not work with that. Anyway, I don't use Emacs in terminal.
;; See <https://github.com/radian-software/radian/blob/develop/emacs/early-init.el#L16-L23>.
;; It seems that `display-graphic-p` has some bugs.
(set-face-attribute 'default nil
                    :foreground "#ABB2BF"
                    :background "#282C34")
;; Hide default mode line until `mood-line` is loaded, because the default style
;; is ugly and it's useless during startup.
(setq-default mode-line-format nil)
;; Start every frame maximized.
;; See <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>.
;; Well, `maximized` should be the correct value, but at least it does not work
;; with `emacsclient`. (It's in maximized mode, but size is incorrect.)
(modify-all-frames-parameters '((fullscreen . fullboth)
                                ;; Alpha background does not work will
                                ;; fullscreen.
                                ;; (alpha-background . 85)
                                (menu-bar-lines . 0)
                                (tool-bar-lines . 0)
                                (internal-border-width . 0)))
;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Initial package settings, put here because package manager are called before
;; `init.el`.
;; See <https://www.masteringemacs.org/article/whats-new-in-emacs-27-1#startup-changes-in-emacs-27.1>.

;; Since Emacs 27, `package-activate-all` is called automatically before
;; `init.el` is loaded, but after `early-init.el`. I prefer to call it
;; explicitly, and prevent this behavior here before the automatically calling.
(setq package-enable-at-startup nil)

;;; early-init.el ends here.
