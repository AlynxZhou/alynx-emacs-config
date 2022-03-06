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

;; Initial GUI states, put here to prevent it become large from a small window.

;; Start every frame maximized.
;; See <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Initial package settings, put here because package manager are called before
;; `init.el`.
;; See <https://www.masteringemacs.org/article/whats-new-in-emacs-27-1#startup-changes-in-emacs-27.1>.

;; Since Emacs 27, `package-initialize` is called automatically before `init.el`
;; is loaded, but after `early-init.el`. I don't do this, I call it manually
;; after setting `package-archives` to mirror, so prevent this behavior.
;; Maybe I'll replace internal package manager with `straight` in future,
;; it seems better for configurations-focused
(setq package-enable-at-startup nil)
;; Or maybe you can put custom `package-archives` in `early-init.el`, but I
;; don't do this. Because if you let Emacs call `package-initialize`
;; automatically, it will also call `package-refresh-contents` if you have
;; custom `package-archives`, but I want to handle this by `use-package` and
;; only call `package-refresh-contents` if `use-package` is not installed.
;; See <https://emacs.stackexchange.com/questions/38368/how-can-i-improve-startup-time-despite-many-packages>.
;; (setq package-archives
;;       '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; early-init.el ends here.
