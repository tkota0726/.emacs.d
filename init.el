;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;; Emacs設定用ファイル
;; timestamp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/emacs.d/elisp　ディレクトリをロードパスに追加する
(add-to-list 'load-path "~/.emacs.d/elisp")
;; "C-t"でウィンドウを切り替える。
(define-key global-map (kbd "C-t") 'other-window)
;; 削除をC-hに変更
(keyboard-translate ?\C-h ?\C-?)
;; ロックファイルを作成しない
(setq create-lockfiles nil)
;; カラム番号を表示
(column-number-mode t)
;; ファイルサイズを表示
;; バッテリー残量を表示
(size-indication-mode t)
(display-battery-mode t)
;;行番号表示
(require 'linum)
(global-linum-mode 1)
;;閉じカッコの自動挿入
(electric-pair-mode 1)
;; 選択領域の色
(show-paren-mode t)
;; bkファイルは残さない
(setq make-backup-files nil)
;;分割画面サイズの変更
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
	       (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key "\C-c\C-b" 'window-resizer)
;;-----------------------------------------------------------
;;auto-install
;;http://www.emacswiki.org/emacs/download/auto-install.el
;;-----------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)   

; Emacs24
;;(auto-install-from-url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/emacs-lisp/package.el")

;;== List1:パッケージを使うための初期設定
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
;;==
;;;;;;;;;;;;;;
;;helmの設定
;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;
;;helm-mini設定
;;;;;;;;;;;;;;;;;;
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let ((cmds))
                    (mapatoms
                     (lambda (elt) (when (commandp elt) (push elt cmds))))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates (lambda ()
                  (let ((cmds))
                    (dolist (elem extended-command-history)
                      (push (intern elem) cmds))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands)))
 '(package-selected-packages
   (quote
    (sws-mode swoop volatile-highlights auto-highlight-symbol highlight-symbol helm-ag helm-ag-r helm-anything eshell-autojump eshell-did-you-mean eshell-fixed-prompt eshell-fringe-status eshell-git-prompt eshell-prompt-extras eshell-up eshell-z bash-completion imenu-anywhere imenus package-utils imenu-list jedi helm auto-install auto-complete-nxml))))
(define-key global-map (kbd "M-m") 'helm-mini)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;;helm補完機能
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;;jedi用設定
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;emacs-bash補完機能
(require 'bash-completion)
(bash-completion-setup)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236")))))
;;shell-mode前方一致検索
(add-hook 'shell-mode-hook
          '(lambda ()
             (progn
               ;; (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
               ;; (define-key shell-mode-map (kbd "C-n") 'comint-next-input)
               (define-key shell-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
               (define-key shell-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
               )))
;;現在行をハイライト
(global-hl-line-mode t)

;;選択範囲をハイライト
(transient-mark-mode t)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; (require 'highlight-symbol) ;; Cask や package-install からの場合はauto-loads cookie があるから不要
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)


(require 'swoop)
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "C-M-o") 'swoop-multi)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match
;; 検索の移行
;; isearch     > press [C-o] > swoop
;; evil-search > press [C-o] > swoop
;; swoop       > press [C-o] > swoop-multi
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
;; (define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)
;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)
;;html補完機能用
(add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\html?$" . html-helper-mode) auto-mode-alist))
