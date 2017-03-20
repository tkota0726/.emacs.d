;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;; Emacs設定用ファイル
;; timestamp:2017-03-19
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
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))

;;== List1:パッケージを使うための初期設定
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
;;
;;swoop設定
(require 'swoop)
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "C-M-o") 'swoop-multi)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match

;;検索の移行
;;isearch     > press [C-o] > swoop
;;evil-search > press [C-o] > swoop
;;swoop       > press [C-o] > swoop-multi
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-search)
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)
;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)
;;nertreeキーバインド
(global-set-key [f8] 'neotree-toggle)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;;;;;;;;;;;;;;
;;helmの設定
;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;;;;;;;
;;helm-mini設定
;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "M-m") 'helm-mini)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;helm補完機能
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;;jedi用設定
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
;;(add-to-list 'company-backends 'company-jedi) ; backendに追加
;;emacs-bash補完機能
(require 'bash-completion)
(bash-completion-setup)

;;現在行をハイライト
;;(global-hl-line-mode t)

;;選択範囲をハイライト
;;(transient-mark-mode t)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;undo-treesetting
(global-undo-tree-mode)
(require 'undo-tree)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-migemo helm-mode-manager volatile-highlights undo-tree sws-mode swoop package-utils neotree jedi-direx isearch-symbol-at-point isearch-prop isearch-dabbrev isearch+ imenus imenu-list imenu-anywhere highlight-symbol help-mode+ help-fns+ help+ helm-helm-commands helm-anything helm-ag-r helm-ag eshell-z eshell-up eshell-prompt-extras eshell-git-prompt eshell-fringe-status eshell-fixed-prompt eshell-did-you-mean eshell-autojump cl-lib-highlight cl-generic cl-format bash-completion auto-install auto-highlight-symbol auto-complete-nxml))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;neotree設定
(global-set-key [f8] 'neotree-toggle)

