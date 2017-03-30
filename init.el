;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;; Emacs設定用ファイル
;; timestamp:2017-03-19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/emacs.d/elisp　ディレクトリをロードパスに追加する
(add-to-list 'load-path "~/.emacs.d/elisp")

;; 削除をC-hに変更
(keyboard-translate ?\C-h ?\C-?)
;; ロックファイルを作成しない
(setq create-lockfiles nil)
;; カラム番号を表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; バッテリー残量を表示
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

;;auto-install
;;-----------------------------------------------------------
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
;;; C-u C-sでswoop-migemo
(defun isearch-forward-or-swoop-migemo (use-swoop)
  (interactive "P")
  (let (current-prefix-arg)
    (call-interactively (if use-swoop 'swoop-migemo 'isearch-forward))))
(global-set-key (kbd "C-s") 'isearch-forward-or-swoop-migemo)
;;; isearchからC-oでswoopへ移行
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
;;; swoop対象バッファのフォントを小さくする
(setq swoop-font-size: 0.5)


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
(require 'python-mode)
(require 'epc)
(require 'jedi-core)
(require 'jedi)
(require 'python)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;(add-to-list 'company-backends 'company-jedi) ; backendに追加
;;emacs-bash補完機能
(require 'bash-completion)
(bash-completion-setup)

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
    (flymake-php flymake-phpcs helm-phpunit helm-proc helm-project-persist helm-projectile helm-prosjekt helm-pt helm-purpose helm-pydoc helm-qiita helm-rage helm-rails helm-rb php+-mode quickrun flymake-python-pyflakes python python-test python-mode helm-swoop auto-package-update company-jedi format-sql jedi jedi-core migemo helm-migemo helm-mode-manager volatile-highlights undo-tree sws-mode swoop package-utils neotree jedi-direx isearch-symbol-at-point isearch-prop isearch-dabbrev isearch+ imenus imenu-list imenu-anywhere highlight-symbol help-mode+ help-fns+ help+ helm-helm-commands helm-anything helm-ag-r helm-ag eshell-z eshell-up eshell-prompt-extras eshell-git-prompt eshell-fringe-status eshell-fixed-prompt eshell-did-you-mean eshell-autojump cl-lib-highlight cl-generic cl-format bash-completion auto-install auto-highlight-symbol auto-complete-nxml))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;neotree設定
(global-set-key [f8] 'neotree-toggle)
;;C: ルートディレクトリの変更
;;c, +, p: ファイル作成
;;d: ファイル削除
;;r: ファイル名変更
;;e: ディレクトリを開く
;--------------

;; "C-c"でウィンドウを切り替える。
(global-set-key (kbd "C-<C-left>")  'windmove-left)
(global-set-key (kbd "C-<C-down>")  'windmove-down)
(global-set-key (kbd "C-<C-up>")    'windmove-up)
(global-set-key (kbd "C-<C-right>") 'windmove-right)

;;pyflakesの設定
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pyflakes"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
; show message on mini-buffer
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)

;;quickrun実行ショートカット設定
(defun my-quickrun-output-fix ()
  (interactive)
  (quickrun)
  (sit-for 0.5)
  (beginning-of-buffer)
  (sit-for 0.5)
  (end-of-buffer)
  )

(global-set-key (kbd "C-\\") 'my-quickrun-output-fix)
;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (if (buffer-file-name)
          (format "%%f - Emacs")
        (format "%%b - Emacs")))
;;terminalのパスをeshellに同期
;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))
