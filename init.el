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
(global-set-key "\C-c\C-r" 'window-resizer)

; ------------------------------------------------------------------------
; auto-install
; http://www.emacswiki.org/emacs/download/auto-install.el
; ------------------------------------------------------------------------
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/auto-install/")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)   

;; ;; load-pathを追加する関数を定義
;; (defun add-to-load-path (&rest paths)
;;   (let (path)
;;     (dolist (path paths paths)
;;       (let ((default-derectory
;; 	      (expand-file-name (concat user-emacs-directory path))))
;; 	(add-to-list 'load-path default-directory)
;; 	(if (fboudp 'normal-top-level-add-subdirs-to-load-path)
;; 	    (normal-top-level-add-subdirs-to-load-path)))))))

;; ;;パス追加用
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-deferred")
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-epc")
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-ctable")
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-jedi")

;; ;;引数のディレクトリとそのサブディレクトリをload-pathに追加
;; (add-to-load-path "elisp" "conf" "public_repos")

;; ;;addhook
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; ;; ;===============
;; ;; ; package.elの設定
;; ;; ;===============
;; ;; (require 'package)
;; ;; (require 'package nil t
;; ;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; ;; (add-to-list 'package-archives'("ELPA" . "https://tromey.com/elpa/"))
;; ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; ;; (when (< emacs-major-version 24)
;; ;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;; ;; ; Initialize
;; ;; (package-initialize)
;; ;; ; melpa.el
;; ;; (require 'melpa)

;; ;; ;=====================
;; ;; ; jedi (package.elの設定より下に書く)
;; ;; ;=====================
;; ;; (require 'epc)
;; ;; (require 'auto-complete-config)
;; ;; (require 'auto-complete)
;; ;; (global-auto-complete-mode t)
;; ;; (require 'python)
;; ;; ;; python の設定
;; ;; (add-hook 'python-mode-hook '(lambda () (font-lock-mode 1)))
;; ;; (autoload 'python-mode "python-mode"
;; ;; "Major mode for editing Python programs" t)
;; ;; (setq auto-mode-alist
;; ;; (cons (cons "\\.py$" 'python-mode) auto-mode-alist))
;; ;; ;;;;; PYTHONPATH上のソースコードがauto-completeの補完対象になる ;;;;;
;; ;; (require 'jedi)
;; ;; (add-hook 'python-mode-hook '(lambda()(jedi:ac-setup)(setq jedi:complete-on-dot t)(local-set-key (kbd "M-TAB") 'jedi:complete)))
