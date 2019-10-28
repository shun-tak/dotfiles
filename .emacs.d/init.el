;;; キーバインド
;; C-hでバックスペース
(keyboard-translate ?\C-h ?\C-?)

;;; 基本
;; grep
(define-key global-map (kbd "C-x g") 'grep)
;; 次のウィンドウへ移動
(define-key global-map (kbd "C-M-n") 'next-multiframe-window)
;; 前のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window)
;; 定義へ移動
;; C-x F -> 関数定義へ移動
;; C-x K -> キーにバインドされている関数定義へ移動
;; C-x V -> 変数定義へ移動
(find-function-setup-keys)

;;; grep
;; 再帰的にgrep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;; 画像
;; 画像ファイルを表示
(auto-image-file-mode t)

;;; バー
;; メニューバーを消す
(menu-bar-mode nil)

;;; 括弧
;; 対応する括弧を光らせる。
(show-paren-mode t)
;; ウィンドウ内に収まらないときだけ括弧内も光らせる
(setq show-paren-style 'mixed)

;;; 位置
;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-new-lines nil)

;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
;; Emacs 24ではデフォルトで有効になっていて、`partial-completion-mode'は
;; なくなっている。カスタマイズする場合は以下の変数を変更する。
;;   * `completion-styles'
;;   * `completion-pcm-complete-word-inserts-delimiters'
(if (fboundp 'partial-completion-mode)
    (partial-completion-mode t))
;; 補完可能なものを随時表示
(icomplete-mode t)

;;; バッファ名
;; ファイル名が重複していたらディレクトリ名を追加する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 実行権
;; ファイルの先頭に#!...があるファイルを保存すると実行権をつける
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; 関数名
;; ウィンドウの上部に現在の関数名を表示
(which-function-mode t)

;;; ビープ音を消す
(setq visible-bell t)

;;; インデント
;; [TAB］キーでインデント実施
(setq c-tab-always-indent t)

;;; スペルチェック
;; ispellの代わりにaspellを使う
;; M-$で現在の単語のみスペルチェック
;; M-x ispell で文書全体のスペルチェック
(setq ispell-program-name "aspell")
;; スペルチェックには英語の辞書を使う
(setq ispell-dictionary "american")

;;; フォント
(when (>= emacs-major-version 23)
;; アスキー
(set-face-attribute 'default nil :family "Anonymous Pro" :height 140)
;; フォントサイズのリスケール
;; アスキーと日本語を等幅にする
(setq face-font-rescale-alist
      '(("Takao.*" . 1.2)
        ("Anonymous.*" . 1.0))))

;;; ウィンドウ
;; サイズ
(setq initial-frame-alist
      (append
       '((top . 0)    ; フレームの Y 位置(ピクセル数)
         (left . 0)    ; フレームの X 位置(ピクセル数)
         (width . 155)    ; フレーム幅(文字数)
         (height . 55)   ; フレーム高(文字数)
         ) initial-frame-alist))
;; カラーテーマ
(when (>= emacs-major-version 24)
  (load-theme 'tango-dark t))

;;; PATHの追加
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/usr/texbin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;; パッケージマネージャ
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
