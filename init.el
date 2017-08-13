;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;;; code:
(add-to-list 'load-path "~/.emacs.d/elisp")

;; ----------------------------------------------------------------
;; Determine system 
;; ----------------------------------------------------------------
(setq system-darwin-p (eq system-type 'darwin)
      system-windows-p (or (eq system-type 'windows-nt)
                           (eq system-type 'cygwin)))
;;
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)

;; ----------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'wheat t)
;(load-theme 'gnupack-dark t)

;; 選択バッファをわかりやすく表示
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "black" :background "orange"))))
 '(mode-line-buffer-id ((t (:foreground nil :background nil))))
 '(mode-line-inactive ((t (:foreground "gray50" :background "gray85")))))

;; ----------------------------------------------------------------
;; keybord bindings
;; ----------------------------------------------------------------
(cond
 (system-darwin-p
  ;; ¥の代わりにバックスラッシュを入力する
  (mac-translate-from-yen-to-backslash)

  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'nil)
  (setq x-select-enable-clipboard t))
 (system-windows-p
  ;; Altキーを使用せずにMetaキーを使用（有効：t、無効：nil）
  (setq w32-alt-is-meta t)))

;; ----------------------------------------------------------------
;; input method
;; ----------------------------------------------------------------
(cond
 (system-darwin-p
  ;; デフォルトIME
  (setq default-input-method "MacOSX")  ;set-language-environmentよりもあとにないとだめ
  ;; モードラインの表示文字列
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
  ;; カーソルの色
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "red")
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.Roman" `cursor-color "blue")
  ;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us))

 (system-windows-p
  ;; モードラインの表示文字列
  (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  ;; IME初期化
  (w32-ime-initialize)
  ;; デフォルトIME
  (setq default-input-method "W32-IME")
  ;; IME変更
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  ;; 漢字/変換キー入力時のエラーメッセージ抑止
  (global-set-key (kbd "<M-kanji>") 'ignore)
  (global-set-key (kbd "<kanji>") 'ignore)
  ;; minibufferのアクティブ時、IMEを無効化
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (deactivate-input-method)))
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  ;; IME無効／有効時のカーソルカラー定義
  (unless (facep 'cursor-ime-off)
    (make-face 'cursor-ime-off)
    (set-face-attribute 'cursor-ime-off nil
                        :background "DarkRed" :foreground "White")
    )
  (unless (facep 'cursor-ime-on)
    (make-face 'cursor-ime-on)
    (set-face-attribute 'cursor-ime-on nil
                        :background "DarkGreen" :foreground "White")
    )

  ;; IME無効／有効時のカーソルカラー設定
  (advice-add 'ime-force-on
              :before (lambda (&rest args)
                        (if (facep 'cursor-ime-on)
                            (let ( (fg (face-attribute 'cursor-ime-on :foreground))
                                   (bg (face-attribute 'cursor-ime-on :background)) )
                              (set-face-attribute 'cursor nil :foreground fg :background bg) )
                          )
                        ))
  (advice-add 'ime-force-off
              :before (lambda (&rest args)
                        (if (facep 'cursor-ime-off)
                            (let ( (fg (face-attribute 'cursor-ime-off :foreground))
                                   (bg (face-attribute 'cursor-ime-off :background)) )
                              (set-face-attribute 'cursor nil :foreground fg :background bg) )
                          )
                        ))
  ;; バッファ切り替え時の状態引継ぎ設定（有効：t、無効：nil）
  (setq w32-ime-buffer-switch-p t)
))

;; ----------------------------------------------------------------
;; font set
;; ----------------------------------------------------------------
(cond
 (system-darwin-p
  (create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
		    'unicode
		    (font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
		    nil
		    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))

 (system-windows-p
  ;; フォント
  ;; abcdefghijklmnopqrstuvwxyz 
  ;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
  ;; `1234567890-=\[];',./
  ;; ~!@#$%^&*()_+|{}:"<>?
  ;;
  ;; 壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五壱弐参四五
  ;; 123456789012345678901234567890123456789012345678901234567890
  ;; ABCdeＡＢＣｄｅ
  ;;
  ;; ┌─────────────────────────────┐
  ;; │　　　　　　　　　　　　　罫線                            │
  ;; └─────────────────────────────┘
  ;;

;;  (set-face-attribute 'default nil :family "Consolas" :height 90)
  (set-face-attribute 'default nil :family "Courier New" :height 90)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
  (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

  ;;
  ;; リストを評価する(Ctrl-j)
  ;;   フォントファミリ (pp (font-family-list))
  ;;   文字セット       (pp (charset-list))
  ;;   フェース         (pp (face-list))


  )
 (nil
  ;; デフォルト フォント
  ;; (set-face-attribute 'default nil :family "Migu 1M" :height 110)
  (set-face-font 'default "Migu 1M-11:antialias=standard")
  ;; プロポーショナル フォント
  ;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)
  (set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")
  ;; 等幅フォント
  ;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)
  (set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")
  ;; ツールチップ表示フォント
  ;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
  (set-face-font 'tooltip "Migu 1M-9:antialias=standard")
  ;; fontset
  ;; フォントサイズ調整
  (global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))
  ;; フォントサイズ リセット
  (global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))))
  

;; ----------------------------------------------------------------
;; global settings
;; ----------------------------------------------------------------

;;(setq Info-directory-list (list "/sw/share/info" "/sw/info" "/usr/share/info" "~/local/info" "~/local/share/info"))
;;(setq Info-additional-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info"))
(setq Info-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info" "/Users/masami/local/info" "/Users/masami/local/share/info" "/opt/local/share/info"))
 
(global-set-key "\M-l" 'goto-line)
(global-set-key "\C-xc" 'compile)

;; disable drag and drop in dired-mode
(setq dired-dnd-protocol-alist nil)
(global-set-key [ns-drag-file] 'ns-find-file)

; dabbrev-expand で大文字小文字の変換機能をオフにする
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

;検索で大文字考慮しない
(setq-default case-fold-search t)

(setq visible-bell nil)

(global-set-key "\M-o" 'universal-argument)

(show-paren-mode 1)
;; tool bar 非表示
(tool-bar-mode 0)
;; インデントは先頭のみ
(setq c-tab-always-indent nil)

;; 行番号の表示（有効：t、無効：nil）
(line-number-mode t)

;; 列番号の表示（有効：t、無効：nil）
(column-number-mode nil)

(global-set-key "\M-ga" 'align-regexp)

(set-cursor-color "Orange")
(blink-cursor-mode 1)

(setq use-dialog-box nil)
;; 
(setq recentf-max-saved-items 100)

(setq default-tab-width 4)

(setq line-move-visual nil)

(setq warning-suppress-types nil)
;; ----------------------------------------------------------------
;; backup
;; ----------------------------------------------------------------

(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;; ----------------------------------------------------------------
;; line number
;; ----------------------------------------------------------------
(require 'linum)

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示（有効：t、無効：nil）
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)

;; ----------------------------------------------------------------
;; grep , ag
;; ----------------------------------------------------------------
;Functions are autoloaded, so (require 'ag) is unnecessary.
(global-set-key "\M-g\M-r" 'ag)
(global-set-key "\M-g\M-f" 'ag-regexp)
(setq ag-highlight-search t)  ; 検索キーワードをハイライト
(setq ag-reuse-buffers nil)     ; 検索ごとに新バッファを作る

; wgrep
(add-hook 'ag-mode-hook '(lambda ()
                           (require 'wgrep-ag)
                           (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
                           (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
                           (wgrep-ag-setup)))

;; ----------------------------------------------------------------
;; package list
;; ----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;(setq url-proxy-services '(("http" . "proxy-odc.intra.daifuku.co.jp:8080")))
(package-initialize)

;; ----------------------------------------------------------------
;; lightning-paren
;; ----------------------------------------------------------------
(require 'lightning-paren)
(global-set-key "\C-c(" 'lightning-open-paren)
(global-set-key "\C-c[" 'lightning-open-paren)
(global-set-key "\C-c{" 'lightning-open-paren)
(global-set-key "\C-c)" 'lightning-close-paren)
(global-set-key "\C-c]" 'lightning-close-paren)
(global-set-key "\C-c}" 'lightning-close-paren)

;; ----------------------------------------------------------------
;; gtags, rtags
;; ----------------------------------------------------------------
(let ((_exec-path (executable-find "gtags")))
  (when _exec-path
    (let ((elisp-path (replace-regexp-in-string ".exe" ""
                                                (replace-regexp-in-string "/bin/" "/share/" _exec-path))))
      (add-to-list 'load-path elisp-path)
      (require 'gtags)
      (setq gtags-path-style 'relative)
      (add-hook 'c-mode-common-hook
                '(lambda()
                   (gtags-mode 1)))
      (setq gtags-mode-hook
            '(lambda ()
;               (define-key gtags-mode-map (kbd "M-r") 'gtags-find-rtag)
;               (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
;               (define-key gtags-mode-map (kbd "M-*") 'gtags-pop-stack)
;               (define-key gtags-mode-map (kbd "M-g s") 'gtags-find-symbol)
               (define-key gtags-mode-map (kbd "M-g g") 'gtags-find-pattern)
;               (define-key gtags-mode-map (kbd "M-g f") 'gtags-find-file)
;               (define-key gtags-select-mode-map (kbd "M-*") 'gtags-pop-stack)
;               (define-key gtags-select-mode-map (kbd "C-j") (lambda() (save-excursion (gtags-select-tag-other-window))))
               ))
      (setq-default gtags-ignore-case nil))))

;;; セーブ時にtagを自動更新
(defun my-c-mode-update-gtags ()
  (let* ((file (buffer-file-name (current-buffer)))
     (dir (directory-file-name (file-name-directory file))))
    (when (executable-find "global")
      (start-process "gtags-update" nil
             "global" "-uv"))))

(add-hook 'after-save-hook
      'my-c-mode-update-gtags)

;(setenv "GTAGSLIBPATH" "C:/pro3/snap5/std;C:/qnx660/target")
(setenv "GTAGSLIBPATH" "C:/pro3/snap7/std;C:/qnx660/target")

; ---------------- rtags
(when (and (executable-find "rdm")
           (executable-find "rc"))
  (require 'rtags)
  (setq rtags-display-result-backend 'helm)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running))  
  
(defun use-rtags (&optional useFileManager)
  (and (featurep 'rtags)
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun tags-find-symbol ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-tag)))
(defun tags-find-references ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
(defun tags-pop-stack ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-location-stack-back 'gtags-pop-stack)))
(defun tags-find-file ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))

(define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol))
(define-key c-mode-base-map (kbd "M-r") (function tags-find-references))
(define-key c-mode-base-map (kbd "M-*") (function tags-pop-stack))
(define-key c-mode-base-map (kbd "M-g f") (function tags-find-file))
(define-key c-mode-base-map (kbd "M-g v") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-g n") (function rtags-next-match))
(define-key c-mode-base-map (kbd "M-g p") (function rtags-previous-match))

(define-key global-map (kbd "M-.") (function tags-find-symbol))
(define-key global-map (kbd "M-r") (function tags-find-references))
(define-key global-map (kbd "M-*") (function tags-pop-stack))
(define-key global-map (kbd "M-g f") (function tags-find-file))
(define-key global-map (kbd "M-g v") (function rtags-find-virtuals-at-point))
(define-key global-map (kbd "M-g n") (function rtags-next-match))
(define-key global-map (kbd "M-g p") (function rtags-previous-match))

;; ----------------------------------------------------------------
;;  for SLIME
;; ----------------------------------------------------------------
(let ((sbcl-path (executable-find "sbcl")))
  (when sbcl-path
    (setq inferior-lisp-program sbcl-path)
    (setq slime-contribs '(slime-repl slime-asdf slime-fancy slime-banner))))

;; ----------------------------------------------------------------
;; c++ c mode
;; ----------------------------------------------------------------
(autoload 'vs-set-c-style "vs-set-c-style")

(add-hook 'c-mode-common-hook 'vs-set-c-style)

;; auto detect header file
(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)

;; ----------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------
(require 'helm)
(require 'helm-config)

;; key bindings
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;(global-set-key (kbd "C-x b")   'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-for-files)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "M-x")     'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action

;; ----------------------------------------------------------------
;; helm gtags
;; ----------------------------------------------------------------
;;(require 'helm-gtags)

;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)

;; (add-hook 'helm-gtags-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
;;              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;              (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
;;              (local-set-key (kbd "M-g g") 'helm-gtags-find-pattern)))

;; ----------------------------------------------------------------
;; yasnippet
;; ----------------------------------------------------------------
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
     (yas-global-mode 1)))
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)

;; ----------------------------------------------------------------
;; company
;; ----------------------------------------------------------------
(when (locate-library "company")
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (setq company-idle-delay nil) ; 自動補完をしない
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; ----------------------------------------------------------------
;; irony
;; ----------------------------------------------------------------
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's asynchronous function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Only needed on Windows
(when system-windows-p
  (setq w32-pipe-read-delay 0))

;; Example .clang_complete file: >
;;  -DDEBUG
;;  -include ../config.h
;;  -I../common
;;  -I/usr/include/c++/4.5.3/
;;  -I/usr/include/c++/4.5.3/x86_64-slackware-linux/
;; <

;; ----------------------------------------------------------------
;; compamy irony
;; ----------------------------------------------------------------
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; ----------------------------------------------------------------
;; company-irony-c-headers
;; ----------------------------------------------------------------
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; ----------------------------------------------------------------
;; irony eldoc
;; ----------------------------------------------------------------
(add-hook 'irony-mode-hook 'irony-eldoc)

;; ----------------------------------------------------------------
;; aspell
;; ----------------------------------------------------------------
;; you may need "lang en_US" in ~/.aspell.conf 
(setq-default ispell-program-name "hunspell")
(setenv "DICTIONARY" "en_US")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; ----------------------------------------------------------------
;; english/Japanese translator for programmers
;; ----------------------------------------------------------------
(require 'codic)

;; ----------------------------------------------------------------
;; eijiro
;; ----------------------------------------------------------------
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)

(eval-after-load "sdic"
  '(progn
     (setq sdicf-array-command (executable-find "sary")) ; コマンドパス
     (setq sdic-eiwa-dictionary-list
           '((sdicf-client "~/.emacs.d/dicts/eijirou.sdic" (strategy array)))
           sdic-waei-dictionary-list
           '((sdicf-client "~/.emacs.d/dicts/waeijirou.sdic" (strategy array))))

     ;; saryを直接使用できるように sdicf.el 内に定義されているarrayコマンド用関数を強制的に置換
     (fset 'sdicf-array-init 'sdicf-common-init)
     (fset 'sdicf-array-quit 'sdicf-common-quit)
     (fset 'sdicf-array-search
           (lambda (sdic pattern &optional case regexp)
             (sdicf-array-init sdic)
             (if regexp
                 (signal 'sdicf-invalid-method '(regexp))
               (save-excursion
                 (set-buffer (sdicf-get-buffer sdic))
                 (delete-region (point-min) (point-max))
                 (apply 'sdicf-call-process
                        sdicf-array-command
                        (sdicf-get-coding-system sdic)
                        nil t nil
                        (if case
                            (list "-i" pattern (sdicf-get-filename sdic))
                          (list pattern (sdicf-get-filename sdic))))
                 (goto-char (point-min))
                 (let (entries)
                   (while (not (eobp)) (sdicf-search-internal))
                   (nreverse entries))))))

     (defadvice sdic-forward-item (after sdic-forward-item-always-top activate)
       (recenter 0))
     (defadvice sdic-backward-item (after sdic-backward-item-always-top activate)
       (recenter 0))))

(setq sdic-default-coding-system 'utf-8-unix)


;; ----------------------------------------------------------------
;; searchweb
;; ----------------------------------------------------------------
(require 'search-web)

;; ----------------------------------------------------------------
;; color-moccur  (ELPA)
;; ----------------------------------------------------------------
(require 'color-moccur)
(global-set-key (kbd "M-o") 'occur-by-moccur)
(setq moccur-split-word t)

;; ----------------------------------------------------------------
;; moccur-edit
;; ----------------------------------------------------------------
(require 'moccur-edit)

;; ----------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ----------------------------------------------------------------
;; flycheck-irony
;; ----------------------------------------------------------------
(eval-after-load "flycheck"
  '(progn
     (when (locate-library "flycheck-irony")
       (flycheck-irony-setup))))
(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup))
;; ----------------------------------------------------------------
;; shell
;; ----------------------------------------------------------------
(cond
 (system-darwin-p
  )
 (system-windows-p
  (setq explicit-shell-file-name "bash.exe")
  (setq shell-command-switch "-c")
  (setq shell-file-name "bash.exe")
  ;; (setq explicit-bash.exe-args '("--login" "-i"))

  ;; (M-! and M-| and compile.el)
  (setq shell-file-name "bash.exe")
  (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)))

;; ----------------------------------------------------------------
;; migemo
;; ----------------------------------------------------------------
(let ((_exec-path (executable-find "cmigemo")))
  (when _exec-path
    (let ((dict-path (replace-regexp-in-string ".exe" ""
                                               (replace-regexp-in-string "/bin/cmigemo" "/share/migemo/utf-8/migemo-dict" _exec-path))))

      (require 'migemo)
      (setq migemo-command "cmigemo")
      (setq migemo-options '("-q" "--emacs"))

      ;; Set your installed path
      (setq migemo-dictionary dict-path)

      (setq migemo-user-dictionary nil)
      (setq migemo-regex-dictionary nil)
      (setq migemo-coding-system 'utf-8-unix)
      (migemo-init)
      ;; [migemo]isearch のとき IME を英数モードにする
      (when system-darwin-p
             (add-hook 'isearch-mode-hook 'mac-change-language-to-us)))))

;; ----------------------------------------------------------------
;; highlight symbol
;; ----------------------------------------------------------------
(require 'highlight-symbol)
;;; 1秒後自動ハイライトされるようになる
(setq highlight-symbol-idle-delay 1.0)
;;; 自動ハイライトをしたいならば
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;;; ソースコードにおいてM-p/M-nでシンボル間を移動
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
;;; ポイント位置のシンボルをハイライト
(global-set-key (kbd "M-g h") 'highlight-symbol-at-point)

(global-set-key (kbd "M-g q") 'highlight-symbol-query-replace)

;; ----------------------------------------------------------------
;; expand region
;; ----------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; ----------------------------------------------------------------
;; beacon
;; ----------------------------------------------------------------
(beacon-mode 1)

;; ----------------------------------------------------------------
;; scheme (gauche)
;; ----------------------------------------------------------------
(autoload 'gauche-mode "gauche-mode" nil t)

(setq auto-mode-alist (cons '("\\.scm$" . gauche-mode) auto-mode-alist))

(setq scheme-program-name (executable-find "gosh"))

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(global-set-key  "\C-cs" 'scheme-other-window)

(setq file-coding-system-alist
      (cons '("gauche-refj\\.info.*\\'" utf-8 . utf-8)
            file-coding-system-alist))
;; ----------------------------------------------------------------
;; kahua
;; ----------------------------------------------------------------
(require 'kahua)
(setq auto-mode-alist
      (append '(("\\.kahua$" . kahua-mode)) auto-mode-alist))
(setq kahua-site-bundle (expand-file-name "~/work/kahua/site"))

;; ----------------------------------------------------------------
;; R
;; ----------------------------------------------------------------
;(require 'ess-site)

;; (fset 'change-to-maxtry
;;       [?\C-  ?\C-s tab ?\C-s ?\C-s ?\C-s return ?\C-w ?m ?a ?x ?t ?r ?y ?\( ?\" ?\C-s ?\C-s ?\C-b ?\" ?, ?\C-  ?\C-s ?\C-s ?\C-s return ?\C-w ?\" ?\C-s ?\C-s ?\C-b ?\" ?, ?1 ?0 ?\) ?\C-k ])

;; (defun change-to-maxtry-region (top bottom)
;;   (interactive "r")
;;   (apply-macro-to-region-lines top bottom (symbol-function 'change-to-maxtry)))
   
;; ----------------------------------------------------------------
;; google translate
;; ----------------------------------------------------------------
(require 'google-translate)
(require 'google-translate-default-ui)

(defvar google-translate-english-chars "[:ascii:]"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(require 'popwin)
(push '("\*Google Translate\*" :height 0.5 :stick t) popwin:special-display-config)

(global-set-key (kbd "C-M-t") 'google-translate-enja-or-jaen)

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-clang-analyzer popwin google-translate company-irony wgrep-ag search-web migemo irony-eldoc highlight-symbol helm-c-yasnippet flycheck-irony expand-region dummy-h-mode company-irony-c-headers color-moccur codic beacon ag))))
