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
;;  (mac-translate-from-yen-to-backslash)

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
;;  (setq default-input-method "MacOSX")  ;set-language-environmentよりもあとにないとだめ
  ;; モードラインの表示文字列
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
  ;; カーソルの色
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "red")
;  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.Roman" `cursor-color "blue")
  ;; ミニバッファを開いたときに英字にする（閉じてもモードは戻らない）
  ;;  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us))
  (mac-auto-ascii-mode 1))

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
;; package list
;; ----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(setq url-proxy-services '(("http" . "proxy-odc.intra.daifuku.co.jp:8080")))
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")

(package-initialize)

;; ----------------------------------------------------------------
;; line number
;; ----------------------------------------------------------------
(use-package linum
  :disabled
  :init
  (defvar linum-line-number 0)            ; 行移動を契機に描画
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
  (set-face-attribute 'linum nil :height 0.75))

(global-display-line-numbers-mode t)

;; ----------------------------------------------------------------
;; grep , ag
;; ----------------------------------------------------------------
(use-package ag
  :ensure t
  :defer t
  :bind (("M-g M-r" . ag)
         ("M-g M-f" . ag-regexp))
  :init
  (setq ag-highlight-search t)  ; 検索キーワードをハイライト
  (setq ag-reuse-buffers nil))  ; 検索ごとに新バッファを作る

; wgrep
(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup)
  :init
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (setq wgrep-auto-save-buffer t)       ; 編集完了と同時に保存
  (setq wgrep-enable-key "r"))          ; "r" キーで編集モードに

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
(use-package gtags
  :if (executable-find "gtags")
  :commands (gtags-mode)
  :init
  ;; to use gtags.el that shipped with gtags
  (let* ((_exec-path (replace-regexp-in-string ".exe" "" (executable-find "gtags")))
         (elisp-path (replace-regexp-in-string "/bin/" "/share/" _exec-path)))
    (add-to-list 'load-path elisp-path))
  (setq gtags-path-style 'relative)
  (add-hook 'c-mode-common-hook '(lambda() (gtags-mode 1)))
  (setq-default gtags-ignore-case nil)
  (setq gtags-auto-update 1)            ; gtags セーブでアップデート
  (defun my-select-tag-other-window ()
      (save-excursion (gtags-select-tag-other-window)))
  ;; 追加のtag search path
  (setenv "GTAGSLIBPATH" "C:/pro3/snap7/std;C:/qnx660/target")
  :bind ( :map gtags-mode-map
               ("M-r" . gtags-find-rtag)
               ("M-." . gtags-find-tag)
               ("M-g s" . gtags-find-symbol)
               ("M-g g" . gtags-find-pattern)
               ("M-g f" . gtags-find-file)
               ("M-," . gtags-pop-stack)
               ("M-*" . gtags-pop-stack)
               ("C-j" . my-select-tag-other-window)))
  

;; ---------------- rtags
(use-package rtags
  :if (and (executable-find "rdm")
           (executable-find "rc"))
  :init
  (setq rtags-display-result-backend 'helm)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

  (defun use-rtags (&optional useFileManager)
    (and (featurep 'rtags)
         (cond ((not (gtags-get-rootpath)) t)
               ((and (not (eq major-mode 'c++-mode))
                     (not (eq major-mode 'c-mode)))
                (rtags-has-filemanager))
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

  (add-hook 'c-mode-common-hook
            (lambda ()
              (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol))
              (define-key c-mode-base-map (kbd "M-r") (function tags-find-references))
              (define-key c-mode-base-map (kbd "M-,") (function tags-pop-stack))
              (define-key c-mode-base-map (kbd "M-*") (function tags-pop-stack))
              (define-key c-mode-base-map (kbd "M-g f") (function tags-find-file))
              (define-key c-mode-base-map (kbd "M-g v") (function rtags-find-virtuals-at-point))
              (define-key c-mode-base-map (kbd "M-g n") (function rtags-next-match))
              (define-key c-mode-base-map (kbd "M-g p") (function rtags-previous-match))
              )))

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
(use-package dummy-h-mode
  :ensure t
  :mode ("\\.h$" . dummy-h-mode))

;; ----------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------
(use-package helm
  :init
  :ensure t
  :init
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         )
  :config
  (require 'helm-config))

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
(use-package yasnippet
  :ensure t
  :init
  (use-package yasnippet-snippets
    :ensure t)
  :bind (:map yas-keymap
              ("<tab>" . nil)           ;companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
              )
  :mode ("emacs.+/snippets/" . snippet-mode)
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure t
  :init
  (setq helm-yas-space-match-any-greedy t)
  :bind (("C-c y" . 'helm-yas-complete)))
  
;; ----------------------------------------------------------------
;; company
;; ----------------------------------------------------------------
(use-package company
  :ensure t
  :init
  (setq company-idle-delay nil) ; 自動補完をしない
  :bind (("C-M-i" . 'company-complete)
         :map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous)
         ("<tab>" . 'company-complete-selection))
  :config
  (global-company-mode 1))

;; ----------------------------------------------------------------
;; irony
;; ----------------------------------------------------------------
(use-package irony
  :ensure t
  :commands (irony-mode)
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; Only needed on Windows
  (when system-windows-p (setq w32-pipe-read-delay 0))
  :bind ( :map irony-mode-map
               ([remap completion-at-point] . 'irony-completion-at-point-async)
               ([remap complete-symbol] .     'irony-completion-at-point-async)))


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
(use-package company-irony
  :ensure t
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

;; ----------------------------------------------------------------
;; company-irony-c-headers
;; ----------------------------------------------------------------
(use-package company-irony-c-headers
  :ensure t
  :init
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))

;; ----------------------------------------------------------------
;; irony eldoc
;; ----------------------------------------------------------------
(use-package irony-eldoc
  :ensure t
  :hook irony-mode)

;; ----------------------------------------------------------------
;; company tabnine
;; ----------------------------------------------------------------
(use-package company-tabnine
  :ensure t
  :init
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-tabnine)))

;; ----------------------------------------------------------------
;; aspell
;; ----------------------------------------------------------------
;; you may need "lang en_US" in ~/.aspell.conf 
(setq-default ispell-program-name "hunspell")
(setenv "DICTIONARY" "en_US")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

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
;; color-moccur  (ELPA)
;; ----------------------------------------------------------------
(use-package color-moccur
  :ensure t
  :defer t
  :bind (("M-o" . occur-by-moccur))
  :init
  :config
  (require 'moccur-edit))

;; ----------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; ----------------------------------------------------------------
;; flycheck-irony
;; ----------------------------------------------------------------
(use-package flycheck-irony
  :ensure t
  :init
  (eval-after-load 'flycheck
  '(progn
     (flycheck-irony-setup)
     (flycheck-add-next-checker 'irony 'c/c++-clang))))

(use-package flycheck-clang-analyzer
  :disabled                             ; rusticと愛性悪い
  :ensure t
  :init
  (eval-after-load 'flycheck
  '(progn
     (flycheck-clang-analyzer-setup))))

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
(use-package migemo
  :if (executable-find "cmigemo")
  :ensure t
  :init
  (let* ((_exec-path (replace-regexp-in-string "/bin/cmigemo" "/share/migemo/utf-8/migemo-dict" (executable-find "cmigemo")))
         (dict-path (replace-regexp-in-string ".exe" "" _exec-path)))
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))

    ;; Set your installed path
    (setq migemo-dictionary dict-path)

    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix))
  :config
  (migemo-init)
  ;; [migemo]isearch のとき IME を英数モードにする
  ;;      (when system-darwin-p
  ;;             (add-hook 'isearch-mode-hook 'mac-change-language-to-us)))))
  )

;; ----------------------------------------------------------------
;; highlight symbol
;; ----------------------------------------------------------------
(use-package highlight-symbol
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode) ; ソースコードにおいてM-p/M-nでシンボル間を移動
  (setq highlight-symbol-idle-delay 1.0)  ;1秒後自動ハイライトされるようになる
  :bind (("M-g h" . highlight-symbol-at-point)          ; ポイント位置のシンボルをハイライト
         ("M-g q" . highlight-symbol-query-replace)))

;; ----------------------------------------------------------------
;; beacon
;; ----------------------------------------------------------------
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

;; ----------------------------------------------------------------
;; scheme (gauche)
;; ----------------------------------------------------------------
(use-package gauche-mode
  :mode
  (("\\.scm$" . gauche-mode))
  :init
  (setq scheme-program-name (executable-find "gosh"))
  (setq file-coding-system-alist
      (cons '("gauche-refj\\.info.*\\'" utf-8 . utf-8)
            file-coding-system-alist))
  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))
  :bind
  (("\C-cs" . 'scheme-other-window)))

;; ----------------------------------------------------------------
;; kahua
;; ----------------------------------------------------------------
(use-package kahua
  :mode (("\\.kahua$" . kahua-mode))
  :init (setq kahua-site-bundle (expand-file-name "~/work/kahua/site")))

;; ----------------------------------------------------------------
;; google translate
;; ----------------------------------------------------------------
(use-package google-translate
  :ensure t
  :defer t
  :commands (google-translate-translate)
  :init
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
  :bind ("C-M-t". 'google-translate-enja-or-jaen)
  :config
  (require 'google-translate-default-ui)
  (use-package popwin
    :ensure t
    :init
    (push '("\*Google Translate\*" :height 0.5 :stick t) popwin:special-display-config)))
    
;; ----------------------------------------------------------------
;; rust
;; ----------------------------------------------------------------
(use-package rustic
  :ensure t
  :defer t
  :mode (("\\.rs$" . rustic-mode))
  :config
  (use-package lsp-mode
    :ensure t)
  (use-package racer
    :ensure t))

;; ----------------------------------------------------------------
;; hiwin
;; ----------------------------------------------------------------
(use-package hiwin
  :disabled
  :ensure t
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "gray80")) ;; 非アクティブウィンドウの背景色を設定

;; ----------------------------------------------------------------

(put 'narrow-to-region 'disabled nil)

;; ---------------- customset
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-tabnine racer rustic lsp-mode hiwin atom-one-dark-theme use-package yasnippet-snippets google-translate popwin flycheck-clang-analyzer helm-ag irony-eldoc wgrep-ag slime migemo highlight-symbol helm-gtags helm-c-yasnippet flycheck-irony ess dummy-h-mode company-irony-c-headers company-irony color-moccur beacon auto-highlight-symbol auto-complete ag)))


