;;; my-init.el --- My init.el  -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:


(add-to-list 'load-path "~/.emacs.d/elisp")

(setq package-check-signature nil)

;; ----------------------------------------------------------------
;; utility function
;; ----------------------------------------------------------------
(defun search-file-for-updir(dir filename)
  "DIRから親ディレクトリ方向へ向かってFILENAMEを探し、最初に見つけたディレクトリ名を返す。見つからなかったらnilを返す."
  (cond
   ((file-exists-p (expand-file-name filename dir))
    dir)
   ((equal (expand-file-name "/") dir)
    nil)
   (t
    (search-file-for-updir (file-name-directory (directory-file-name dir)) filename))))

(defun search-file-for-downdir(dir filename)
  "DIRから子ディレクトリ方向へ向かってFILENAMEを探し、最初に見つけたディレクトリ名を返す。見つからなかったらnilを返す."
  (if (file-directory-p dir)
      (catch 'found
        (message dir)
        (let ((files (reverse (directory-files-and-attributes dir nil nil nil))))
          (dolist (file files)
            (let* ((dirp (eq t (car (cdr file))))     ;directory?
                   (path (expand-file-name (car file) dir))
                   (fname (car file)))
              (cond
               ((or (equal fname ".") (equal fname ".."))
                nil)
               (dirp
                (if-let ((res (search-file-for-downdir path  filename))) (throw 'found res)))
               ((equal fname filename)
                (message "hit %s" dir)
                (throw 'found dir))
               (t
                nil))))))))

;; ----------------------------------------------------------------
;; Determine system
;; ----------------------------------------------------------------
(defvar system-darwin-p (eq system-type 'darwin))
(defvar system-windows-p (or (eq system-type 'windows-nt)
                             (eq system-type 'cygwin)))

(if system-darwin-p
    (if-let ((dir (search-file-for-downdir "/usr/local/Cellar/llvm" "clangd")))
        (setq exec-path (cons dir exec-path))))

(defvar c-mode-company-use-lsp (cond ((executable-find "clangd")
                                      'clangd)
                                     ((executable-find "ccls")
                                      'ccls)
                                     (t nil)))

(defvar narrowing-system 'ivy)

(defvar greping-system 'rg)

(defvar theme-selection 'doom)


;;
(set-language-environment 'Japanese)
(if system-windows-p
    (progn
      (prefer-coding-system 'utf-8-dos)
;      (set-file-name-coding-system 'cp932)
;      (set-keyboard-coding-system 'cp932)
      (set-terminal-coding-system 'utf-8)
      (set-coding-system-priority 'utf-8)
      (set-default 'buffer-file-coding-system 'utf-8-with-signature-dos)
      )
  (prefer-coding-system 'utf-8-unix))
  

;; ----------------------------------------------------------------
;; proxy
;; ----------------------------------------------------------------
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")

(cond ((getenv "HTTP_PROXY")
       (let* ((url_ (url-generic-parse-url (getenv "HTTP_PROXY")))
              (auth_ (if (and (url-user url_) (url-password url_) )
                         (base64-encode-string
                          (format "%s:%s" (url-user url_) (url-password url_)))
                       nil))
              (host_ (format "%s:%s" (url-host url_) (url-portspec url_))))

         (setq url-proxy-services
           (list (cons "no_proxy"  "^\\(localhost\\|10.*\\)")
                 (cons "http" host_)
                 (cons "https" host_)))
         (if auth_
             (setq url-http-proxy-basic-auth-storage
               (list (list host_ (cons "/" auth_)))))
         )))


;;----- mse proxy hack
;; (eval-after-load 'url-http
;;   '(defun url-https-proxy-connect (connection)
;;      (setq url-http-after-change-function 'url-https-proxy-after-change-function)
;;      (process-send-string connection (format (concat "CONNECT %s:%d HTTP/1.1\r\n"
;;                                                      "Host: %s:%d\r\n"
;;                                                      "User-Agent: %s\r\n"
;;                                                      "Proxy-Authorization: Basic %s\r\n"
;;                                                      "\r\n")
;;                                              (url-host url-current-object)
;;                                              (or (url-port url-current-object)
;;                                                  url-https-default-port)
;;                                              (url-host url-current-object)
;;                                              (or (url-port url-current-object)
;;                                                  url-https-default-port)
;;                                              (url-http--user-agent-default-string)
;;                                              (let* ((url_ (url-generic-parse-url (getenv "HTTP_PROXY")))
;;                                                     (auth_ (if (and (url-user url_) (url-password url_) )
;;                                                                (base64-encode-string
;;                                                                 (format "%s:%s" (url-user url_) (url-password url_))))))
;;                                                auth_)))))

;; ----------------------------------------------------------------
;; straight
;;  https://github.com/raxod502/straight.el
;; ----------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ----------------------------------------------------------------
;; leaf
;; ----------------------------------------------------------------
;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; Now you can use leaf!
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; customが出力するinit.el へのcustom-set-variables出力を変更する
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; ----------------------------------------------------------------
;; meta key bindings
;; ----------------------------------------------------------------
(cond
 (system-darwin-p
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'nil)
  (setq select-enable-clipboard t))
 (system-windows-p
  ;; Altキーを使用せずにMetaキーを使用（有効：t、無効：nil）
  (setq w32-alt-is-meta t)))

;; ----------------------------------------------------------------
;; input method
;; ----------------------------------------------------------------
(leaf *input-method
  :config
  ;; ---------------- mac
  (leaf *mac-ime
    :if system-darwin-p
    :config
    ;; C-\ でOSの入力モードを切り替える
    (defun my/toggle-input-method ()
      (interactive)
      (message "my/toggle-input-method")
      (if (string-match "\\.Roman$" (mac-input-source))
	  (progn
            (mac-select-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"))
	(progn
	  (mac-select-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping.Roman"))))

    (global-set-key "\C-\\" 'my/toggle-input-method)

    ;; モードラインにOSのIME状態を表示
    (defvar mode-line-ime-info nil)

    (setcdr (nthcdr 1 mode-line-format)
	    (cons 'mode-line-ime-info (nthcdr 2 mode-line-format)))

    (defun my/update-ime-info ()
      (if (string-match "\\.Roman$" (mac-input-source))
	  (setq mode-line-ime-info "[Aa]")
	(setq mode-line-ime-info "[こ]"))
      (force-mode-line-update))
    
    (add-hook 'mac-selected-keyboard-input-source-change-hook 'my/update-ime-info)

    (mac-auto-ascii-mode 1))

  ;; ---------------- win
  (leaf tr-ime
    :if system-windows-p
    :ensure t
    :config
    (tr-ime-advanced-install)
    (when (fboundp 'w32-ime-initialize)
      ;; モードラインの表示文字列
      (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
      (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
      ;; デフォルトIME
      (setq default-input-method "W32-IME")
      ;; IME初期化
      (w32-ime-initialize)
      ;; IME変更
      (global-set-key (kbd "C-\\") 'toggle-input-method)
      ;;    (global-set-key (kbd "<M-spc>") 'toggle-input-method)
      ;; 漢字/変換キー入力時のエラーメッセージ抑止
      (global-set-key (kbd "<M-kanji>") 'ignore)
      (global-set-key (kbd "<kanji>") 'ignore)
      ;; minibufferのアクティブ時、IMEを無効化
      (add-hook 'minibuffer-setup-hook
		(lambda ()
                  (deactivate-input-method)))
      (wrap-function-to-control-ime 'universal-argument t nil)
      (wrap-function-to-control-ime 'read-string nil nil)
      (wrap-function-to-control-ime 'read-char nil nil)
      (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
      (wrap-function-to-control-ime 'y-or-n-p nil nil)
      (wrap-function-to-control-ime 'yes-or-no-p nil nil)
      (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
      ;; バッファ切り替え時の状態引継ぎ設定（有効：t、無効：nil）
      (setq w32-ime-buffer-switch-p t)
      ))
  )


;; ----------------------------------------------------------------
;; font set
;; ----------------------------------------------------------------
(cond (system-darwin-p
       (custom-set-faces
	'(default ((t (:family "HackGenNerd" :foundry "nil" :slant normal :weight normal :height 141 :width normal))))))
       (system-windows-p
	(custom-set-faces
	 '(default ((t (:family "HackGenNerd" :foundry "outline" :slant normal :weight normal :height 90 :width normal)))))))

;; Options->setdefault font で HackGenNerd を選択して Options->save options
;;
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


;; ----------------------------------------------------------------
;; global settings
;; ----------------------------------------------------------------

;;(setq Info-directory-list (list "/sw/share/info" "/sw/info" "/usr/share/info" "~/local/info" "~/local/share/info"))
;;(setq Info-additional-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info"))
;;(setq Info-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info"
;;				"/Users/masami/local/info"
;;				"/Users/masami/local/share/info"
;;				"/opt/local/share/info"))
 
(global-set-key "\M-l" 'goto-line)
(global-set-key "\C-xc" 'compile)
(global-set-key "\M-ga" 'align-regexp)

(global-unset-key "\C-t")
(global-unset-key "\M-t")

;; disable drag and drop in dired-mode
(setq dired-dnd-protocol-alist nil)
(global-set-key [ns-drag-file] 'ns-find-file)

; dabbrev-expand で大文字小文字の変換機能をオフにする
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

;検索で大文字考慮しない
(setq-default case-fold-search t)
;ただしC-w したときは大文字考慮する
(setq search-upper-case t)

;折り返し行も含めて次行へ移動
(setq line-move-visual nil)

(setq visible-bell nil)

(show-paren-mode 1)
;; tool bar 非表示
(tool-bar-mode 0)
;; インデントは先頭のみ
(setq c-tab-always-indent nil)

;; ---------------- mode line
;; 行番号の表示（有効：t、無効：nil）
(line-number-mode t)
;; 列番号の表示（有効：t、無効：nil）
(column-number-mode nil)

;;---------------- cursor
;; カーソル行のハイライト
(global-hl-line-mode 1)

(set-cursor-color "orange")
(blink-cursor-mode 1)

(setq use-dialog-box nil)
;; 
(setq recentf-max-saved-items 100)

;(setq default-tab-width 4)

;; ----------------------------------------------------------------
;; backup
;; ----------------------------------------------------------------

(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))


;; ----------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------
(leaf doom-themes
  :if (eq theme-selection 'doom)
  :ensure t
  :custom-face
  ((yas-field-highlight-face . '((t (:inherit match :inverse-video t))))
   ;; 選択バッファをわかりやすく表示
   (mode-line . '((t (:foreground "black" :background "orange"))))
   (mode-line-buffer-id . '((t (:foreground nil :background nil))))
   (mode-line-inactive . '((t (:foreground "gray50" :background "gray85"))))
   (header-line . '((t (:foreground "#51afef" :background "#505662"))))
   ;; elispでのcompletion-at-point での選択表示が分かりにくいので変更
   (ivy-current-match . '((t :background "#1a4b77" :foreground "white"  t :extend t))))

  :custom
  ;; Global settings (defaults)
  ((doom-themes-enable-bold . t)    ; if nil, bold is universally disabled
   (doom-themes-enable-italic . t) ; if nil, italics is universally disabled
   (doom-themes-treemacs-theme .  "doom-colors")) ; use the colorful treemacs theme
  :config
  ;;;(load-theme 'doom-nord-light t)
  (load-theme 'doom-vibrant t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; ----------------------------------------------------------------
  ;; 選択Windowsを分かりやすくする
  ;; ----------------------------------------------------------------
  (defun highlight-selected-window ()
    "Highlight selected window with a different background color."
    (walk-windows (lambda (w)
                    (unless (eq w (selected-window))
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set 'default)))))
    (with-current-buffer (window-buffer (selected-window)) (buffer-face-set '(:background "#111"))))
  (add-hook 'buffer-list-update-hook 'highlight-selected-window))


(leaf nano
  :if (eq theme-selection 'nano)
  :straight
  (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :config
  (require 'nano))


(leaf all-the-icons
  :ensure t
  )
;; ----------------------------------------------------------------
;; line number
;; ----------------------------------------------------------------

(global-display-line-numbers-mode t)

;; ----------------------------------------------------------------
;; grep , ag, rg
;; ----------------------------------------------------------------
(leaf ag
  :if (eq greping-system 'ag)
  :ensure t
  :bind (("M-g M-r" . ag)
         ("M-g M-f" . ag-regexp))
  :custom
  ((ag-highlight-search . t)  ; 検索キーワードをハイライト
   (ag-reuse-buffers . t))  ; 検索ごとに新バッファを作る
  ;; wgrep
  :config
  (leaf wgrep-ag
    :ensure t
    :custom
    ((wgrep-auto-save-buffer . t)       ; 編集完了と同時に保存
     (wgrep-enable-key . "e"))))          ; "r" キーで編集モードに

(leaf rg
  :if (eq greping-system 'rg)
  :ensure t
  :bind (("M-g M-r" . rg)
         ("M-g M-f" . search-everything-at-project))
  :config
  ;; wgrep は "e" に割り当てすみ
  ;; "i" でignore無視して再検索
  (rg-define-search search-everything-at-project
    "Search files everything in project directory"
    :query ask
    :format regexp
    :files "everything"
    :dir ask)
  (rg-define-search search-everything-at-current
    "Search files everything in project directory"
    :query ask
    :format regexp
    :files "everything"
    :dir current)
  )

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
;; ワード削除はkill-bufferに入れない
;; ----------------------------------------------------------------
(defun my-kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-kill-word (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my-kill-word (- arg)))

(global-set-key "\M-d" 'my-kill-word)
(global-set-key [M-backspace] 'my-backward-kill-word)

;; ----------------------------------------------------------------
;; gtags
;; ----------------------------------------------------------------
;; 追加のtag search path
(setenv "GTAGSLIBPATH" "C:/pro3/snap7/std;C:/qnx660/target")

(leaf gtags
  :if (not narrowing-system)
  :commands (gtags-mode)
  :init
  ;; to use gtags.el that shipped with gtags
  (let* ((_exec-path (replace-regexp-in-string ".exe" "" (executable-find "gtags")))
         (elisp-path (replace-regexp-in-string "/bin/" "/share/" _exec-path)))
    (add-to-list 'load-path elisp-path))
  :hook
  (c-mode-common-hook . (lambda() (if (search-file-for-updir  buffer-file-name "GTAGS")   ;; 上位ディレクトリにGTAGSがあるなら
                                      (gtags-mode 1))))
  :config
  (setq gtags-path-style 'relative)
  (setq-default gtags-ignore-case nil)
  (setq gtags-auto-update 1)            ; gtags セーブでアップデート
  (defun my-select-tag-other-window ()
    (save-excursion (gtags-select-tag-other-window)))
  :bind (gtags-mode-map
         ("M-?" . gtags-find-rtag)
         ("M-." . gtags-find-tag)
         ("M-g s" . gtags-find-symbol)
         ("M-g g" . gtags-find-pattern)
         ("M-g f" . gtags-find-file)
         ("M-," . gtags-pop-stack)
         ("M-*" . gtags-pop-stack)
         ("C-j" . my-select-tag-other-window)))

(leaf helm-gtags
  :if (eq narrowing-system 'helm)
  :ensure t
  :hook
  (c-mode-common-hook . (lambda() (if (search-file-for-updir  buffer-file-name "GTAGS")   ;; 上位ディレクトリにGTAGSがあるなら
                                      (helm-gtags-mode 1))))
  :custom
  ((helm-gtags-ignore-case . nil)
   (helm-gtags-auto-update . 1)            ; gtags セーブでアップデート
   (helm-gtags-path-style . 'relative))

  :bind (helm-gtags-mode-map
         ("M-?" . helm-gtags-find-rtag)
         ("M-." . helm-gtags-find-tag)
         ("M-g s" . helm-gtags-find-symbol)
         ("M-g g" . helm-gtags-find-pattern)
         ("M-g f" . helm-gtags-find-files)
         ("M-," . helm-gtags-pop-stack)
         ("M-*" . helm-gtags-pop-stack)
         ("C-j" . helm-gtags-select-tag-other-window)))

(leaf counsel-gtags
  :if (eq narrowing-system 'ivy)
  :ensure t
  :hook
  (c-mode-common-hook . (lambda() (if (search-file-for-updir  buffer-file-name "GTAGS")   ;; 上位ディレクトリにGTAGSがあるなら
                                      (counsel-gtags-mode 1))))
  :custom
  ((counsel-gtags-ignore-case . nil)
   (counsel-gtags-update-interval-second . nil)            ; gtags セーブでアップデート
   (counsel-gtags-path-style . 'relative))

  :bind (counsel-gtags-mode-map
         ("M-?" . counsel-gtags-find-reference)
         ("M-." . counsel-gtags-find-definition)
         ("M-g s" . counsel-gtags-find-symbol)
         ("M-g f" . counsel-gtags-find-file)
         ("M-," . counsel-gtags-go-backward)
         ("M-*" . counsel-gtags-go-backward)))

;; ----------------------------------------------------------------
;;  for SLIME
;; ----------------------------------------------------------------
(leaf slime
  :if (executable-find "sbcl")
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-repl slime-asdf slime-fancy slime-banner)))

;; ----------------------------------------------------------------
;; c++ c mode
;; ----------------------------------------------------------------
(autoload 'vs-set-c-style "vs-set-c-style")

(add-hook 'c-mode-common-hook 'vs-set-c-style)

;; ----------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------
(leaf helm
  :if (eq narrowing-system 'helm)
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-for-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         (helm-map
          ("<tab>" . helm-execute-persistent-action))) ; rebind tab to run persistent action
  :config
  (require 'helm-config))

;; ----------------------------------------------------------------
;; ivy
;; ----------------------------------------------------------------
(leaf counsel
  :if (eq narrowing-system 'ivy)
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
;         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-c j" . counsel-imenu))
  :custom
  (
   ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
   (ivy-use-virtual-buffers . t)
   (ivy-virtual-abbreviate . 'full)
   ;; デフォルトで入力される ^ 前方マッチ記号を非表示
   (ivy-initial-inputs-alist . '((org-agenda-refile . "^")
				 (org-capture-refile . "^")
				 ;; (counsel-M-x . "^") ;; 削除．必要に応じて他のコマンドも除外する．
				 ;;(counsel-describe-function . "^")
				 ;;(counsel-describe-variable . "^")
				 (Man-completion-table . "^")
				 (woman . "^")))
   (lsp-imenu-sort-methods . '(position)))
  :config
  (leaf smex
    :ensure t
    :custom
    ((smex-history-length . 35)
     (smex-completion-method . 'ivy)))

  (leaf ivy-rich
    :ensure t
    :custom
    (ivy-rich-path-style . 'abbrev)
    :config
    (ivy-rich-mode 1)

    ;; use buffer-file-name and list-buffers-directory instead of default-directory
    ;; so that special buffers, e.g. *scratch* don't get a directory (we return nil in those cases)
    (defun ivy-rich--switch-buffer-directory (candidate)
      "Return directory of file visited by buffer named CANDIDATE, or nil if no file."
      (let* ((buffer (get-buffer candidate))
             (fn (buffer-file-name buffer)))
	;; if valid filename, i.e. buffer visiting file:
	(if fn
            ;; return containing directory
            (directory-file-name fn)
          ;; else if mode explicitly offering list-buffers-directory, return that; else nil.
          ;; buffers that don't explicitly visit files, but would like to show a filename,
          ;; e.g. magit or dired, set the list-buffers-directory variable
          (buffer-local-value 'list-buffers-directory buffer))))
    ;; override ivy-rich project root finding to use FFIP or to skip completely
    (defun ivy-rich-switch-buffer-root (candidate)
      ;; 1. changed let* to when-let*; if our directory func above returns nil,
      ;;    we don't want to try and find project root
      (when-let* ((dir (ivy-rich--switch-buffer-directory candidate)))
	(unless (or (and (file-remote-p dir)
			 (not ivy-rich-parse-remote-buffer))
                    ;; Workaround for `browse-url-emacs' buffers , it changes
                    ;; `default-directory' to "http://" (#25)
                    (string-match "https?://" dir))
          (cond
           ;; 2. replace the project-root-finding
           ;; a. add FFIP for projectile-less project-root finding (on my setup much faster) ...
           ((require 'find-file-in-project nil t)
            (let ((default-directory dir))
              (ffip-project-root)))
           ;; b. OR disable project-root-finding altogether
           (t "")
           ((bound-and-true-p projectile-mode)
            (let ((project (or (ivy-rich--local-values
				candidate 'projectile-project-root)
                               (projectile-project-root dir))))
              (unless (string= project "-")
		project)))
           ((require 'project nil t)
            (when-let ((project (project-current nil dir)))
              (car (project-roots project))))
         ))))))

;; ----------------------------------------------------------------
;; yasnippet
;; ----------------------------------------------------------------
(leaf yasnippet
  :ensure t
  :config
  (leaf yasnippet-snippets
    :ensure t)
  :bind
  :mode ("emacs.+/snippets/" . snippet-mode)
  :custom
  ;; time_t<tab>などでsnippet展開しないようにする
  (yas-key-syntaxes . '(yas-try-key-from-whitespace "w_.()" "w_." "w_"))
  :config
  (yas-global-mode 1))

(leaf helm-c-yasnippet
  :if (eq narrowing-system 'helm)
  :ensure t
  :custom
  (helm-yas-space-match-any-greedy . t)
  :bind (("C-c y" . helm-yas-complete)))

(leaf ivy-yasnippet
  :if (eq narrowing-system 'ivy)
  :ensure t
  :init
  :bind (("C-c y" . ivy-yasnippet)))

;; ----------------------------------------------------------------
;; company
;; ----------------------------------------------------------------
(leaf company
  :ensure t
  :custom
  (company-idle-delay . nil) ; 自動補完をしない
  :bind (("C-M-i" . company-complete)
         (company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection)))
  :config
  (global-company-mode 1))

;; ----------------------------------------------------------------
;; irony
;; ----------------------------------------------------------------
;; Example .clang_complete file: >
;;  -DDEBUG
;;  -include ../config.h
;;  -I../common
;;  -I/usr/include/c++/4.5.3/
;;  -I/usr/include/c++/4.5.3/x86_64-slackware-linux/
;; <
(leaf irony
  :if (not c-mode-company-use-lsp)
  :ensure t
  :hook
  (c++-mode-hook . irony-mode)
  (c-mode-hook . irony-mode)
  (objc-mode-hook . irony-mode)
  (irony-mode-hook . irony-cdb-autosetup-compile-options)
  ;; Only needed on Windows
  :config
  (when system-windows-p (setq w32-pipe-read-delay 0))

  ;; ----------------------------------------------------------------
  ;; compamy irony
  ;; ----------------------------------------------------------------
  (leaf company-irony
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))

  ;; ----------------------------------------------------------------
  ;; company-irony-c-headers
  ;; ----------------------------------------------------------------
  (leaf company-irony-c-headers
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))

  ;; ----------------------------------------------------------------
  ;; irony eldoc
  ;; ----------------------------------------------------------------
  (leaf irony-eldoc
    :ensure t
    :hook irony-mode-hook))

;; ----------------------------------------------------------------
;; company tabnine
;; ----------------------------------------------------------------
;; to install binary M-x company-tabnine-install-binary
(leaf company-tabnine
  :disabled t
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tabnine))

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
;; flycheck
;; ----------------------------------------------------------------
(leaf flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

;; ----------------------------------------------------------------
;; flycheck-irony
;; ----------------------------------------------------------------
(leaf flycheck-irony
  :if (not c-mode-company-use-lsp)
  :ensure t
  :after flycheck
  :config
  (flycheck-irony-setup)
  (flycheck-add-next-checker 'irony 'c/c++-clang))

(leaf flycheck-clang-analyzer
  :disabled t                             ; rusticと愛性悪い
  :ensure t
  :after flycheck
  :config
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
(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  :require t
  :custom
  ((migemo-command . "cmigemo")
   (migemo-options . '("-q" "--emacs"))
   (migemo-dictionary . `,(let* ((_exec-path (replace-regexp-in-string "/bin/cmigemo" "/share/migemo/utf-8/migemo-dict" (executable-find "cmigemo")))
			       (dict-path (replace-regexp-in-string ".exe" "" _exec-path)))
			  dict-path))
   (migemo-user-dictionary . nil)
   (migemo-regex-dictionary . nil)
   (migemo-coding-system . 'utf-8-unix))
  :config
  (migemo-init)
  ;; [migemo]isearch のとき IME を英数モードにする
  ;;      (when system-darwin-p
  ;;             (add-hook 'isearch-mode-hook 'mac-change-language-to-us)))))
  )

;; ----------------------------------------------------------------
;; highlight symbol
;; ----------------------------------------------------------------
(leaf highlight-symbol
  :ensure t
  :hook
  (prog-mode-hook . highlight-symbol-mode)
  (prog-mode-hook . highlight-symbol-nav-mode) ; ソースコードにおいてM-p/M-nでシンボル間を移動
  :custom
  (highlight-symbol-idle-delay . 1.0)  ;1秒後自動ハイライトされるようになる
  :bind (("M-g h" . highlight-symbol-at-point)          ; ポイント位置のシンボルをハイライト
         ("M-g q" . highlight-symbol-query-replace)))

;; ----------------------------------------------------------------
;; beacon
;; ----------------------------------------------------------------
(leaf beacon
  :ensure t
  :init
  (beacon-mode 1))

;; ----------------------------------------------------------------
;; scheme (gauche)
;; ----------------------------------------------------------------
(leaf gauche-mode
  :mode  (("\\.scm$" . gauche-mode))
  :config
  (setq scheme-program-name (executable-find "gosh"))
  (setq file-coding-system-alist (cons '("gauche-refj\\.info.*\\'" utf-8 . utf-8)
				       file-coding-system-alist))
  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))
  :bind
  (("C-c g" . scheme-other-window)))

;; ----------------------------------------------------------------
;; kahua
;; ----------------------------------------------------------------
(leaf kahua
  :mode (("\\.kahua$" . kahua-mode))
  :config
  (setq kahua-site-bundle (expand-file-name "~/work/kahua/site")))

;; ----------------------------------------------------------------
;; google translate
;; ----------------------------------------------------------------
(leaf google-translate
  :ensure t
  :commands (google-translate-translate)
  :bind ("C-M-t". google-translate-enja-or-jaen)
  :custom (google-translate-backend-method . 'curl)
  :config
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

  ;; To fix error: google-translate--search-tkk: Search failed: ",tkk:'"
  ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888
    (eval-after-load 'google-translate-tk
      '(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))
  (leaf popwin
    :ensure t
    :require t  ; need for append popwin:special-display-config
    :custom
    (push '("\*Google Translate\*" :height 0.5 :stick t) popwin:special-display-config))
  (leaf popup
    :ensure t))

    
;; ----------------------------------------------------------------
;; projectによってモードラインの色を変える
;; ----------------------------------------------------------------
;; projectのルートに .dir-locals.el を作成することにより可能
;; 例）　モードラインを黄色にして読み込み専用にする
;; ((nil . ((eval . (face-remap-add-relative 'mode-line :foreground "black" :background "yellow"))))
;;  (prog-mode . ((eval . (view-mode)))))


;; ----------------------------------------------------------------
;; lsp-ui
;; ----------------------------------------------------------------
;;clangd : プロジェクトのTOPに空の .clangd を作成することによりtagが使用できる

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-enable-snippet t)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 2)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (create-lockfiles nil)
  (lsp-prefer-capf  t)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (cond ((eq c-mode-company-use-lsp 'clangd)
         (setq lsp-clients-clangd-executable (executable-find "clangd"))
         (setq lsp-disabled-clients (list 'ccls)))
        ((eq c-mode-company-use-lsp 'ccls)
         (setq lsp-disabled-clients (list 'clangd))))
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  ((c-mode c++-mode objc-mode) . lsp))


(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width  60)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-diagnostic-max-lines 3)
  
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-peek-height 30)
  (lsp-ui-peek-list-width 30)
  (lsp-ui-peek-fontify 'always)
  
  :hook   (lsp-mode . lsp-ui-mode)
  :bind ( :map lsp-ui-mode-map
               ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . 'lsp-ui-peek-find-references)))

(use-package ccls
  :if (eq c-mode-company-use-lsp 'ccls)
  :ensure t
  :custom ((ccls-sem-highlight-method 'font-lock)
           (ccls-use-default-rainbow-sem-highlight))
  :hook
  ((c-mode c++-mode objc-mode) . lsp))


;; ----------------------------------------------------------------
;; swift
;; ----------------------------------------------------------------
(leaf lsp-sourcekit
  :ensure t
  :custom
  (lsp-sourcekit-executable . `,(string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))
  :config
  (leaf swift-mode
    :ensure t
    :hook (swift-mode-hook . lsp)))

;; ----------------------------------------------------------------
;; rust
;; ----------------------------------------------------------------
(leaf rustic
  :ensure t
  :custom
  :hook (rust-mode . lsp)
  :config
  (leaf cargo
    :ensure t
    :hook (rust-mode . cargo-minor-mode)))

;; ----------------------------------------------------------------
;; C-mode stuct enum の中ではコメントはdoxgenの後述コメントを使う
;; ----------------------------------------------------------------
(defun in-defun-p()
  (save-excursion
    (beginning-of-defun)
    (let ((word (thing-at-point 'word t)))
      (if (or (equal word "struct")
              (equal word "enum"))
          t
        nil))))

(defun my-ins-comment(arg)
  (interactive "*P")
  (if (and (not (use-region-p))
          (in-defun-p))
      (let ((comment-start "///< ")
            (comment-start-skip "///< ")
            (comment-end   ""))
        (comment-dwim arg))
    (comment-dwim arg)))


(add-hook 'c-mode-common-hook
      (lambda()
         ;; cc-mode内で定義されるキーバインド
         (define-key c-mode-base-map "\M-;" 'my-ins-comment)))
    
;; ----------------------------------------------------------------
;; DAP
;;  to start debugging M-x dap-debug
;; ----------------------------------------------------------------
(leaf dap-mode
  :if (executable-find "lldb-vscode")
  :ensure t
  :custom
  (dap-lldb-debug-program . `,(list (executable-find "lldb-vscode")))
  (dap-lldb-debugged-program-function . '(lambda() (read-file-name "Select file to debug.")))
  (dap-auto-configure-features . '(sessions locals controls tooltip))
  :config
  (require 'dap-lldb))

;; ----------------------------------------------------------------
;;  ace jump
;; ----------------------------------------------------------------
(leaf ace-jump-mode
  :ensure t
  :bind
  ("C-c SPC" . ace-jump-mode))

;; ----------------------------------------------------------------
;;  ace jump zap
;; ----------------------------------------------------------------
(leaf ace-jump-zap
  :ensure t
  :bind
  ("M-z" . ace-jump-zap-to-char))

;; ----------------------------------------------------------------
;;  swiper
;; ----------------------------------------------------------------
(leaf swiper
  :ensure t
  :config
  (defun my-swiper-all-from-swiper ()
    " Intended to be bound in `swiper-map'."
    (interactive)
    (ivy-exit-with-action
     (lambda (_)
       (swiper-all ivy-text))))

  (defun my-counsel-rg-from-swiper-all ()
    " Intended to be bound in `swiper-map'."
    (interactive)
    (ivy-exit-with-action
     (lambda (_)
       (counsel-rg ivy-text))))

  (defun my-swiper-from-counsel-rg ()
    " Intended to be bound in `swiper-map'."
    (interactive)
    (ivy-exit-with-action
     (lambda (_)
       (swiper ivy-text))))

  :custom
  (ivy-re-builders-alist . '((t . ivy--regex-ignore-order)))

  :bind
  (("C-c s" . swiper-isearch-thing-at-point)
   ("<S-tab>" . counsel-rg)
   (isearch-mode-map
    :package isearch
    ("C-j" . swiper-from-isearch))
   (swiper-map
    ("C-j" . my-swiper-all-from-swiper))
   (swiper-all-map
    ("C-j" . my-counsel-rg-from-swiper-all))
   (counsel-ag-map
    :package counsel
    ("C-j" . my-swiper-from-counsel-rg))))


;; ----------------------------------------------------------------
;;  camelcase snakecase
;; ----------------------------------------------------------------
(leaf string-inflection
  :ensure t
  :config
  (defun my-string-inflection-cycle-auto ()
    "foo_bar => FOO_BAR => FooBar => fooBar => foo_bar"
    (interactive)
    (string-inflection-insert
     (let ((str (string-inflection-get-current-word)))
       (cond
        ;; foo => FOO
        ((string-inflection-word-p str)
         (string-inflection-upcase-function str))
        ;; foo_bar => FOO_BAR
        ((string-inflection-underscore-p str)
         (string-inflection-upcase-function str))
        ;; FOO_BAR => FooBar
        ((string-inflection-upcase-p str)
         (string-inflection-pascal-case-function str))
        ;; FooBar => fooBar
        ;; Foo    => foo
        ((string-inflection-pascal-case-p str)
         (string-inflection-camelcase-function str))
        ;; foo-bar => foo_bar
        (t
         (string-inflection-underscore-function str))))))
  :bind
  ("M-u" . my-string-inflection-cycle-auto))
  
;; ----------------------------------------------------------------
;;  universal mark
;; ----------------------------------------------------------------
(leaf universal-mark
  :commands universal-mark-mode
  :init
  (universal-mark-mode t)
  :config
  (universal-mark-advice-add 'isearch-forward)
  (universal-mark-advice-add 'isearch-backward)
  (eval-after-load 'swiper '(progn
			      (universal-mark-advice-add 'swiper-isearch-thing-at-point)
			      (universal-mark-advice-add 'swiper-all-thing-at-point )))
  (eval-after-load 'cousel '(universal-mark-advice-add 'counsel-rg ))
  (eval-after-load 'ace-jump-mode '(universal-mark-advice-add 'ace-jump-mode ))
  :bind
  ("M-," . universal-mark-previous-location)
  )
  
;; ----------------------------------------------------------------
;;  rainbow mode
;; ----------------------------------------------------------------
(leaf rainbow-mode
  :ensure t
  :hook css-mode-hook less-mode-hook web-mode-hook html-mode-hook emacs-lisp-mode-hook)

;; ----------------------------------------------------------------
;;  arg付きのsplit-windowやother-windowは別frameにする
;; ----------------------------------------------------------------
(defun my/split-window (&optional arg)
  (interactive "P")
  (if arg
      (make-frame `((width . ,(frame-width)) (height . ,(frame-height))))
    (split-window-below)))
  
(defun my/other-window (&optional arg)
  (interactive "P")
  (if arg
      (other-frame 1)
    (other-window 1)))

(defun my/delete-window (&optional arg)
  (interactive "P")
  (if arg
      (delete-frame)
    (delete-window)))

(global-set-key "\C-xo" 'my/other-window)
(global-set-key "\C-x2" 'my/split-window)
(global-set-key "\C-x0" 'my/delete-window)
  
;; ----------------------------------------------------------------
;;  editor config
;; ----------------------------------------------------------------
(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
