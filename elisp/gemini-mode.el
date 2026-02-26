;;; gemini-mode.el --- Run gemini-cli in vterm with separate input buffer -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: masami
;; Keywords: tools, ai

;;; Commentary:

;; Emacs上でgemini-cliを操作する拡張機能。
;; vtermでgemini-cliを起動し出力を表示、別バッファでユーザー入力を行う。
;;
;; 使い方:
;;   M-x gemini     - gemini-cliを起動
;;   C-c C-c        - 入力バッファの内容をgemini-cliに送信
;;   TAB            - 入力バッファの内容でgemini-cliのTAB補完を実行

;;; Code:

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-key "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-copy-mode "vterm")
(defvar vterm-shell)
(defvar vterm-copy-mode)

(defgroup gemini nil
  "Run gemini-cli in vterm with separate input buffer."
  :group 'tools
  :prefix "gemini-")

(defcustom gemini-cli-command "gemini"
  "gemini-cliの実行コマンド名。"
  :type 'string
  :group 'gemini)

(defcustom gemini-prompt-regexp "^ *[>❯] *"
  "gemini-cliのプロンプトを認識するための正規表現。"
  :type 'regexp
  :group 'gemini)

(defcustom gemini-vterm-buffer-name "*gemini*"
  "gemini-cli用vtermバッファ名。"
  :type 'string
  :group 'gemini)

(defcustom gemini-input-buffer-name "*gemini-input*"
  "gemini入力バッファ名。"
  :type 'string
  :group 'gemini)

(defcustom gemini-completion-wait 0.5
  "TAB補完結果を待つ秒数。"
  :type 'number
  :group 'gemini)

(defvar gemini--vterm-buffer nil
  "gemini-cli用vtermバッファ。")

(defvar gemini--input-buffer nil
  "gemini入力バッファ。")

(defvar gemini--inhibit-hook nil
  "non-nilの間、window-selection-hookを抑制する。")

(defun gemini--get-vterm-buffer ()
  "vtermバッファを取得。存在しなければnil。"
  (and gemini--vterm-buffer
       (buffer-live-p gemini--vterm-buffer)
       gemini--vterm-buffer))

(defun gemini--get-input-buffer ()
  "入力バッファを取得。存在しなければnil。"
  (and gemini--input-buffer
       (buffer-live-p gemini--input-buffer)
       gemini--input-buffer))

(defun gemini--window-selection-hook (_frame)
  "ウィンドウ選択変更時にvterm-copy-modeを切り替える。
*gemini*バッファに入ったらcopy-modeを有効化、出たら無効化する。"
  (unless gemini--inhibit-hook
    (let ((vbuf (gemini--get-vterm-buffer)))
      (when vbuf
        (if (eq (window-buffer (selected-window)) vbuf)
            ;; vtermバッファに入った → copy-mode有効化（スクロール可能に）
            (with-current-buffer vbuf
              (unless (bound-and-true-p vterm-copy-mode)
                (vterm-copy-mode 1)))
          ;; vtermバッファから出た → copy-mode無効化（表示更新を再開）
          (with-current-buffer vbuf
            (when (bound-and-true-p vterm-copy-mode)
              (vterm-copy-mode -1))))))))

(defun gemini--vterm-last-line ()
  "vtermバッファからプロンプト行のテキストを取得する。
point-maxから後方検索でプロンプトパターンに一致する行を探す。"
  (with-current-buffer (gemini--get-vterm-buffer)
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward gemini-prompt-regexp nil t)
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position))
        ""))))

(defun gemini--strip-prompt (line)
  "LINE からプロンプト部分を除去して返す。"
  (if (string-match gemini-prompt-regexp line)
      (substring line (match-end 0))
    line))

(defun gemini--with-vterm-window (body-fn)
  "vtermウィンドウを選択してBODY-FNを実行する。
copy-modeの解除とhookの抑制を行い、完了後に元のウィンドウに戻る。
vtermウィンドウが表示されていなければ自動的に表示する。"
  (let ((vbuf (gemini--get-vterm-buffer))
        (orig-window (selected-window)))
    (unless vbuf
      (user-error "gemini vtermバッファが見つかりません。M-x gemini で起動してください"))
    (let ((vwin (get-buffer-window vbuf)))
      ;; ウィンドウがなければ初期レイアウトと同じ配置で表示
      (unless vwin
        (delete-other-windows)
        (setq vwin (selected-window))
        (set-window-buffer vwin vbuf)
        (let ((iwin (split-window-below -10)))
          (set-window-buffer iwin (gemini--get-input-buffer))
          ;; orig-windowは消えたので入力バッファのウィンドウを戻り先にする
          (setq orig-window iwin)))
      (let ((gemini--inhibit-hook t))
        (select-window vwin)
        (when (bound-and-true-p vterm-copy-mode)
          (vterm-copy-mode -1))
        (unwind-protect
            (funcall body-fn)
          (select-window orig-window))))))

(defun gemini-send-input ()
  "入力バッファの内容をgemini-cliに送信する。"
  (interactive)
  (let ((input (string-trim (buffer-substring-no-properties
                             (point-min) (point-max))))
        (vbuf (gemini--get-vterm-buffer)))
    (when (string-empty-p input)
      (user-error "入力が空です"))
    (unless vbuf
      (user-error "gemini vtermバッファが見つかりません"))
    ;; vtermウィンドウを選択して送信
    (gemini--with-vterm-window
     (lambda ()
       ;; まずCtrl-Uで現在行をクリア（TAB補完の残りを消す）
       (vterm-send-key "u" nil nil t)
       (sit-for 0.05)
       (vterm-send-string input)
       (sit-for 0.1)
       (vterm-send-return)))
    ;; 入力バッファをクリア
    (erase-buffer)
    ;; vtermバッファを最下部にスクロール
    (let ((vwin (get-buffer-window vbuf)))
      (when vwin
        (with-selected-window vwin
          (goto-char (point-max)))))))

(defun gemini-tab-complete ()
  "入力バッファの内容でgemini-cliのTAB補完を実行する。"
  (interactive)
  (let ((input (buffer-substring-no-properties (point-min) (point-max)))
        (vbuf (gemini--get-vterm-buffer)))
    (unless vbuf
      (user-error "gemini vtermバッファが見つかりません"))
    ;; vtermウィンドウを選択して補完実行
    (gemini--with-vterm-window
     (lambda ()
       ;; Ctrl-Uで現在行をクリアしてから入力を送信
       (vterm-send-key "u" nil nil t)
       (sit-for 0.05)
       (vterm-send-string input)
       (sit-for 0.05)
       ;; TABを送信
       (vterm-send-key "<tab>")))
    ;; 補完結果を待つ
    (sit-for gemini-completion-wait)
    ;; vtermの最終行を読み取る
    (let* ((last-line (gemini--vterm-last-line))
           (completed (string-trim (gemini--strip-prompt last-line))))
      ;; 入力バッファを更新（補完結果が空でなければ常に更新）
      (when (not (string-empty-p completed))
        (erase-buffer)
        (insert completed)))))

(defvar gemini-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gemini-send-input)
    (define-key map (kbd "<tab>") #'gemini-tab-complete)
    (define-key map (kbd "TAB") #'gemini-tab-complete)
    map)
  "gemini-input-mode用キーマップ。")

(define-derived-mode gemini-input-mode fundamental-mode "Gemini Input"
  "gemini-cliへの入力を行うためのメジャーモード。

\\{gemini-input-mode-map}")

(defun gemini--cleanup ()
  "geminiバッファが削除された時のクリーンアップ処理。"
  (remove-hook 'window-selection-change-functions #'gemini--window-selection-hook))

;;;###autoload
(defun gemini ()
  "gemini-cliをvtermで起動し、入力用バッファを作成する。"
  (interactive)
  ;; 既存バッファがあれば再利用
  (if (and (gemini--get-vterm-buffer) (gemini--get-input-buffer))
      (progn
        (switch-to-buffer gemini--vterm-buffer)
        (delete-other-windows)
        (let ((win (split-window-below -10)))
          (set-window-buffer win gemini--input-buffer)
          (select-window win)))
    ;; 新規作成
    ;; vtermバッファを作成してgemini-cliを起動
    (require 'vterm)
    (let ((vterm-shell gemini-cli-command))
      (vterm gemini-vterm-buffer-name))
    (setq gemini--vterm-buffer (get-buffer gemini-vterm-buffer-name))
    ;; vtermバッファ削除時にhookをクリーンアップ
    (with-current-buffer gemini--vterm-buffer
      (add-hook 'kill-buffer-hook #'gemini--cleanup nil t))
    ;; ウィンドウ選択変更時にcopy-modeを切り替えるhookを登録
    (add-hook 'window-selection-change-functions #'gemini--window-selection-hook)
    ;; ウィンドウを分割して入力バッファを作成
    (delete-other-windows)
    (let ((win (split-window-below -10)))
      (setq gemini--input-buffer (get-buffer-create gemini-input-buffer-name))
      (set-window-buffer win gemini--input-buffer)
      (select-window win)
      (with-current-buffer gemini--input-buffer
        (gemini-input-mode)))))

(provide 'gemini-mode)
;;; gemini-mode.el ends here
