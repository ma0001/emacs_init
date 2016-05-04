;;;From: tsumura@fml.ec.tmit.ac.jp (TSUMURA Kazumasa)
;;;Newsgroups: fj.editor.emacs
;;;Subject: upcase-previous-symbol and lightning-open-paren
;;;Date: 15 Jul 1995 09:16:36 GMT
;;;Organization: Tokyo Metro. Inst. of Tech., Hino, Tokyo 191, Japan.
;;;Distribution: fj
;;;NNTP-Posting-Host: ec6rt.tmit.ac.jp
;;;
;;;津村%只今おさる状態@都科技大です。
;;;ひょっとしたら便利かなと思って，こんなのを作ってみました。
;;;
;;;［その1］
;;; upcase-previous-symbol:
;;;   直前のシンボル(単語でないところに注目!)を大文字にします。たとえば
;;; `MAX_LINE_BUFFER_SIZE'などといったシンボルを入力するとき，全部小文字
;;; で入力してから，一括して大文字に変換できます。
;;;   二回続けて実行するとキャピタライズを行います。語頭を大文字にしないと
;;; いけないのにタッチの差(;^^)で大文字にならなかった場合も，これでいちいち
;;; 戻らずに直せます(M-b M-c すれば同じだなんてのは言いっこなし)。
;;;   一応，三回続けて実行すると小文字化します(何に使うんだ)。

(defun upcase-previous-symbol (arg)
  "Convert previous symbol (or ARG symbols) to upper case, without moving over.
Successive execution capitalizes the symbol. Triple execution downcases it."
  (interactive "p")
  (funcall (cond ((eq last-command 'upcase-previous-symbol)
                  (setq this-command 'capitalize-previous-symbol)
                  (function capitalize-region))
                 ((eq last-command 'capitalize-previous-symbol)
                  (setq this-command 'downcase-previous-symbol)
                  (function downcase-region))
                 (t
                  (function upcase-region)))
           (save-excursion
             (while (> arg 0)
               (if (re-search-backward "[a-zA-Z]" nil t)
                   ;; アルファベットを含むシンボルのみ対象にする
                   (while (let ((syntax (char-syntax (preceding-char))))
                            (and (or (= syntax ?w) (= syntax ?_))
                                 (not (bobp))))
                     (backward-char 1)))
               (setq arg (1- arg)))
             (point))
           (point)))

;;;設定例
(global-set-key "\M-u" 'upcase-previous-symbol)
;;;
;;;［その2］
;;; lightning-open-paren:
;;;   開き括弧類を入力すると，対応する閉じ括弧を自動的に挿入します。
;;;   それだけならよくありそうですが，このコマンドのlightningたる所以は，
;;; 続けて開き括弧キーを押すと，閉じ括弧がどんどん先へ飛んで，前方の単語や
;;; 式を囲んでくれることにあります。
;;;
;;; (使用例) `C-c (' にこのコマンドを割り当ててあるとします。
;;;
;;;         x = a + b / (c + d) * 2;
;;;             ^ここにポイント(カーソルのことね)があると思いねぇ
;;;
;;;     ・C-c ( を押す
;;;         x = ()a + b / (c + d) * 2;
;;;
;;;     ・ポンと ( を押す
;;;         x = (a) + b / (c + d) * 2;
;;;
;;;     ・さらに二回 ( を押す
;;;         x = (a + b) / (c + d) * 2;
;;;         x = (a + b / (c + d)) * 2;
;;;
;;;     ・ここでもう一度 C-c ( ( (
;;;         x = (()a + b / (c + d)) * 2;
;;;         x = ((a) + b / (c + d)) * 2;
;;;         x = ((a + b) / (c + d)) * 2;  できあがり!
;;;
;;;   キーに割り付けるときには，キーシーケンスの最後が開き括弧類のキーでな
;;; くてはいけません。プレフィクスつきで割り当てることをお薦めします(でな
;;; いと連続した開き括弧を入力したいときに結構不便)。
(defun opposite_paren (paren) "get charcode of opposit palen"
  (cond ((eq paren ?\)) ?\()
	((eq paren ?\() ?\))
	((eq paren ?\]) ?\[)
	((eq paren ?\[) ?\])
	((eq paren ?\}) ?\{)
	((eq paren ?\{) ?\})))

(defun lightning-open-paren (&optional arg)
  "Insert character or ARG characters. If its syntax class is open paren
and no argument is given, this command inserts matching close paren, and
places cursor inside them.
Successive execution moves that close paren across following expressions."
  (interactive "P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (if (/= (char-syntax last-input-char) ?\( )
        (self-insert-command 1)
      (let ((echo-keystrokes 0)                 ;inhibit echo on read-char
            (op-char last-input-char)
;;            (cl-char (% (/ (aref (syntax-table) last-input-char) 256) 256))
                                                ;こんな計算でいいのか?(;^^)
            (cl-char (opposite_paren last-input-char))
            op-pos cl-pos ch)
        (insert op-char cl-char)
        (backward-char 1)
        (setq op-pos (point)
              cl-pos op-pos)
        (while (= (setq ch (read-char)) op-char)
          (goto-char cl-pos)
          (delete-char 1)
          (condition-case err
              (forward-sexp 1)
            (error (ding t)))                   ;この辺もいい加減……
          (insert cl-char)
          (backward-char 1)
          (sit-for 1)
          (setq cl-pos (point))
          (goto-char op-pos))
        (setq unread-command-char ch)))))

(defun lightning-close-paren (&optional arg)
  "Insert character or ARG characters. If its syntax class is open paren
and no argument is given, this command inserts matching close paren, and
places cursor inside them.
Successive execution moves that close paren across following expressions."
  (interactive "P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (if (/= (char-syntax last-input-event) ?\) )
        (self-insert-command 1)
      (let ((echo-keystrokes 0)                 ;inhibit echo on read-char
            (cl-char last-input-event)
;;            (op-char (% (/ (aref (syntax-table) last-input-char) 256) 256))
            (op-char (opposite_paren last-input-event))
                                                ;こんな計算でいいのか?(;^^)
            op-pos cl-pos ch)
        (insert op-char cl-char)
        (backward-char 1)
        (setq cl-pos (point)
              op-pos (1- cl-pos))
        (while (= (setq ch (read-char)) cl-char)
          (goto-char op-pos)
          (delete-char 1)
          (condition-case err
              (backward-sexp 1)
            (error (ding t)))                   ;この辺もいい加減……
          (insert op-char)
          (backward-char 1)
          (sit-for 1)
          (setq op-pos (point))
          (goto-char cl-pos))
        (setq unread-command-char ch)))))

;;;設定例
; 各種プログラム言語用
;; (define-key emacs-lisp-mode-map "\C-c(" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c(" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c[" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c{" 'lightning-open-paren)
;; (define-key emacs-lisp-mode-map "\C-c)" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c)" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c]" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c}" 'lightning-close-paren)

(provide 'lightning-paren)
;;;; latex-mode用(add-hookがないとちょっと面倒なのだ)
;;;(if (not (boundp latex-mode-hook)) (setq latex-mode-hook ()))
;;;(setq latex-mode-hook
;;;      (cons '(lambda ()
;;;               (define-key latex-mode-map "\C-c{" 'lightning-open-paren))
;;;            latex-mode-hook))
;;;
;;;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;;;
;;;動作確認は NEmacs 3.3 でのみ行なっていますが，Muleでもたぶん大丈夫でしょ
;;;う(大したことやってないし)。
;;;なお，私のところでは u-p-s はc-modeで，l-o-p はemacs-lisp-modeでそれぞ
;;;れ威力を発揮しております。逆に言うとそれくらいしか使い途がないのか;^^)
;;;
;;;こういうのを待ってたんだ，という変な人も世界に3人くらいはいるだろうと
;;;願いつつ……
;;;--
;;;\ /     津村一昌、東京都立科学技術大学、tsumura@fml.ec.tmit.ac.jp
;;;. .     TSUMURA Kazumasa, Tokyo Metropolitan Institute of Technology
;;; ,,
;;; ~~く「すんだことったらすんだこと」
