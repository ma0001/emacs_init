;;;From: tsumura@fml.ec.tmit.ac.jp (TSUMURA Kazumasa)
;;;Newsgroups: fj.editor.emacs
;;;Subject: upcase-previous-symbol and lightning-open-paren
;;;Date: 15 Jul 1995 09:16:36 GMT
;;;Organization: Tokyo Metro. Inst. of Tech., Hino, Tokyo 191, Japan.
;;;Distribution: fj
;;;NNTP-Posting-Host: ec6rt.tmit.ac.jp
;;;
;;;��¼%�������������@�Բʵ���Ǥ���
;;;�Ҥ�äȤ������������ʤȻפäơ�����ʤΤ��äƤߤޤ�����
;;;
;;;�Τ���1��
;;; upcase-previous-symbol:
;;;   ľ���Υ���ܥ�(ñ��Ǥʤ��Ȥ��������!)����ʸ���ˤ��ޤ������Ȥ���
;;; `MAX_LINE_BUFFER_SIZE'�ʤɤȤ��ä�����ܥ�����Ϥ���Ȥ���������ʸ��
;;; �����Ϥ��Ƥ��顤��礷����ʸ�����Ѵ��Ǥ��ޤ���
;;;   ���³���Ƽ¹Ԥ���ȥ���ԥ��饤����Ԥ��ޤ�����Ƭ����ʸ���ˤ��ʤ���
;;; �����ʤ��Τ˥��å��κ�(;^^)����ʸ���ˤʤ�ʤ��ä����⡤����Ǥ�������
;;; ��餺��ľ���ޤ�(M-b M-c �����Ʊ�����ʤ�ƤΤϸ����ä��ʤ�)��
;;;   ���������³���Ƽ¹Ԥ���Ⱦ�ʸ�������ޤ�(���˻Ȥ����)��

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
                   ;; ����ե��٥åȤ�ޤॷ��ܥ�Τ��оݤˤ���
                   (while (let ((syntax (char-syntax (preceding-char))))
                            (and (or (= syntax ?w) (= syntax ?_))
                                 (not (bobp))))
                     (backward-char 1)))
               (setq arg (1- arg)))
             (point))
           (point)))

;;;������
(global-set-key "\M-u" 'upcase-previous-symbol)
;;;
;;;�Τ���2��
;;; lightning-open-paren:
;;;   �������������Ϥ���ȡ��б������Ĥ���̤�ưŪ���������ޤ���
;;;   ��������ʤ�褯���ꤽ���Ǥ��������Υ��ޥ�ɤ�lightning�����ʤϡ�
;;; ³���Ƴ�����̥����򲡤��ȡ��Ĥ���̤��ɤ�ɤ��������ǡ�������ñ���
;;; ����Ϥ�Ǥ���뤳�Ȥˤ���ޤ���
;;;
;;; (������) `C-c (' �ˤ��Υ��ޥ�ɤ������ƤƤ���Ȥ��ޤ���
;;;
;;;         x = a + b / (c + d) * 2;
;;;             ^�����˥ݥ����(��������Τ��Ȥ�)������Ȼפ��ͤ�
;;;
;;;     ��C-c ( �򲡤�
;;;         x = ()a + b / (c + d) * 2;
;;;
;;;     ���ݥ�� ( �򲡤�
;;;         x = (a) + b / (c + d) * 2;
;;;
;;;     ���������� ( �򲡤�
;;;         x = (a + b) / (c + d) * 2;
;;;         x = (a + b / (c + d)) * 2;
;;;
;;;     �������Ǥ⤦���� C-c ( ( (
;;;         x = (()a + b / (c + d)) * 2;
;;;         x = ((a) + b / (c + d)) * 2;
;;;         x = ((a + b) / (c + d)) * 2;  �Ǥ�������!
;;;
;;;   �����˳���դ���Ȥ��ˤϡ������������󥹤κǸ夬���������Υ����Ǥ�
;;; ���ƤϤ����ޤ��󡣥ץ�ե������Ĥ��ǳ�����Ƥ뤳�Ȥ����ᤷ�ޤ�(�Ǥ�
;;; ����Ϣ³����������̤����Ϥ������Ȥ��˷빽����)��
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
                                                ;����ʷ׻��Ǥ����Τ�?(;^^)
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
            (error (ding t)))                   ;�����դ⤤���ø��ġ�
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
                                                ;����ʷ׻��Ǥ����Τ�?(;^^)
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
            (error (ding t)))                   ;�����դ⤤���ø��ġ�
          (insert op-char)
          (backward-char 1)
          (sit-for 1)
          (setq op-pos (point))
          (goto-char cl-pos))
        (setq unread-command-char ch)))))

;;;������
; �Ƽ�ץ���������
;; (define-key emacs-lisp-mode-map "\C-c(" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c(" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c[" 'lightning-open-paren)
;; (define-key c-mode-map "\C-c{" 'lightning-open-paren)
;; (define-key emacs-lisp-mode-map "\C-c)" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c)" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c]" 'lightning-close-paren)
;; (define-key c-mode-map "\C-c}" 'lightning-close-paren)

(provide 'lightning-paren)
;;;; latex-mode��(add-hook���ʤ��Ȥ���ä����ݤʤΤ�)
;;;(if (not (boundp latex-mode-hook)) (setq latex-mode-hook ()))
;;;(setq latex-mode-hook
;;;      (cons '(lambda ()
;;;               (define-key latex-mode-map "\C-c{" 'lightning-open-paren))
;;;            latex-mode-hook))
;;;
;;;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;;;
;;;ư���ǧ�� NEmacs 3.3 �ǤΤ߹ԤʤäƤ��ޤ�����Mule�Ǥ⤿�֤�����פǤ���
;;;��(�礷�����Ȥ�äƤʤ���)��
;;;�ʤ�����ΤȤ���Ǥ� u-p-s ��c-mode�ǡ�l-o-p ��emacs-lisp-mode�Ǥ��줾
;;;����Ϥ�ȯ�����Ƥ���ޤ����դ˸����Ȥ��줯�餤�����Ȥ��Ӥ��ʤ��Τ�;^^)
;;;
;;;���������Τ��ԤäƤ�������Ȥ����Ѥʿͤ�������3�ͤ��餤�Ϥ��������
;;;�ꤤ�Ĥġġ�
;;;--
;;;\ /     ��¼�쾻�������Ω�ʳص�����ء�tsumura@fml.ec.tmit.ac.jp
;;;. .     TSUMURA Kazumasa, Tokyo Metropolitan Institute of Technology
;;; ,,
;;; ~~���֤�������Ȥä��餹������ȡ�
