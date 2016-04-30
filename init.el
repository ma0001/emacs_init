;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-


(add-to-list 'load-path "~/.emacs.d/elisp")


;;(setq Info-directory-list (list "/sw/share/info" "/sw/info" "/usr/share/info" "~/local/info" "~/local/share/info"))
;;(setq Info-additional-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info"))
(setq Info-directory-list (list "/Applications/MacPorts/Emacs.app/Contents/Resources/info" "/Users/masami/local/info" "/Users/masami/local/share/info" "/opt/local/share/info"))
 
(global-set-key "\M-l" 'goto-line)
(global-set-key "\C-xc" 'compile)

(global-set-key "\M-g\M-r" 'grep)
(global-set-key "\M-g\M-f" 'grep-find)
(global-set-key "\M-g\M-c" 'recompile)

(setq grep-find-use-xargs 'gnu)
;;(setq grep-find-command "find . -type f -name '*.[cphm]' -print0 | xargs -0 grep -nH -e ")
(setq grep-find-command "find . -name '.svn' -prune -o -name '.git' -prune -o -name '*.o' -prune -o -type f -print0 | xargs -0 grep -nH -e ")
;;(setq grep-find-command "gfind . -name '.svn' -prune -o -name '.git' -prune -o -not -regex \".*\\.\\(o\\|a\\)\" -print0 | xargs -0 grep -nH -e ")

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)
(setq x-select-enable-clipboard t)

;;
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; disable drag and drop in dired-mode
(setq dired-dnd-protocol-alist nil)
(define-key global-map [ns-drag-file] 'ns-find-file)

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [165] [92])

; dabbrev-expand で大文字小文字の変換機能をオフにする
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

(setq visible-bell nil)

(global-set-key "\M-o" 'universal-argument)

(show-paren-mode 1)

;;
;; theme
;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'wheat t)

(setq c-tab-always-indent nil)

;;
;; package list
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;;
(require 'lightning-paren)
(global-set-key "\C-c(" 'lightning-open-paren)
(global-set-key "\C-c[" 'lightning-open-paren)
(global-set-key "\C-c{" 'lightning-open-paren)
(global-set-key "\C-c)" 'lightning-close-paren)
(global-set-key "\C-c]" 'lightning-close-paren)
(global-set-key "\C-c}" 'lightning-close-paren)

;;
;; gtags
;;
(setq gtags-path-style 'relative)
(autoload 'gtags-mode "gtags" "" t)

(setq gtags-mode-hook
       '(lambda ()
          (define-key gtags-mode-map "\er" 'helm-gtags-find-rtag)
	  (define-key gtags-mode-map "\e." 'helm-gtags-find-tag)
	  (define-key gtags-mode-map "\e*" 'helm-gtags-pop-stack)
;         (define-key gtags-mode-map "\et" 'gtags-find-tag)
;         (define-key gtags-mode-map "\ev" 'gtags-visit-rootdir)
	 ))

;;
;;  for SLIME
;;
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
(setq slime-lisp-implementations
;;     `((sbcl ("/Users/masami/local/bin/sbcl" "--core" "/Users/masami/local/lib/sbcl/sbcl.core"))
     `((sbcl ("/opt/local/bin/sbcl"))
       (abcl ("/opt/local/bin/abcl"))
       (clisp ("/opt/local/bin/clisp"))))
(require 'slime)
(slime-setup  '(slime-repl slime-asdf slime-fancy slime-banner))

;;(setq inferior-lisp-program "~/src/sbcl/src/runtime/sbcl --core /Users/masami/src/sbcl/output/sbcl.core --no-userinit")

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(add-hook 'lisp-mode-hook (lambda ()
			    (slime-mode t)
			    (show-paren-mode 1)
			    (global-set-key "\C-cH" 'hyperspec-lookup)))

;;
;; c++ c mode
;;
(autoload 'vs-set-c-style "vs-set-c-style")

(add-hook 'c-mode-hook
          (function (lambda ()
		      (vs-set-c-style) 
;;                      (setq tab-width  4)
;;		      (c-set-style "cc-mode")
		      (helm-gtags-mode 1)
)))

(add-hook 'c++-mode-hook
          (function (lambda ()
		      (vs-set-c-style) 
;;                      (setq tab-width  4)
;;		      (c-set-style "cc-mode")
		      (helm-gtags-mode 1)
)))

;;
;; helm
;;
(require 'helm)
(require 'helm-config)

;; key bindings
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action

;;
;; helm gtags
::
(require 'helm-gtags)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
             (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
             (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
             (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
             (local-set-key (kbd "M-g g") 'helm-gtags-find-pattern)))

;;
;; aspell
;;
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;;
;; color-moccur  (ELPA)
;;
(require 'color-moccur)
(global-set-key (kbd "M-o") 'occur-by-moccur)
(setq moccur-split-word t)

;;
;; flycheck
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; font set 23
;;
(when (>= emacs-major-version 23)
  (create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
		    'unicode
		    (font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
		    nil
		    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
  (set-cursor-color "Orange")
  (blink-cursor-mode 1))

;;
;; ruby
;;
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
	     (inf-ruby-keys)))

;;
;; for haskell
;;
(load "/opt/local/share/emacs/site-lisp/haskell-mode-2.4/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
 
;;
;; ocaml 
;;
(setq auto-mode-alist
      (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "cameldeb" "Run the Caml debugger." t)

;;
;; erlang
;;
(add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.8.1/emacs")
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/bin" exec-path)) 
(require 'erlang-start)
(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)

;;
;; scheme (gauche)
;;
(autoload 'gauche-mode "gauche-mode" nil t)

(setq auto-mode-alist (cons '("\\.scm$" . gauche-mode) auto-mode-alist))

(setq scheme-program-name "/opt/local/bin/gosh")

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

(setq file-coding-system-alist
      (cons '("gauche-refj\\.info.*\\'" utf-8 . utf-8)
             file-coding-system-alist))
;;
;; kahua
;;
(require 'kahua)
(setq auto-mode-alist
      (append '(("\\.kahua$" . kahua-mode)) auto-mode-alist))
(setq kahua-site-bundle (expand-file-name "~/work/kahua/site"))

;;
;; R
;;
(require 'ess-site)

(fset 'change-to-maxtry
      [?\C-  ?\C-s tab ?\C-s ?\C-s ?\C-s return ?\C-w ?m ?a ?x ?t ?r ?y ?\( ?\" ?\C-s ?\C-s ?\C-b ?\" ?, ?\C-  ?\C-s ?\C-s ?\C-s return ?\C-w ?\" ?\C-s ?\C-s ?\C-b ?\" ?, ?1 ?0 ?\) ?\C-k ])

(defun change-to-maxtry-region (top bottom)
  (interactive "r")
  (apply-macro-to-region-lines top bottom (symbol-function 'change-to-maxtry)))
   


;;
;;
;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "bsd"))))
 '(safe-local-variable-values (quote ((scheme-comint-send-module-name . binomial-heap) (scheme-comint-send-module-name . redblack-tree) (scheme-comint-send-module-name . redblask-tree) (scheme-comint-send-module-name . leftist-heap) (scheme-comint-send-module-name . tree) (Package . XREF) (Syntax . Common-lisp) (Syntax . COMMON-LISP) (Package . CL-PPCRE) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . CL-USER) (package . net\.aserve) (package . net\.aserve\.client) (scheme-comint-send-module-name . graph)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
