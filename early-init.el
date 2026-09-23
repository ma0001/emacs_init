
; mac-ime をデバッグモードで起動する場合nil以外にする
(defvar debug-mac-ime nil)
(if debug-mac-ime
    (setq package-load-list '((mac-ime nil) all)))

; golden gate (macos27) 環境において Emacs 31 の Native Compilation（libgccjit）がコンパイラ（clang）に無効な macOS バージョン引数を渡してしまうことで発生しているエラー回避
(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET"
          (or (getenv "MACOSX_DEPLOYMENT_TARGET")
              (string-trim (shell-command-to-string "sw_vers -productVersion")))))
