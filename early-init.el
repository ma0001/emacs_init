
(defvar debug-mac-ime nil)
(if debug-mac-ime
    (setq package-load-list '((mac-ime nil) all)))
