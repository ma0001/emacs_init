;;; gemini-mode.el --- Emacs extension to run gemini-cli -*- lexical-binding: t; -*-

;; Require vterm
(require 'vterm)

(defvar gemini-cli-command "gemini"
  "The command to start gemini-cli.")

(defvar gemini-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gemini-send-input)
    (define-key map (kbd "TAB") #'gemini-complete-input)
    (define-key map (kbd "<backtab>") #'gemini-send-backtab)
    (define-key map [backtab] #'gemini-send-backtab)
    (define-key map (kbd "S-<tab>") #'gemini-send-backtab)
    map)
  "Keymap for `gemini-input-mode'.")

(define-derived-mode gemini-input-mode text-mode "Gemini-Input"
  "Major mode for gemini input buffer.
\\{gemini-input-mode-map}"
  (setq-local cursor-type 'box))

(defun gemini--ensure-visible ()
  "Ensure the *gemini-vterm* buffer is visible above the current input buffer.
If it is not visible, the original top-bottom split layout is restored."
  (let ((vterm-buf (get-buffer "*gemini-vterm*"))
        (input-buf (current-buffer)))
    (when (and vterm-buf (not (get-buffer-window vterm-buf)))
      (delete-other-windows)
      (let ((window (split-window-vertically -12)))
        (set-window-buffer (selected-window) vterm-buf)
        (set-window-buffer window input-buf)
        (select-window window)))))

(defun gemini-send-input ()
  "Send the current input buffer content to the gemini vterm and execute it."
  (interactive)
  (gemini--ensure-visible)
  (let ((input (string-trim-right (buffer-string)))
        (vterm-buf (get-buffer "*gemini-vterm*")))
    (if (not vterm-buf)
        (message "gemini vterm buffer not found")
      (with-current-buffer vterm-buf
        ;; Turn off copy mode to allow streaming text to be visible and auto-scroll
        (when vterm-copy-mode
          (vterm-copy-mode -1))
        ;; Clear current line in terminal (C-a C-k) to measure prompt
        (vterm-send-key "c" t nil t) ;; Ctrl-C to cancel any previous input just in case
        (sleep-for 0.1)
        (vterm-send-return)
        (sleep-for 0.2)
        
        ;; Send the input string and execute it
        (vterm-send-string input)
        (sleep-for 0.1)
        (vterm-send-return))
      (erase-buffer))))

(defun gemini-send-backtab ()
  "Press Shift+Tab in vterm (used to accept edits)."
  (interactive)
  (gemini--ensure-visible)
  (let ((vterm-buf (get-buffer "*gemini-vterm*")))
    (if (not vterm-buf)
        (message "gemini vterm buffer not found")
      (with-current-buffer vterm-buf
        (when vterm-copy-mode
          (vterm-copy-mode -1))
        (vterm-send-key "<backtab>")))))

(defun gemini--get-last-line ()
  "Get the actual last prompt line from the vterm buffer by searching for the prompt character '> '."
  (save-excursion
    (goto-char (point-max))
    (let (found-line)
      (while (and (> (point) (point-min)) (not found-line))
        (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          ;; Look for a typical prompt containing ">"
          ;; Gemini-cli prompt looks like "~/.emacs.d (6d4877d*) > "
          ;; Using a very relaxed match for ">"
          (if (string-match-p ">" current-line)
              (setq found-line current-line)
            (forward-line -1))))
      ;; Trim trailing whitespaces because vterm pads lines to window width
      (if found-line
          (replace-regexp-in-string " +$" "" found-line)
        ""))))

(defun gemini-complete-input ()
  "Ask the underlying vterm for completion and update the current line."
  (interactive)
  (gemini--ensure-visible)
  (let* ((input (buffer-substring-no-properties (line-beginning-position) (point)))
         (vterm-buf (get-buffer "*gemini-vterm*"))
         prompt line completed)
    (if (not vterm-buf)
        (message "gemini vterm buffer not found")
      (with-current-buffer vterm-buf
        ;; Turn off copy mode temporarily if on so terminal responds
        (when vterm-copy-mode (vterm-copy-mode -1))
        ;; Clear current line in terminal
        (vterm-send-key "c" nil nil nil t) ;; Ctrl-C to cancel any previous input just in case
        (sleep-for 0.1)
        (vterm-send-return)
        (sleep-for 0.2)
        
        ;; Record the length of the bare prompt on this new line
        (setq prompt (gemini--get-last-line))
        
        ;; Now send our input buffer's current line and TAB
        (vterm-send-string input)
        (sleep-for 0.1)
        (vterm-send-key "<tab>")
        
        ;; Wait strictly for rendering
        (sleep-for 0.5)
        
        (setq line (gemini--get-last-line)))
      
      ;; Extract the clean prompt up to and including the "> " or ">"
      ;; Force it to contain precisely ONE space after the ">" to avoid placeholder space mismatches.
      (let* ((clean-prompt (if (string-match "\\(.*>\\)\\s-*" prompt)
                               (concat (match-string 1 prompt) " ")
                             prompt))
             (clean-line (if (string-match "\\(.*>\\)\\s-\\{2,\\}\\(.*\\)" line)
                             (concat (match-string 1 line) " " (match-string 2 line))
                           ;; if it's already " > " or " >/help" ensure 1 space
                           (if (string-match "\\(.*>\\)\\s-*\\(.*\\)" line)
                               (concat (match-string 1 line) " " (match-string 2 line))
                             line))))
        
        ;; Debug logs to *Messages* buffer
        (message "GEMINI-DEBUG: input='%s'" input)
        (message "GEMINI-DEBUG: prompt='%s' -> clean-prompt='%s' (len %d)" prompt clean-prompt (length clean-prompt))
        (message "GEMINI-DEBUG: line='%s' -> clean-line='%s' (len %d)" line clean-line (length clean-line))
        
        ;; If the line starts with the clean prompt
        (when (string-prefix-p clean-prompt clean-line)
          (setq completed (substring clean-line (length clean-prompt)))
          (message "GEMINI-DEBUG: extracted completed='%s'" completed))
        
        (if (and completed 
                   (> (length completed) 0)
                   (not (string= input completed)))
            (progn
              (message "GEMINI-DEBUG: Replacing input buffer text!")
              (delete-region (line-beginning-position) (point))
              (insert completed))
          (message "GEMINI-DEBUG: Condition not met var: completed=%S, (string= input completed)=%S" 
                   completed (string= input completed)))))))

(defun gemini-vterm-window-change (frame)
  "Toggle vterm-copy-mode automatically based on window selection."
  (let ((buf (window-buffer (selected-window))))
    (if (and (buffer-live-p buf)
             (string= (buffer-name buf) "*gemini-vterm*"))
        ;; Entered the vterm buffer -> enable copy mode to just view
        (with-current-buffer buf
          (unless vterm-copy-mode
            (vterm-copy-mode 1)))
      ;; Switched away from vterm buffer -> disable copy mode so it updates behind the scenes
      (let ((vterm-buf (get-buffer "*gemini-vterm*")))
        (when (and vterm-buf (buffer-live-p vterm-buf))
          (with-current-buffer vterm-buf
            (when vterm-copy-mode
              (vterm-copy-mode -1))))))))

(add-hook 'window-selection-change-functions #'gemini-vterm-window-change)

;;;###autoload
(defun gemini ()
  "Start gemini-cli in a vterm, enable copy-mode, and create an input buffer."
  (interactive)
  (let ((vterm-buf (get-buffer "*gemini-vterm*"))
        (input-buf (get-buffer-create "*gemini-input*")))
    
    (unless (and vterm-buf (buffer-live-p vterm-buf))
      ;; Disable ask before kill for the new buffer created by vterm
      (let ((vterm-buffer-name "*gemini-vterm*"))
        (save-window-excursion
          (setq vterm-buf (vterm "*gemini-vterm*"))
          (sleep-for 0.5)
          (vterm-send-string gemini-cli-command)
          (vterm-send-return)
          (sleep-for 0.5))))
          
    (with-current-buffer vterm-buf
      ;; Ensure copy mode is off initially so we can see the streaming output
      (when vterm-copy-mode
        (vterm-copy-mode -1))
      (goto-char (point-max)))
        
    (with-current-buffer input-buf
      (unless (eq major-mode 'gemini-input-mode)
        (gemini-input-mode)))
        
    (delete-other-windows)
    (let ((window (split-window-vertically -12)))
      (set-window-buffer (selected-window) vterm-buf)
      (set-window-buffer window input-buf)
      (select-window window))))

(provide 'gemini-mode)
;;; gemini-mode.el ends here
