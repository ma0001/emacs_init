;;; mode-line-colour.el --- 状態に応じてモードラインの色を変える -*- lexical-binding: t -*-

;; Author: 　https://tarao.hatenablog.com/entry/20110907/1315399652
;; Maintainer:
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(provide 'mode-line-colour)

;;; mode-line-colour.el ends here

(defgroup mode-line-color nil
  "Mode line color."
  :prefix "mode-line-color-"
  :group 'mode-line)

(defcustom mode-line-color-buffers-regexp '("^\\*scratch\\*$")
  "List of regular expressions of buffer names to enable mode-line-color-mode automatically."
  :group 'mode-line-color
  :type '(repeat 'string))

(defcustom mode-line-color-exclude-buffers-regexp '("^ .*" "^\\*")
  "List of regular expressions of buffer names not to enable mode-line-color-mode automatically."
  :group 'mode-line-color
  :type '(repeat 'string))

(defvar mode-line-color-hook nil
  "hook for setting mode line color

   Usage:
     (defun your-function-to-set-mode-line-color (setter)
       (funcall setter \"yellow\"))
     (add-hook 'mode-line-color-hook 'your-function-to-set-mode-line-color)")

(defvar mode-line-color-mode nil)
(defvar mode-line-color-color nil)
(defvar mode-line-color-original "white")

(defun mode-line-color-set-color (color)
  (setq mode-line-color-color color))

(defun mode-line-color-active-p ()
  (let ((buf (buffer-name (current-buffer)))
        (mem-pat
         '(lambda (x l)
            (member t (mapcar '(lambda (r) (when (string-match r x) t)) l)))))
    (and mode-line-color-mode
         (not (minibufferp (current-buffer)))
         (or (funcall mem-pat buf mode-line-color-buffers-regexp)
             (not (funcall mem-pat buf
                           mode-line-color-exclude-buffers-regexp))))))

(defun mode-line-color-update ()
  (when (mode-line-color-active-p)
    (let ((mode-line-color-color nil))
      (run-hook-with-args 'mode-line-color-hook 'mode-line-color-set-color)
      (set-face-background 'mode-line (or mode-line-color-color
                                          mode-line-color-original)))))

(defun mode-line-color-install ()
  (setq mode-line-color-original (face-background 'mode-line))
  (add-hook 'post-command-hook 'mode-line-color-update))

(defun mode-line-color-uninstall ()
  (remove-hook 'post-command-hook 'mode-line-color-update))

;;;###autoload
(define-minor-mode mode-line-color-mode
  "Set color of mode line."
  :global t
  :group 'mode-line-color
  (if mode-line-color-mode
      (mode-line-color-install)
    (mode-line-color-uninstall)))

(provide 'mode-line-color)
