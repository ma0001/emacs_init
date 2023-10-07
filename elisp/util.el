;;; util.el --- 色々 -*- lexical-binding: t -*-

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

(defun hexdump-to-decimal-little (hex-str)
  "Converts a little-endian hex dump to decimal."
  (let ((hex-str (replace-regexp-in-string "[^a-fA-F0-9]" "" hex-str))
        (result ""))
    "Reverse the string by splitting it into two characters."
    (dotimes (i (/ (length hex-str) 2) result)
      (setq result (concat (substring hex-str (* i 2) (+ 2 (* i 2))) result)))
    (string-to-number result 16)))
