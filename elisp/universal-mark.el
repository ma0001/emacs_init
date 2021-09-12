;;; universal-mark.el --- navigation backwards and forwards across marks -*- lexical-binding: t -*-

;; Author: masami
;; Maintainer: masami
;; Version: 0.1
;; Package-Requires: ( )
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

;; Summary:
;; this package provides eclipse-like forward/backward navigation
;; bound by default to "C-<" (unversal-mark-previous-location)
;; and "C->" (universal-mark-next-location)

;; allmoset code are borrow from backward-forward.el

;;; Code:

(require 'cl-lib)


(defvar universal-mark-ring nil
  "The list of saved marks, bringing together the global mark ring and the local mark ring into one ring.")

(defvar universal-mark-ring-max 32
  "Maximum size of overall mark ring.  Start discarding off end if gets this big.")

(defvar universal-mark-ring-traversal-position 0
  "Stores the traversal position within the universal-mark-ring.
Gets modified by universal-mark-previous-location and
universal-mark-next-location.
Gets reset to zero whenever universal-mark-after-push-mark runs.")

(defvar universal-mark-in-progress nil
  "Suppresses generation of marks in universal-mark-ring.
Dynamically bound to during the navigation process.")

;;;###autoload
(define-minor-mode universal-mark-mode
  "enables or disable universal-mark minor mode.

when universal-mark mode is enabled, it keeps track of mark pushes across
all buffers in a variable universal-mark-ring, and allows you to navigate backwards
and forwards across these marks using <C-left> and <C-right>.  to customize
the navigation behavior one must customize the mark pushing behavior --
add 'advice' to a command to make it push a mark before invocation if you
want it to be tracked.  see universal-mark.el for examples and more
information.
"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<") #'universal-mark-previous-location)
            (define-key map (kbd "C->") #'universal-mark-next-location)
            map
            )
  :global t
  (if universal-mark-mode
      (progn
        (advice-add 'xref-push-marker-stack :after #'universal-mark-push-mark)
        )
    (progn
       (advice-remove 'xref-push-marker-stack #'universal-mark-push-mark)
;        (advice-remove 'ggtags-find-tag-dwim #'push-mark)
        )))

(defun universal-mark-push-mark (&optional m)
  "Handles mark-tracking work for universal-mark.
Optional argument ARGS completely ignored"
  (if (not universal-mark-in-progress)
      (progn
;;	(message "universal-mark-after-push-mark %S %S %S" location nomsg activate)
	(when (> universal-mark-ring-traversal-position 0)
	  (setq universal-mark-ring (nthcdr (1+ universal-mark-ring-traversal-position) universal-mark-ring))
	  (setf universal-mark-ring-traversal-position 0))
        (let* ((marker (or m (point-marker)))
	       (position (marker-position marker))
	       (buffer (marker-buffer marker)))
          ;;don't insert duplicate marks
          (if (or (eql (length universal-mark-ring) 0)
                  (not (and (eql position (marker-position (elt universal-mark-ring 0)))
                            (eql buffer (marker-buffer (elt universal-mark-ring 0))))))
	      (progn
		(message "pushing marker %S" marker)
                (setq universal-mark-ring (cons (copy-marker marker) universal-mark-ring)))))
        ;;purge excess entries from the end of the list
        (when (> (length universal-mark-ring) universal-mark-ring-max)
          (move-marker (car (nthcdr universal-mark-ring-max universal-mark-ring)) nil)
          (setcdr (nthcdr (1- universal-mark-ring-max) universal-mark-ring) nil))))
    ;;  (message "f/b in progress!")
    )
          
(defun universal-mark-go-to-marker (marker)
  "See pop-to-global-mark for where most of this code came from.
Argument MARKER the marker, in any buffer, to go to."
  (let* ((buffer (marker-buffer marker))
         (position (marker-position marker))
         (universal-mark-in-progress t))
    (if (null buffer)
        (message "buffer no longer exists.")
      (progn
        (if (eql buffer (current-buffer))
            (goto-char marker)
          (progn
            (set-buffer buffer)
            (or (and (>= position (point-min))
                     (<= position (point-max)))
                (if widen-automatically
                    (widen)
                  (error "Global mark position is outside accessible part of buffer")))
            (goto-char position)
            (switch-to-buffer buffer)))))))

(defun universal-mark-previous-location ()
  "Used to navigate to the previous position on universal-mark-ring.
1. Increments universal-mark-ring-traversal-position.
2. Jumps to the mark at that position.
Borrows code from `pop-global-mark'."
  (interactive)
  (if (and (eql universal-mark-ring-traversal-position 0)
           (not
            (and (elt universal-mark-ring 0)
		 (eql (marker-buffer (elt universal-mark-ring 0)) (current-buffer))
                 (eql (marker-position (elt universal-mark-ring 0)) (point)))))
      ;;then we are at the beginning of our navigation chain and we want to mark the current position
      (universal-mark-push-mark))
  (if (< universal-mark-ring-traversal-position (1- (length universal-mark-ring)))
      (cl-incf universal-mark-ring-traversal-position)
    (message "no more marks to visit!"))
  (let* ((marker (elt universal-mark-ring universal-mark-ring-traversal-position))
	 (universal-mark-in-progress t))
    (universal-mark-go-to-marker marker)))

;;(marker-buffer (elt universal-mark-ring 3))

(defun universal-mark-next-location ()
    "Used to navigate to the next position on universal-mark-ring.
1. Decrements universal-mark-ring-traversal-position.
2. Jumps to the mark at that position.
Borrows code from `pop-global-mark'."
  (interactive)
  (if (> universal-mark-ring-traversal-position 0)
      (cl-decf universal-mark-ring-traversal-position)
    (message "you are already at the most current mark!"))
  (let* ((marker (elt universal-mark-ring universal-mark-ring-traversal-position))
	 (universal-mark-in-progress t))	 
    (universal-mark-go-to-marker marker)))

(defun universal-mark-push-mark-wrapper (&rest args)
  "Handles mark-tracking work for universal-mark.
Optional argument ARGS completely ignored"
  (universal-mark-push-mark (point-marker)))

(defun universal-mark-advice-add (func)
  (advice-add func :before #'universal-mark-push-mark-wrapper))

(provide 'universal-mark)

;;; universal-mark.el ends here
