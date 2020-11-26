;;; modablist.el --- Modifiable tabulated-list extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-06 20:32:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Modifiable tabulated-list extension.
;; Keyword: table list tablist
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (easy-tabulated-list "0.1.2"))
;; URL: https://github.com/jcs-elpa/modablist

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Modifiable tabulated-list extension.
;;

;;; Code:

(require 'easy-tabulated-list)

(defgroup modablist nil
  "Modifiable tabulated-list extension."
  :prefix "modablist-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/modablist"))

(defcustom modablist-highlight-delay 0.2
  "Seconds to delay highlight the current selection in the table."
  :type 'float
  :group 'modablist)

(defvar modablist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'modablist--confirm)
    map)
  "Kaymap for function `modablist-mode'.")

(defvar modablist-mode-insert-map
  (let ((map (make-sparse-keymap)))
    (let ((c ?\s))
      (while (< c ?\d)
        (define-key map (vector c) #'self-insert-command)
        (setq c (1+ c)))
      (when (eq system-type 'ms-dos)
        (setq c 128)
        (while (< c 160)
          (define-key map (vector c) #'self-insert-command)
          (setq c (1+ c))))
      (setq c 160)
      (while (< c 256)
        (define-key map (vector c) #'self-insert-command)
        (setq c (1+ c))))
    (define-key map (kbd "<return>") #'modablist--confirm)
    map)
  "Kaymap for function `modablist-mode' when inserting text.")

(defvar-local modablist--buffer nil
  "Record the working buffer.")

(defvar-local modablist--overlays '()
  "List of selection overlays.")

(defvar-local modablist--column-boundary '()
  "List fo column boundary; it uses to identify the current column in table.")

(defvar-local modablist--timer-selection-overlay nil
  "Timer for selection overlay.")

;;
;; (@* "Faces" )
;;

(defface modablist-select-face
  '((t :box '(:line-width -1 :color "#787878" :style nil)))
  "Face when selecting the current box."
  :group 'modablist)

;;
;; (@* "Util" )
;;

(defun modablist--in-range-p (in-val in-min in-max)
  "Check to see if IN-VAL is between IN-MIN and IN-MAX."
  (and (<= in-min in-val) (< in-val in-max)))

(defun modablist--column-to-pos (column)
  "Convert COLUMN to position."
  (save-excursion (move-to-column column) (point)))

(defun modablist--kill-timer (timer)
  "Safe way to kill the TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defmacro modablist-current-buffer (&rest body)
  "Execute BODY with in the current working buffer.."
  (declare (indent 0) (debug t))
  `(when modablist--buffer (with-current-buffer modablist--buffer (progn ,@body))))

;;
;; (@* "Table" )
;;

(defun modablist--change-data (row column value)
  ""
  (setf (nth column (nth row modablist-data)) value))

(defun modablist--count-rows ()
  "Return integer value represent rows."
  (length tabulated-list-entries))

(defun modablist--count-columns ()
  "Return integer value represent columns."
  (length tabulated-list-format))

(defun modablist--first-entry-line ()
  "Return line number from the first entry from current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (tabulated-list-get-entry)) (not (eobp))) (forward-line))
    (line-number-at-pos)))

(defun modablist--current-row ()
  "Return current row number."
  (let ((first-entry-line (modablist--first-entry-line)))
    (1+ (- (line-number-at-pos) first-entry-line))))

(defun modablist--update-column-boundary ()
  "Update the variable `modablist--column-boundary'."
  (setq modablist--column-boundary '())
  (let ((boundary tabulated-list-padding))
    (push boundary modablist--column-boundary)
    (mapc
     (lambda (fmt)
       (setq boundary (+ boundary (nth 1 fmt)))
       (push boundary modablist--column-boundary))
     tabulated-list-format)
    (setq modablist--column-boundary (reverse modablist--column-boundary))))

(defun modablist--current-column ()
  "Return current column number."
  (modablist--update-column-boundary)
  (let ((cur-col (current-column)) (len (length modablist--column-boundary))
        (index 0) break column upper-column lower-column)
    (while (and (not break) (< index (1- len)))
      (setq lower-column (nth index modablist--column-boundary)
            upper-column (nth (1+ index) modablist--column-boundary))
      (when (modablist--in-range-p cur-col lower-column upper-column)
        (setq column index break t))
      (setq index (1+ index)))
    (when column (setq column (1+ column)))
    column))

(defun modablist--current-range ()
  "Return current column number."
  (modablist--update-column-boundary)
  (let ((cur-col (current-column)) (len (length modablist--column-boundary))
        (index 0) break column upper-column lower-column)
    (while (and (not break) (< index (1- len)))
      (setq lower-column (nth index modablist--column-boundary)
            upper-column (nth (1+ index) modablist--column-boundary))
      (when (modablist--in-range-p cur-col lower-column upper-column)
        (setq column index break t))
      (setq index (1+ index)))
    (when column
      (cons (modablist--column-to-pos lower-column)
            (modablist--column-to-pos upper-column)))))

;;
;; (@* "Selection" )
;;

(defun modablist--make-overlay (beg end)
  "Make selection overlay."
  (when (and (integerp beg) (integerp end))
    (let ((ol (make-overlay beg end)))
      (overlay-put ol 'face 'modablist-select-face)
      (overlay-put ol 'priority 100)
      (push ol modablist--overlays)  ; NOTE: Eventually get managed to list.
      ol)))

(defun modablist--make-selection-ov ()
  "Make selection overlay."
  (modablist-current-buffer
    (let ((range (modablist--current-range)))
      (modablist--make-overlay (car range) (cdr range)))))

(defun modablist--clear-overlays ()
  "Remove all overlays."
  (dolist (ov modablist--overlays) (delete-overlay ov))
  (setq modablist--overlays '()))

;;
;; (@* "Core" )
;;

(defun modablist--confirm ()
  ""
  (interactive)
  (unless (tabulated-list-get-entry) (modablist--new-row))
  (setq buffer-read-only (not buffer-read-only))
  (if buffer-read-only
      (progn
        ()
        )
    )
  (message "read: %s" buffer-read-only))

(defun modablist--new-row ()
  "Create a new row."
  (let ((cnt 0) (columns (modablist--count-columns)) new-row)
    (while (< cnt columns) (push "" new-row) (setq cnt (1+ cnt)))
    (setq tabulated-list-entries
          (append tabulated-list-entries
                  (easy-tabulated-list-form-entries new-row)))
    (tabulated-list-revert)))

(defun modablist--post-command ()
  "Post command for `modablist-mode'."
  (modablist--clear-overlays)
  (modablist--kill-timer modablist--timer-selection-overlay)
  (setq modablist--timer-selection-overlay
        (run-with-timer modablist-highlight-delay nil #'modablist--make-selection-ov)))

;;
;; (@* "Entry" )
;;

(defun modablist--enable ()
  "Enable `modablist' in current buffer."
  (if (derived-mode-p 'tabulated-list-mode)
      (progn
        (setq modablist--buffer (current-buffer))
        (add-hook 'post-command-hook #'modablist--post-command nil t))
    (modablist-mode -1)
    (user-error "[WARNING] You can't enable modablist in buffer that aren't derived from `tabulated-list-mode`")))

(defun modablist--disable ()
  "Disable `modablist' in current buffer."
  (remove-hook 'post-command-hook #'modablist--post-command t))

;;;###autoload
(define-minor-mode modablist-mode
  "Minor mode 'modablist-mode'."
  :lighter " ModLst"
  :group modablist
  (if modablist-mode (modablist--enable) (modablist--disable)))

(provide 'modablist)
;;; modablist.el ends here
