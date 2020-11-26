;;; modablist.el --- Modifiable tabulated-list extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-06 20:32:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Modifiable tabulated-list extension.
;; Keyword: table list tablist
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (easy-tabulated-list "0.1.2"))
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

(require 'subr-x)

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
    (define-key map [C-mouse-1] #'modablist-continue-select-at-point)
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

(defvar-local modablist--selected-box '()
  "List of selected box.
The data is construct by (row . column).")

(defvar-local modablist--column-boundary '()
  "List fo column boundary; it uses to identify the current column in table.")

(defvar-local modablist--timer-selection-overlay nil
  "Timer for selection overlay.")

(defvar-local modablist--continue-select-p nil
  "Flag to see if currently continue selecting.")

;;
;; (@* "Faces" )
;;

(defface modablist-select-face
  '((t :box '(:line-width -1 :color "#65A7E2" :style nil)))
  "Face when selecting the current box."
  :group 'modablist)

(defface modablist-insert-face
  '((t :inverse-video t))
  "Face when inserting the current box."
  :group 'modablist)

;;
;; (@* "Util" )
;;

(defun modablist--in-range-p (in-val in-min in-max)
  "Check to see if IN-VAL is between IN-MIN and IN-MAX."
  (and (<= in-min in-val) (< in-val in-max)))

(defun modablist--column-to-pos (column)
  "Convert COLUMN to position."
  (when (integerp column) (save-excursion (move-to-column column) (point))))

(defun modablist--kill-timer (timer)
  "Safe way to kill the TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defmacro modablist-current-buffer (&rest body)
  "Execute BODY with in the current working buffer.."
  (declare (indent 0) (debug t))
  `(when modablist--buffer (with-current-buffer modablist--buffer (progn ,@body))))

(defun modablist--inserting-p ()
  "Return non-nil if currently inserting."
  (not buffer-read-only))

(defun modablist--toggle-mode ()
  "Invert the insert flag."
  (setq buffer-read-only (not buffer-read-only)))

;;
;; (@* "Table" )
;;

(defun modablist--change-data (column value)
  "Change current table value to VALUE.

Argument COLUMN is use to identify the the order of the table entry.

Notice this function will modified variable `tabulated-list-entries'.
You will need to call function `tabulated-list-revert' afterward."
  (when (integerp column) (aset (tabulated-list-get-entry) (1- column) value)))

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
  "Update the variable `modablist--column-boundary'.

Notice that you will need to call this if either of these following variables
have changed.

  - `tabulated-list-padding' - Starting column to print table.
  - `tabulated-list-format' - Each column's width definition."
  (setq modablist--column-boundary '())
  (let ((boundary tabulated-list-padding))
    (push boundary modablist--column-boundary)
    (mapc (lambda (fmt)
            (setq boundary (+ boundary (nth 1 fmt)))
            (push boundary modablist--column-boundary))
          tabulated-list-format)
    (setq modablist--column-boundary (reverse modablist--column-boundary))))

(defun modablist--current-column ()
  "Return current column number."
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

(defun modablist--get-column-boundary (column &optional pos)
  "Return the column boundary by COLUMN.

If optional argument POS is non-nil then convert the column data into the
current buffer position data."
  (when (integerp column)
    (let ((lower-column (nth (1- column) modablist--column-boundary))
          (upper-column (nth column modablist--column-boundary)))
      (if pos (cons (modablist--column-to-pos lower-column)
                    (modablist--column-to-pos upper-column))
        (cons lower-column upper-column)))))

(defun modablist--current-range ()
  "Return current column buffer position by (beg . end)."
  (let ((column (modablist--current-column)))
    (modablist--get-column-boundary column t)
    (modablist--get-column-boundary column t)))

(defun modablist--current-content ()
  "Return content from box."
  (let* ((range (modablist--current-range)) (beg (car range)) (end (cdr range)))
    (when (and (integerp beg) (integerp end))
      (string-trim (buffer-substring beg end)))))

(defun modablist--move-to (row column)
  "Move cursor to ROW and COLUMN."
  (when row (forward-line (- row (modablist--current-row))))
  (when column (move-to-column (car (modablist--get-column-boundary column)))))

;;
;; (@* "Selection" )
;;

(defun modablist--make-overlay (beg end)
  "Make selection overlay by BEG and END."
  (when (and (integerp beg) (integerp end))
    (let ((ol (make-overlay beg end)))
      (overlay-put ol 'face (if (modablist--inserting-p) 'modablist-insert-face
                              'modablist-select-face))
      (overlay-put ol 'priority 100)
      (push ol modablist--overlays)  ; NOTE: Eventually get managed to list.
      ol)))

(defun modablist--clear-overlays ()
  "Remove all overlays."
  (dolist (ov modablist--overlays) (delete-overlay ov))
  (setq modablist--overlays '()))

(defun modablist--ensure-current-selection ()
  "Ensure current selection will always be displayed."
  (push (cons (modablist--current-row) (modablist--current-column))
        modablist--selected-box)
  (delete-dups modablist--selected-box))

(defun modablist--make-selection-ov ()
  "Make selection overlay."
  (modablist-current-buffer
    (modablist--ensure-current-selection)
    (save-excursion
      (dolist (box modablist--selected-box)
        (modablist--move-to (car box) (cdr box))
        (let ((range (modablist--current-range)))
          (modablist--make-overlay (car range) (cdr range)))))))

(defun modablist-remove-all-selections ()
  "Remove all selections."
  (interactive)
  (setq modablist--selected-box '()))

(defun modablist-continue-select-at-point ()
  "Make continue select at current box."
  (interactive)
  (setq modablist--continue-select-p t)
  (call-interactively #'mouse-set-point)
  (modablist--ensure-current-selection))

;;
;; (@* "Core" )
;;

(defun modablist--confirm ()
  "Not documented."
  (interactive)
  (unless (tabulated-list-get-entry) (modablist--new-row))
  (modablist--toggle-mode)
  (if (modablist--inserting-p)
      (use-local-map modablist-mode-insert-map)
    (use-local-map modablist-mode-map)
    (modablist--change-data (modablist--current-column) (modablist--current-content))
    (tabulated-list-revert)))

(defun modablist--new-row ()
  "Create a new row."
  (let ((cnt 0) (columns (modablist--count-columns)) new-row)
    (while (< cnt columns) (push "" new-row) (setq cnt (1+ cnt)))
    (setq tabulated-list-entries
          (append tabulated-list-entries
                  (easy-tabulated-list-form-entries new-row)))
    (tabulated-list-revert)))

(defun modablist--pre-command ()
  "Pre command for function `modablist-mode'."
  (setq modablist--continue-select-p nil))

(defun modablist--post-command ()
  "Post command for function `modablist-mode'."
  (modablist--clear-overlays)
  (unless modablist--continue-select-p (modablist-remove-all-selections))
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
        (modablist--update-column-boundary)
        (setq modablist--buffer (current-buffer))
        (add-hook 'pre-command-hook #'modablist--pre-command nil t)
        (add-hook 'post-command-hook #'modablist--post-command nil t))
    (modablist-mode -1)
    (user-error "[WARNING] You can't enable modablist in buffer that aren't derived from `tabulated-list-mode`")))

(defun modablist--disable ()
  "Disable `modablist' in current buffer."
  (remove-hook 'pre-command-hook #'modablist--pre-command t)
  (remove-hook 'post-command-hook #'modablist--post-command t))

;;;###autoload
(define-minor-mode modablist-mode
  "Minor mode 'modablist-mode'."
  :lighter " ModLst"
  :group modablist
  (if modablist-mode (modablist--enable) (modablist--disable)))

(provide 'modablist)
;;; modablist.el ends here
