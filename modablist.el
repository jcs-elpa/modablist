;;; modablist.el --- Modifiable tabulated-list extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-06 20:32:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Modifiable tabulated-list extension.
;; Keyword: table list tablist
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (easy-tabulated-list "0.1.2")
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

(defcustom modablist-default-width 8
  "Default width for each column."
  :type 'integer
  :group 'modablist)

(defcustom modablist-default-columns 26
  "Default column when generate a new sheet."
  :type 'integer
  :group 'modablist)

(defconst modablist--buffer-name-format "*modablist:<%s>*"
  "String format for buffer name.")

(defvar modablist--buffer-index 0
  "Global buffer name index.

This value will be increment by 1 for every new modablist creation.")

(defvar-local modablist-data '()
  "Table data in list.")

(defvar-local modablist--overlays '()
  "List of selection overlays.")

(defvar-local modablist--old-key-map nil
  "Record the old key map before editing.")

(defvar-local modablist--total-rows nil
  "Total rows in the table.")

(defvar-local modablist--total-columns nil
  "Total columns in the table")

(defvar-local modablist--current-row nil "Current row the cursor current on.")
(defvar-local modablist--current-column nil "Current column the cursor current on.")

;;
;; (@* "Util" )
;;

;;
;; (@* "Selection" )
;;

(defun modablist--clear-overlays ()
  "Remove all overlays."
  (dolist (ov modablist--overlays) (delete-overlay ov))
  (setq modablist--overlays '()))

;;
;; (@* "Table" )
;;

(defun modablist--change-data (row column value)
  ""
  (setf (nth column (nth row modablist-data)) value))

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
        (setq-local modablist-mode-map modablist--old-key-map)
        )
    )
  (message "read: %s" buffer-read-only))

(defun modablist--post-command ()
  ""
  (message "%s" (current-column))
  ;;(make-overlay )
  )

(defvar modablist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'modablist--confirm)
    map)
  "Kaymap for `modablist-mode'.")

(defun modablist--new-row ()
  "Generate a new row."
  (let (new-row (cnt 0))
    (while (< cnt modablist--total-rows) (push "" new-row) (setq cnt (1+ cnt)))
    (push new-row modablist-data)
    (modablist--refresh)))

(defun modablist--refresh ()
  "Refresh `tabulated-list' once."
  (easy-tabulated-list-make modablist--tablist-format
                            (easy-tabulated-list-form-entries modablist-data)
                            :padding 1)
  (tabulated-list-revert))

(defun modablist--init-tablist-format ()
  "Initialize the `tabulated-list' format."
  (interactive)
  (let* ((start (string-to-char "A")) (end (+ start modablist-default-columns)) lst)
    (while (< start end)
      (push (list (char-to-string start) modablist-default-width t) lst)
      (setq start (1+ start)))
    (vconcat (reverse lst))))

(defvar modablist--tablist-format (modablist--init-tablist-format)
  "Format to assign to `tabulated-list-format' variable.")

(define-derived-mode modablist-mode tabulated-list-mode
  "modablist-mode"
  "Major mode for `modablist'."
  :group 'modablist
  (setq modablist-data '())
  (setq modablist--total-rows modablist-default-columns
        modablist--total-columns 0
        modablist--current-row 0
        modablist--current-column 0)
  (modablist--refresh)
  (add-hook 'post-command-hook 'modablist--post-command nil t))

;;;###autoload
(defun modablist-new ()
  "Create a new modablist data."
  (interactive)
  (let ((name (format modablist--buffer-name-format modablist--buffer-index)))
    (pop-to-buffer name nil)
    (modablist-mode))
  (setq modablist--buffer-index (1+ modablist--buffer-index)))

(provide 'modablist)
;;; modablist.el ends here
