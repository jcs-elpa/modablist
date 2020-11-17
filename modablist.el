;;; modablist.el --- Modifiable tabulated-list extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-06 20:32:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Modifiable tabulated-list extension.
;; Keyword: table list tablist
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (easy-tabulated-list "0.1.0")
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

(defconst modablist--buffer-name-format "*modablist:<%s>*"
  "String format for buffer name.")

(defvar modablist--buffer-index 0
  "Global buffer name index.

This value will be increment by 1 for every new modablist creation.")

(defvar-local modablist-data '()
  "Table data in list.")

(defun modablist--change-data (row column value)
  ""
  (setf (nth column (nth row modablist-data)) value))

(defvar modablist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'npm-pkgs-unmark-mark)
    map)
  "Kaymap for `modablist-mode'.")

(define-derived-mode modablist-mode tabulated-list-mode
  "modablist-mode"
  "Major mode for `modablist'."
  :group 'modablist
  (easy-tabulated-list-make modablist-data :padding 1))

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
