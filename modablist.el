;;; modablist.el --- Modifiable tabulated-list extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-06 20:32:21

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Modifiable tabulated-list extension.
;; Keyword: table list tablist
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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

(require 'tabulated-list)

(defgroup modablist nil
  "Modifiable tabulated-list extension."
  :prefix "modablist-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/modablist"))

(provide 'modablist)
;;; modablist.el ends here
