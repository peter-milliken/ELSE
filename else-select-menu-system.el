;;; else-select-menu-system.el --- Emacs Language Sensitive Editor (ELSE)
;;
;; Copyright (C) 2021 Peter Milliken
;;
;; Author: Peter Milliken <peter.milliken@gmail.com>
;; Version: 1.0.0
;; Package Requires: ((ELSE "2.1.0") (emacs "25.1"))
;; URL: https://github.com/peter-milliken/ELSE
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;  This file amalgamates a number of simplistic defuns that allow easy
;;  selection (by the user) of which menu/selection scheme to use. Current
;;  options are:
;;  1) popup (the ELSE default)
;;  2) popup with isearch enabled on the selection (which unfortunately
;;           conflicts with the menu keymap i.e. 'q' (quit) and 's' (select) do
;;           not work anymore.
;;  3) ivy
;;  4) completing-read (Emacs default)
;;
(defun else-use-display-menu-popup ()
  "Use the popup menu selector."
  (interactive)
  (setq else-alternate-menu-picker "else-default-display-menu"))

(defun else-use-display-menu-popup-2 ()
  "Use the popup-2 menu selector."
  (interactive)
  (require 'else-popup-2)
  (setq else-alternate-menu-picker "else-popup-2-display-menu"))

(defun else-use-display-menu-ivy ()
  "Use the ivy menu selector."
  (interactive)
  (require 'else-ivy)
  (setq else-alternate-menu-picker "else-ivy-display-menu"))

(defun else-use-display-menu-completing-read ()
  "Use the completing read menu selector."
  (interactive)
  (require 'else-completing-read)
  (setq else-alternate-menu-picker "else-completing-read-display-menu"))

(provide 'else-select-menu-system)

;;; else-select-menu-system.el ends here
