;;; else-plus.el --- Emacs Language Sensitive Editor (ELSE)
;;
;; Copyright (C) 2021 Johannes Brunen
;;
;; Author: Johannes Brunen <hatlafax@gmx.de>
;; Version: 1.0.0
;; Package Requires: ((ELSE "2.1.0") (emacs "25.1"))
;; Keywords: language sensitive abbreviation template placeholder
;; URL: https://github.com/peter-milliken/ELSE
;; URL: https://github.com/hatlafax/ELSE
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; This package provides some additional convenience functions for ELSE.
;; These function do not belong to the core ELSE mode and this file
;; requires additional loading in oder to use these functions.
;;
;; Usage:
;;      (require 'else-plus)
;;      (use package 'else-plus :ensure nil :after else-mode)
;;      (use package 'else-plus :straight nil :after else-mode)
;;

(require 'else-mode)

(defun else-expand-or-next-expand ()
    "Expand or move to next placeholder and expand."
    (interactive)
    (else-run-when-active
        (let ((entity-details nil))
           (setq entity-details (or (else-in-placeholder)
                                    (else-expand-abbreviation)))
           (unless entity-details
               (else-next))
           (else-expand))))

(defun else-expand-or-previous-expand ()
    "Expand or move to previous placeholder and expand."
    (interactive)
    (else-run-when-active
        (let ((entity-details nil))
           (setq entity-details (or (else-in-placeholder)
                                    (else-expand-abbreviation)))
           (unless entity-details
               (else-previous))
           (else-expand))))

(defun else-kill-or-next-kill ()
    "Kill or move to next placeholder and kill."
    (interactive)
    (else-run-when-active
        (let ((entity-details nil)
              (else-kill-proceed-to-next-placeholder nil))
           (setq entity-details (else-in-placeholder))
           (unless entity-details
               (else-next))
           (else-kill)
           (else-next))))

(defun else-kill-or-previous-kill ()
    "Kill or move to previous placeholder and kill."
    (interactive)
    (else-run-when-active
        (let ((entity-details nil)
              (else-kill-proceed-to-next-placeholder nil))
           (setq entity-details (else-in-placeholder))
           (unless entity-details
               (else-previous))
           (else-kill)
           (else-previous))))

(provide 'else-plus)

;;; else-plus.el ends here
