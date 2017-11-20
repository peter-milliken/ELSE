;;; else-template-mode.el --- Major mode for ELSE templates
;;
;; Copyright (C) 2017 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; Version: 1.0
;; Package Requires: ()
;; Keywords: language sensitive abbreviation template placeholder
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

(defvar else-template-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; String delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; Comments
    (modify-syntax-entry ?\; "<   " table)
    (modify-syntax-entry ?\n ">" table)
    table))

(eval-when-compile
  (defun else-rx-symbol (form)
    "Translate FORM into a regexp."
    (let ((body (cdr form)))
      (rx-to-string `(and symbol-start ,@body symbol-end) 'no-group)))

  (defvar else-rx-constituents
    `((symbol else-rx-symbol 0 nil)
      (keyword . ,(rx (or "DEFINE" "DELETE" "END" "LANGUAGE" "PLACEHOLDER")))))

  (defmacro else-rx (&rest sexps)
    "Specialized `rx' variant for ELSE Template Mode.

In addition to the standard forms of `rx', the following forms
are available:

`(symbol SEXP …)'
     Match SEXPs inside symbol boundaries only

`keyword'
     Any valid ELSE template keyword"
    (let ((rx-constituents (append else-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))

(defvar else-template-font-lock-keywords
  `(
    ;; Keywords
    (,(else-rx (symbol keyword)) 0 font-lock-keyword-face)

    ;; Placeholder definition
    (,(else-rx (symbol (or "DEFINE" "DELETE"))
               (one-or-more " ")
               (symbol "PLACEHOLDER")
               (one-or-more " ")
               (group (one-or-more (any "A-Z" "_"))))
     1 font-lock-function-name-face)))

;;;###autoload
(defun else-template-compile-buffer ()
  "Compile the whole current buffer."
  (interactive)
  (save-excursion
    (else-compile-buffer 'start-at-point-min)))

;;;###autoload
(define-derived-mode else-template-mode prog-mode "ELSE-Template"
  "Major mode for editing ELSE templates.
\\{else-template-mode-map}"
  :syntax-table else-template-mode-syntax-table
  ;; (font-lock-fontify-buffer)

  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)
  (setq-local comment-column 0)
  (setq-local comment-use-syntax t)

  ;; Font locking
  (setq font-lock-defaults '(else-template-font-lock-keywords))

  (unless (assoc "ELSE-Template" else-Alternate-Mode-Names)
    (add-to-list 'else-Alternate-Mode-Names '("ELSE-Template" . "Template")))

  (when (fboundp 'else-mode)
    (else-mode 1)))

(provide 'else-template-mode)
