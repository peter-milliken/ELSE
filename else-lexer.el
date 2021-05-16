;;; else-lexer.el --- else-lexer

;; Copyright (C) 2017 Peter Milliken
;;
;; Author: Peter Milliken peter.milliken@gmail.com
;;
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

;;; Commentary:

;;  Parse a ELSE template file into a stream of tokens.

;;; Code:

(defconst else-defining-string
  "^\\s *\\(DELETE\\|DEFINE\\) +\\(TOKEN\\|PLACEHOLDER\\|LANGUAGE\\) +\\(\".*\"\\|\\S-+\\)")
(defconst else-defining-command 1)
(defconst else-defining-type (1+ else-defining-command))
(defconst else-defining-name (1+ else-defining-type))

(defconst else-body-string
  "^\\s +\\(/[A-Z_]+\\) *= *\\(\".*\"\\|[-a-zA-Z_0-9\\.]+\\)\\([/A-Z_]+\\)*\\|\\(/[A-Z_]+\\) *-\\|\\(\".*\"\\) *\\(/[A-Z_ =]+\\)?\\(/NOFOLLOW\\|/FOLLOW\\)?\\|\\(END *DEFINE\\)")

(defconst else-body-menu-with-description
  "^\\s +\"\\(.+\\)\"/DESCRIPTION=\"\\(.+\\)\"")

(defconst else-attribute-string "^\\s +\\(\\(\\(/[A-Z_]+\\) *= *\\(\".*\"\\|\\S-+\\)\\)\\|\\(/[A-Z_]+\\)\\) *$"
  "Expression to detect strings of form /xxxx=\"yyyy\" or /xxxx.")
(defconst else-attribute-with-value 3
  "Grouping for /xxxx=\"yyyy\".")
(defconst else-attribute-value 4
  "Grouping for \"yyyy\".")
(defconst else-attribute-no-value 5
  "Grouping for /xxxx.")

(defconst else-text-string "^\\s +\\(\".*\"\\)"
  "Expression for the 'body' of any definitions.")

(defconst else-trailing-attribute "/[A-Z]+"
  "Expression for /PLACEHOLDER/TOKEN/FOLLOW/NOFOLLOW that may trail a menu entry.")

(defconst else-end-define "^\\s *END +DEFINE"
  "END DEFINE.")

(cl-defstruct token
  (type nil)                         ; token value - refer to 'else-attribute-mapping
  (value nil)                        ; value accompanying the token (if present)
  (line-no nil))                     ; line #

(defvar else-attribute-mapping
  '(("/LANGUAGE" . language)
    ("/NOAUTO_SUBSTITUTE" . noauto-substitute)
    ("/AUTO_SUBSTITUTE" . auto-substitute)
    ("/SUBSTITUTE_COUNT" . substitution-count)
    ("/DESCRIPTION" . description)
    ("/DUPLICATION" . duplication)
    ("/SEPARATOR" . separator)
    ("/TYPE" . type)
    ("CONTEXT_DEPENDENT" . context-dependent)
    ("VERTICAL" . vertical)
    ("HORIZONTAL" . horizontal)
    ("NONTERMINAL" . nonterminal)
    ("TERMINAL" . terminal)
    ("MENU" . menu)
    ("/PLACEHOLDER" . placeholder)
    ("/TOKEN" . token)
    ("/BEFORE" . before-action)
    ("/AFTER" . after-action)
    ("/FOLLOW" . follow)
    ("/NOFOLLOW" . nofollow)
    ("/INITIAL_STRING" . initial-string)
    ("/PUNCTUATION_CHARACTERS" . punctuation-characters)
    ("/VALID_IDENTIFIER_CHARACTERS" . valid-identifier-characters)
    ("/INDENT_SIZE" . indent-size)
    ("/VERSION" . version))
  "Mapping of attribute keywords to tokens.")

(defun extract-command-token ()
  "Extract the command information from the match data."
  (let ((command (match-string-no-properties else-defining-command))
        (type (match-string-no-properties else-defining-type))
        (name (match-string-no-properties else-defining-name))
        (this-token (make-token)))
    (if (string= command "DEFINE")
        (cond ((string= type "PLACEHOLDER")
               (setf (token-type this-token) 'define-placeholder))
              ((string= type "TOKEN")
               (setf (token-type this-token) 'define-token))
              ((string= type "LANGUAGE")
               (setf (token-type this-token) 'define-language)))
      (cond ((string= type "PLACEHOLDER")
             (setf (token-type this-token) 'delete-placeholder))
            ((string= type "TOKEN")
             (setf (token-type this-token) 'delete-token))
            ((string= type "LANGUAGE")
             (setf (token-type this-token) 'delete-language))))
    (setf (token-value this-token) (upcase (match-string-no-properties else-defining-name)))
    this-token))

(defun else-extract-attribute ()
  "Extract the attribute information from the match data."
  (let ((attribute (or (match-string-no-properties else-attribute-with-value)
                       (match-string-no-properties else-attribute-no-value)))
        (value (match-string-no-properties else-attribute-value))
        (this-token nil))
    (setq this-token (make-token :type (cdr (assoc attribute else-attribute-mapping))
                                 :value value))
    (unless (token-type this-token)
      (signal 'else-compile-error (list (format "Unrecognised attribute %s" attribute) (current-buffer))))

    (when (or (eq (token-type this-token) 'duplication)
              (eq (token-type this-token) 'type))
      (setf (token-value this-token) (cdr (assoc value else-attribute-mapping)))
      (unless (token-value this-token)
        (signal 'else-compile-error (list (format "Unrecognised attribute value %s" value) (current-buffer)))))
    this-token))

(defun else-strip-quotes (arg)
  "Strip any quotes from 'ARG and return it."
  (if (and (stringp arg)
           (string= (substring arg 0 1) "\""))
      (substring arg 1 (1- (length arg)))
    arg))

(defclass lexer ()
  ((buffer :initarg :buffer
           :initform nil)
   (position :initarg :position
             :initform nil)
   (token :initarg :token
          :initform nil)
   (token-valid :initarg :token-valid
                :initform nil
                :type boolean))
  "Lexer class to provide tokenised parsing of the buffer")

(cl-defmethod load-template ((obj lexer) file-name)
  "Load the indicated template file 'into' the lexer instance."
  (unless (file-exists-p file-name)
    (signal 'else-compile-error (list (format "%s doesn't exist" file-name) (current-buffer))))
  (save-excursion
    (oset obj :buffer (find-file file-name))
    (oset obj :position (point-min))))

(cl-defmethod load-buffer ((obj lexer))
  "Record the buffer and point of next search"
  (oset obj :buffer (current-buffer))
  (oset obj :position (point)))

(cl-defmethod push-back-token ((obj lexer) this-token)
  "Push the token 'back'"
  (oset obj :token-valid t)
  (oset obj :token (copy-token this-token)))

(cl-defmethod get-token ((obj lexer))
  "Get the next token - may be from the buffer or from the last token pushed back."
  (let ((value nil)
        (this-token nil)
        (next-token nil)
        (temp-token nil)
        (msg nil))
    (if (oref obj :token-valid)
        (progn
          (setq this-token (copy-token (oref obj :token)))
          (oset obj :token-valid nil))
      (save-match-data
        (set-buffer (oref obj :buffer))
        (catch 'scan-loop
          (while t
            (cond ((re-search-forward else-defining-string (line-end-position) t)
                   (setq this-token (extract-command-token))
                   (when (stringp this-token)
                     (signal 'else-compile-error (list this-token (oref obj :buffer)))))

                  ((re-search-forward  else-attribute-string (line-end-position) t)
                   (setq this-token (else-extract-attribute)))

                  ((re-search-forward else-end-define (line-end-position) t)
                   (setq this-token (make-token :type 'end-define)))

                  ((re-search-forward else-body-menu-with-description (line-end-position) t)
                   ;; process a line with an attached DESCRIPTION
                   ;; i.e. "xxx"/DESCRIPTION="hello there"
                   (setq this-token (list (make-token :type 'text
                                                      :value (match-string-no-properties 1))
                                          (make-token :type 'description
                                                      :value (match-string-no-properties 2)))))

                  ((re-search-forward else-text-string (line-end-position) t)
                   (setq this-token (make-token :type 'text
                                                :value (match-string-no-properties 1)))
                   ;; process a /PLACEHOLDER/TOKEN/FOLLOW/NOFOLLOW ?
                   (while (re-search-forward else-trailing-attribute (line-end-position) t)
                     (when (token-p this-token)
                       (setq this-token (list this-token)))
                     (setq next-token (make-token :type (cdr (assoc (match-string-no-properties 0) else-attribute-mapping))))
                     (unless (token-type next-token)
                       (setq msg (format "Unrecognised trailing attribute %s" (match-string-no-properties 0)))
                       (signal 'else-compile-error (list msg (oref obj :buffer))))
                     (setq this-token (append this-token (list next-token)))))

                  ((= (point) (point-max))
                   (setq this-token (make-token :type 'end-file)))
                  (t
                   nil))
            (forward-line)
            (when this-token
              (throw 'scan-loop nil))))))
    (if (listp this-token)
        (dolist (temp-token this-token)
          (setf (token-value temp-token) (else-strip-quotes (token-value temp-token)))
          (setf (token-line-no temp-token) (1- (line-number-at-pos))))
      (while (and (stringp (token-value this-token))
                  (> (length (token-value this-token)) 0)
                  (char-equal (aref (token-value this-token) (1- (length (token-value this-token)))) ?\ ))
        (setf (token-value this-token) (substring (token-value this-token) 0 (1- (length (token-value this-token))))))
      (setf (token-value this-token) (else-strip-quotes (token-value this-token)))
      (setf (token-line-no this-token) (1- (line-number-at-pos))))
    this-token))

(provide 'else-lexer)

;;; else-lexer.el ends here
