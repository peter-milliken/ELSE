;;; else-template.el --- template file loading facilities for ELSE
;;
;; Copyright (C) 2016 Peter Milliken
;;
;; Author: Peter Milliken <peter.milliken@gmail.com>
;;
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

;;; Commentary:

;; This file (attempts to) separate the functions that deal with the syntax of
;; language template files from the rest of the ELSE functionality.  The idea is
;; that if the user so chooses, they can substitute their own syntax for
;; language template files i.e. more 'Lisp-like' syntax? and replace these
;; functions with functions that can process that syntax into the data
;; structures defined in else-structs.el.
;;

(require 'else-structs)

;;; Code:

(defconst else-lse-ext "\.lse")
(defconst else-esl-ext "\.esl")

(defun else-dump-definition-header (definition)
  "Print the DELETE/DEFINE PLACEHOLDER statements."
  (let ((name (oref definition :name))
        (formatted-string nil)
        (indent-column 4)
        (delete-define-type '("DELETE PLACEHOLDER " "DEFINE PLACEHOLDER ")))
    (mapc #'(lambda (s)
              (insert s)
              (if (string-match-p " \\|\\W" name)
                  (setq formatted-string "\"%s\"")
                (setq formatted-string "%s"))
              (insert (format formatted-string name))
              (newline)
              ;; deliberately 'inherit' the variable 'indent-column from the caller
              (indent-to indent-column)
              (if (string-match-p "\\s-" (oref definition :language-name))
                  (insert (concat "/LANGUAGE=\"" (oref definition :language-name) "\""))
                (insert (concat "/LANGUAGE=" (oref definition :language-name))))
              (newline))
          delete-define-type)))

(cl-defgeneric dump (obj tab-size)
  ())

(cl-defmethod dump ((obj else-placeholder-base))
  "Print the 'base' portion of a placeholder"
  (let ((insert-position (point))
        (indent-column 4)
        (formatted-string nil))
    (message "base")
    (if (oref obj :reference)
        (progn
          (indent-to indent-column)
          (insert (format "/PLACEHOLDER=\"%s\"" (oref obj :reference))))
      (indent-to indent-column)
      (if (eq (oref obj :substitution) 'noauto-substitute)
          (insert "/NOAUTO_SUBSTITUTE")
        (insert "/AUTO_SUBSTITUTE")
        (newline)
        (indent-to indent-column)
        (insert (concat "/SUBSTITUTE_COUNT=" (number-to-string (oref obj :substitution-count)))))
      (newline)
      (indent-to indent-column)
      (insert (format "/DESCRIPTION=\"%s\"" (oref obj :description)))
      (newline)
      (indent-to indent-column)
      (insert (format "/DUPLICATION=%s" (cl-case (oref obj :duplication)
                                          ('context-dependent "CONTEXT_DEPENDENT")
                                          ('vertical "VERTICAL")
                                          ('horizontal "HORIZONTAL"))))
      (newline)
      (indent-to indent-column)
      (insert (format "/SEPARATOR=\"%s\"" (oref obj :separator)))
      (newline)
      (if (oref obj :before-action)
          (progn
            (indent-to-column indent-column)
            (insert (format "/BEFORE=%s" (oref obj :before-action)))
            (newline)))
      (if (oref obj :after-action)
          (progn
            (indent-to-column indent-column)
            (insert (format "/AFTER=%s" (oref obj :after-action)))
            (newline))))))

(cl-defmethod dump ((obj else-non-terminal-placeholder) tab-size)
  "Print the details of a non-terminal placeholder"
  (let ((indent-column 4)
        (text nil))
    (message "non-terminal")
    (else-dump-definition-header obj)
    (cl-call-next-method obj)
    ;; placeholders that reference another placeholder are non-terminal by
    ;; default, so catch that exception here
    (if (not (oref obj :reference))
        (progn
          (indent-to indent-column)
          (insert "/TYPE=NONTERMINAL")
          (newline)
          (newline)
          (indent-to indent-column)
          (dolist (this-line (oref obj :insert-text))
            (indent-to indent-column)
            (setq text (insert-line-text this-line))
            (unless (eq (insert-line-indent this-line) 0)
              (setq text (concat (make-string (* tab-size (insert-line-indent this-line)) ?\ )
                                 text)))
            (insert (format "\"%s\"" text))
            (newline)))
      (newline))
    (else-dump-end-define)))

(cl-defmethod dump ((obj else-menu-placeholder) tab-size)
  "Print the details of a menu placeholder"
  (let ((indent-column 4))
    (else-dump-definition-header obj)
    (cl-call-next-method obj)
    (indent-to indent-column)
    (insert "/TYPE=MENU")
    (newline)
    (dolist (this-line (oref obj :menu))
      (newline)
      (indent-to indent-column)
      (insert (format "\"%s\"" (menu-entry-text this-line)))
      (cl-case (menu-entry-type this-line)
        ('placeholder
         (insert "/PLACEHOLDER")
         (cl-case (menu-entry-follow this-line)
           ('follow (insert "/FOLLOW"))
           ;;; for the moment, since nofollow is the default on creation, don't
           ;;; print the /NOFOLLOW
           ('nofollow nil))))
      (when (> (length (menu-entry-description this-line)) 0)
        (insert (format "/DESCRIPTION=\"%s\"" (menu-entry-description this-line)))))
    (newline)
    (else-dump-end-define)))

(cl-defmethod dump ((obj else-terminal-placeholder) tab-size)
  "Print the details of a terminal placeholder"
  (let ((indent-column 4))
    (else-dump-definition-header obj)
    (cl-call-next-method obj)
    (indent-to indent-column)
    (insert "/TYPE=TERMINAL")
    (newline)
    (dolist (this-line (oref obj :prompt))
      (newline)
      (indent-to indent-column)
      (insert (format "\"%s\"" this-line)))
    (newline)
    (else-dump-end-define)))

(cl-defmethod dump ((obj else-language))
  "Print a complete language definition into the current buffer."
  (let ((index nil)
        (p-chars nil)
        (indent-column 4))

    (insert (format "DELETE LANGUAGE \"%s\"" (oref obj :name)))
    (newline)
    (insert (format "DEFINE LANGUAGE \"%s\"" (oref obj :name)))
    (newline)
    (indent-to indent-column)
    (insert (format "/INITIAL_STRING=\"%s\"" (oref obj :initial-string)))
    (newline)
    (map-char-table #'(lambda (key value)
                        (if (listp key)
                            (progn
                              (setq index (car key))
                              (while (<= index (cdr key))
                                (setq p-chars (concat (string index) p-chars))
                                (setq index (1+ index))))
                          (setq p-chars (concat (string key) p-chars))))
                    (oref obj :punctuation-characters))
    (indent-to indent-column)
    (insert (format "/PUNTUATION_CHARACTERS=\"%s\"" p-chars))
    (newline)
    (indent-to indent-column)
    (insert (format "/INDENT_SIZE=%s" (oref obj :tab-size)))
    (newline)
    (indent-to indent-column)
    (insert (format "/VERSION=%s" (oref obj :version)))
    (newline)
    (else-dump-end-define)
    (newline)
    (dolist (name (get-names obj))
      (dump (symbol-plist (intern-soft name (oref obj :placeholders))) (oref obj :tab-size)))))

(defun else-dump-end-define ()
  "Print the END DEFINE statement."
  (newline)
  (insert "END DEFINE")
  (newline)
  (newline))

(defun else-derive-language-name-from-mode-name ()
  "Derive the language name.
Use the major mode name and check if the name needs processing
through the translation table."
  (let ((language-name mode-name))
    ;; Check to see if there is a translation or alternative naming
    ;; for the mode name i.e. in Emacs 22.1, the mode name for files
    ;; with a .c extension changed from "C" to "C/l" - which meant
    ;; either the language template file(s) should have their name
    ;; changed or ELSE be made to accomodate this 'quirk' - the latter
    ;; was chosen.
    (when (assoc language-name else-Alternate-Mode-Names)
      (setq language-name
            (cdr (assoc language-name else-Alternate-Mode-Names))))
    language-name))

(defun else-load-file-and-compile (language-name language-files)
  "Load the language templates.
From the language files in 'language-files, either load the
.lse/-cust.lse files or the .esl file (whichever is newer).  If
the .esl file is either out of date or missing, create a new file
in the same directory as the -cust.lse file or (if the -cust.lse
file is missing) in the same directory as the .lse file."
  (let ((template-file-already-loaded nil)
        (primary-language-file nil)
        (custom-language-file nil)
        (fast-load-file nil)
        (load-from-file t)
        (disable-write-fast-load nil) ; refer to condition-case below
        (result nil))
    (save-excursion
      (dolist (file-name language-files)
        (cond ((string-suffix-p else-lse-ext file-name)
               (if (string-suffix-p "-cust.lse" file-name)
                   (setq custom-language-file file-name)
                 (setq primary-language-file file-name)))
              ((string-suffix-p else-esl-ext file-name)
               (setq fast-load-file file-name))))
      ;; Emacs 26.1 introduced some changed internals dealing with writing Lisp
      ;; Objects and the reading of the fast load file fails with an
      ;; invalid-read-syntax. To get around the problem, intercept the error and
      ;; force ELSE to read from the language files - and disable (re)writing
      ;; the fast load file!
      (condition-case err
          ;; give precedence to the fast-load file
          (if fast-load-file
              (if custom-language-file
                  (when (and (file-newer-than-file-p fast-load-file primary-language-file)
                             (file-newer-than-file-p fast-load-file custom-language-file))
                    (restore-language-from-file else-Language-Repository fast-load-file)
                    (setq load-from-file nil))
                (when (file-newer-than-file-p fast-load-file primary-language-file)
                  (restore-language-from-file else-Language-Repository fast-load-file)
                  (setq load-from-file nil))))
        ((invalid-read-syntax)
         (setq load-from-file t)
         (setq disable-write-fast-load t)))
      (when load-from-file
        ;; otherwise load the definitions from the (.lse) language files - create
        ;; a (new) .esl file in the else-fast-load-directory
        (setq fast-load-file (concat else-fast-load-directory
                                     "/"
                                     (file-name-base primary-language-file)
                                     else-esl-ext))
        (dolist (file-name (list primary-language-file custom-language-file))
          (when file-name
            (setq template-file-already-loaded (get-file-buffer file-name))
            (set-buffer (find-file-noselect file-name))
            (else-compile-buffer t)
            ;; Only delete the buffer if we had to load it explicitly, otherwise leave
            ;; it alone.
            (unless template-file-already-loaded
              ;; Only want to clean up when it succeeded, otherwise the
              ;; user probably wants to see the error line.
              (kill-buffer (get-file-buffer file-name)))))
        (unless disable-write-fast-load
          (dump-language-to-file else-Language-Repository language-name fast-load-file))))))

(defun else-load-language ()
  "Load the template language for the current buffer."
  (let ((found-template-name nil)
        (language-file-names nil)
        (language-name))
    ;; default to no language for the buffer
    (setq else-Current-Language nil)
    (setq language-name (else-derive-language-name-from-mode-name))
    ;; if the language is not already loaded (for another buffer) then locate
    ;; the template file(s) and load it
    (setq else-Current-Language (access-language else-Language-Repository language-name))
    (unless else-Current-Language
      (setq language-file-names (else-locate-language-file language-name))
      ;; if we are unable to locate the template file(s), it might be because the
      ;; derived mode name wasn't correct i.e. the template file for the LSE
      ;; language definition itself is called template.lse, so ask the user.
      (if (> (length language-file-names) 0)
          (progn
            ;; have a file name and the language is not loaded, so load it into
            ;; a buffer and compile it
            (else-load-file-and-compile language-name language-file-names)
            (setq else-Current-Language (access-language else-Language-Repository language-name)))
        ;; otherwise request a file/template name from the user
        (setq language-name
              (read-from-minibuffer "Enter the language name (no file extensions, please): "))
        ;; if that language is not already load then locate, load and compile
        ;; it
        (setq else-Current-Language (access-language else-Language-Repository language-name))
        (unless else-Current-Language
          (setq language-file-names (else-locate-language-file language-name))
          (unless (> (length language-file-names) 0)
            (signal 'else-loading-error (list (format "Unable to locate language file %s.lse" language-name))))
          (else-load-file-and-compile language-name language-file-names)
          (setq else-Current-Language (access-language else-Language-Repository language-name))))))
  (else-language-p else-Current-Language))

(defun else-locate-language-file (name)
  "Locate possible language file candidates.
Search the load/fast-load-path looking for a language definition
file, a custom(ised) language file and any fast-load language
file.  Return the results as a list (or nil if no files are
found)"
  (let ((files '())
        (candidates nil)
        (located-file nil))
    (push (list load-path (concat name else-lse-ext)) candidates)
    (push (list load-path (concat name "-cust" else-lse-ext)) candidates)
    (push (list (list else-fast-load-directory) (concat name else-esl-ext)) candidates)
    (dolist (file-specification candidates files)
      (setq located-file (else-search-path-for-file file-specification))
      (when located-file
        (setq files (push located-file files))))
    files))

(defun else-scan-menu-body (the-lexer placeholder)
  "Scan the body of a menu placeholder."
  (let ((this-token (get-token the-lexer))
        (text nil)
        (type nil)
        (follow nil)
        (description nil)
        (type-of-token nil))
    (catch 'scan-loop
      (setq this-token (get-token the-lexer))
      (while t
        (setq type nil
              follow 'nofollow
              description nil)
        (when (and (not (listp this-token))
                   (or (eq (token-type this-token) 'end-define)
                       (eq (token-type this-token) 'end-file)))
          (push-back-token the-lexer this-token)
          (throw 'scan-loop nil))

        (if (not (listp this-token))          ; just a single token (text)?
            (setq text (token-value this-token))
          ;; (add-line placeholder (token-value this-token) nil nil nil)
          (dolist (element this-token)
            (setq type-of-token (token-type element))
            (cl-case type-of-token
              ('text
               (setq text (token-value element)))
              ('follow
               (setq follow type-of-token))
              ('nofollow
               (setq follow type-of-token))
              ('placeholder
               (setq type type-of-token))
              ('description
               (setq description (token-value element)))
              (t
               (signal 'else-compile-error (list "Unpected error scanning menu body" (current-buffer)))))))
        (add-line placeholder text type follow description)
        (setq this-token (get-token the-lexer))))
    placeholder))

(defun else-scan-non-terminal-body (the-lexer element tab-size)
  "Scan the body of a non-terminal placeholder."
  (let ((this-token (get-token the-lexer))
        (this-line nil))
    (catch 'scan-loop
      (unless (eq (token-type this-token) 'text)
        (setq this-token (get-token the-lexer)))
      (while (and (not (eq (token-type this-token) 'end-define))
                  (not (eq (token-type this-token) 'end-file)))
        (if (eq (token-type this-token) 'text)
            (add-line element (token-value this-token))
          (unless (eq (token-type this-token) 'end-define)
            (signal 'else-compile-error (list (format "illegal entry found in template body")
                                              (current-buffer))))
          (push-back-token the-lexer this-token)
          (throw 'scan-loop nil))
        (setq this-token (get-token the-lexer)))
      (normalise-indentation element tab-size)
      (push-back-token the-lexer this-token))
    element))

(defun else-scan-terminal-body (the-lexer placeholder)
  "Scan the body of a terminal placeholder."
  (let ((this-token (get-token the-lexer)))
    (catch 'scan-loop
      (setq this-token (get-token the-lexer))
      (while (and (not (eq (token-type this-token) 'end-define))
                  (not (eq (token-type this-token) 'end-file)))
        (if (eq (token-type this-token) 'text)
            (add-line placeholder (token-value this-token))
          (unless (eq (token-type this-token) 'end-define)
            (signal 'else-compile-error (list "illegal entry found in template body"
                                              (current-buffer))))
          (push-back-token the-lexer this-token)
          (throw 'scan-loop nil))
        (setq this-token (get-token the-lexer)))
      (push-back-token the-lexer this-token))
    placeholder))

(defun else-search-path-for-file (file-spec)
  "Search load path for 'file-spec."
  (let ((search-path (car file-spec))
        (this-attempt nil))
    (catch 'found-file
      (dolist (this-attempt search-path nil)
        (if (file-exists-p (expand-file-name (concat this-attempt "/" (cadr file-spec))))
            (throw 'found-file (expand-file-name (concat this-attempt "/" (cadr file-spec)))))))))

(provide 'else-template)

;;; else-template.el ends here
