;;; else-structs.el --- structure/class definitions for ELSE.
;;
;; Copyright (C) 2017 Peter Milliken
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
;;
;; This file contains the definitions of the data structures and classes used to
;; generate the placeholder functionality of ELSE.  This is an attempt to
;; separate the data structures from the template language syntax/parsing (refer
;; to else-template.el) - because individuals in the past have suggested that
;; the structure of the actual template files could perhaps be generated in some
;; other form i.e. some form more Lisp-like.  By making this split, I hope to
;; make it easier for anybody who is interested in creating their own template
;; file syntax - all they need to do is convert whatever syntax they would
;; prefer into the data structures/classes defined below to make their template
;; definitions compatible with the core functionality of ELSE.
;;
(require 'cl-lib)
(require 'eieio)
(require 'else-lexer)

;;; Code:

(defcustom else-overwrite-placeholder-on-conflict nil
  "If non-nil, replace the placeholder with the new version.
When nil, throw an error."
  :type 'boolean
  :group 'ELSE)

(cl-defstruct menu-entry
  (type nil)                            ; 'placeholder or 'token or nil
  (text "")                             ; self-insert text or menu name
  (follow 'follow)                    ; follow menu entry
  (description ""))                     ; description from referenced menu

(cl-defstruct menu-item
  (text "")                             ; text of the menu line
  (summary ""))                         ; 'hint' to display with the menu text

(cl-defstruct insert-line   ; structure for a non-terminal placeholder body line
  (indent 0)                ; indent
  (text ""))                ; text to insert

(defclass else-repository ()
  ((languages :initarg :languages
              :initform nil))
  "Repository Class to hold all the language definitions as an association list.")

(cl-defmethod get-language-names ((obj else-repository))
  "Read out a list of names of all the language templates held in the repository."
  (let ((the-list nil))
    (dolist (language (oref obj :languages) the-list)
      (setq the-list (append (list (car language)) the-list)))))

(cl-defmethod language-list ((obj else-repository))
  "Return a list of languages in the repository."
  (oref obj :languages))

(cl-defmethod add-language ((obj else-repository) language)
  "Add a language instance to the repository"
  (unless (else-language-p language)
    (error "This is an error"))
  (when (assoc (oref language :name) (oref obj :languages))
    (error (format "Language %s already present, delete language first" (oref language :name))))
  (if (not (oref obj :languages))
      (oset obj :languages (list (cons (oref language :name) language)))
    (oset obj :languages (push (cons (oref language :name) language) (oref obj :languages)))))

(cl-defmethod delete-language ((obj else-repository) language-name)
  "Delete a language instance from the repository"
  (oset obj :languages (cl-remove language-name (oref obj :languages) :test 'equal :key 'car)))

(cl-defmethod access-language ((obj else-repository) language-name)
  "Access or find a language instance held by the repository"
  (cdr (assoc-string language-name (oref obj :languages) t)))

(cl-defmethod dump-language-to-file ((obj else-repository) name to-file)
  (let ((language-to-dump (access-language else-Language-Repository name))
        (write-marker nil)
        (language-output-buffer nil))
    (save-excursion
      (unless (file-directory-p (file-name-directory to-file))
        (make-directory (file-name-directory to-file)))
      (set-buffer (find-file-noselect to-file))
      (erase-buffer)
      (setq write-marker (point-min-marker))
      (print (oref language-to-dump :name) write-marker)
      (print (oref language-to-dump :initial-string) write-marker)
      (print (oref language-to-dump :punctuation-characters) write-marker)
      (print (oref language-to-dump :valid-identifier-characters) write-marker)
      (print (oref language-to-dump :tab-size) write-marker)
      (print (oref language-to-dump :version) write-marker)

      (mapatoms (lambda (e)
                  (print (symbol-plist e) write-marker))
                (oref language-to-dump placeholders))
      (save-buffer)
      (kill-buffer))))

(cl-defmethod restore-language-from-file ((obj else-repository) from-file)
  (let ((this-language nil)
        (read-marker nil)
        (read-item nil))
    (save-excursion
      (condition-case nil
          (progn
            (setq this-language (make-instance 'else-language))
            (set-buffer (find-file-noselect from-file))
            (setq read-marker (point-min-marker))
            (oset this-language :name (read read-marker))
            (oset this-language :initial-string (read read-marker))
            (oset this-language :punctuation-characters (read read-marker))
            (oset this-language :valid-identifier-characters (read read-marker))
            (oset this-language :tab-size (read read-marker))
            (oset this-language :version (read read-marker))
            (while t
              (setq read-item (read read-marker))
              (add-element this-language read-item)))
        (end-of-file
         (add-language else-Language-Repository this-language)
         (kill-buffer))))))

(defconst else-Placeholder-Vector-Size 9973
  "Size of each placeholder obarray.")

(defclass else-language ()
  ((name :initarg :name
         :initform "")
   (initial-string :initarg :initial-string
                   :initform "")
   (punctuation-characters :initarg :punctuation-characters
                           :initform (make-char-table (make-symbol "else-table")))
   (valid-identifier-characters :initarg :valid-identifier-characters
                                :initform "")
   (tab-size :initarg :tab-size
             :initform 0)
   (version :initarg :version
            :initform 0)
   (placeholders :initarg :placeholders
                 :initform (make-vector else-Placeholder-Vector-Size 0))
   (dirty :initarg :dirty
          :initform t)
   (placeholder-names :initarg :placeholder-names
                      :initform nil))
  "Language Class definition")

(cl-defmethod add-element ((obj else-language) element)
  "Add a placeholder to the language instance."
  (set-dirty obj t)
  (unless (or (else-terminal-placeholder-p element)
              (else-non-terminal-placeholder-p element)
              (else-menu-placeholder-p element))
    (signal 'else-compile-error (list "Attempting to add an illegal object to language" (current-buffer))))
  (when (intern-soft (oref element :name) (oref obj placeholders))

    (if else-overwrite-placeholder-on-conflict
        (delete-element (access-language else-Language-Repository
                                         (oref element :language-name))
                        (oref element :name))
      (signal 'else-compile-error (list (format "Placeholder %s already defined, delete first" (oref element :name))
                                        (current-buffer)))))
  (setplist (intern (upcase (oref element :name)) (oref obj :placeholders)) element))

(cl-defmethod delete-element ((obj else-language) name)
  "Delete a placeholder from the language instance."
  (oset obj :dirty t)
  (unintern (upcase name) (oref obj :placeholders)))

(cl-defmethod lookup ((obj else-language) name &optional ignore-forward-refs)
  "Lookup or access a placeholder definition in the language instance"
  (let ((definition nil))
    (setq definition (intern-soft (upcase name) (oref obj :placeholders)))
    (when definition
      (setq definition (symbol-plist definition)))
    (when (and definition
               (not ignore-forward-refs)
               (oref definition :reference))
      (setq definition (lookup obj (oref definition :reference))))
    definition))

(cl-defmethod get-names ((obj else-language))
  "Get the names of all the placeholder held by the language instance.
Sort them alphabetically."
  (let ((case-fold-search nil)
        (list-of-names nil))
    (when (oref obj :dirty)
      (mapatoms #'(lambda (element)
                    (push (oref (symbol-plist element) :name) list-of-names))
                (oref obj :placeholders))
      (oset obj :placeholder-names (sort list-of-names (lambda (left right)
                                                         (string< left right))))
      (set-dirty obj nil))
    (oref obj :placeholder-names)))

(cl-defmethod is-language-dirty ((obj else-language))
  "Has the language definition changed recently?
i.e. since the last time the language was tagged as non-dirty."
  (oref obj :dirty))

(cl-defmethod set-dirty ((obj else-language) value)
  "Set the dirty bit for the language instance."
  (when (booleanp value)
    (oset obj :dirty value)))

(defclass else-base ()
  ((name :initarg :name
         :initform "")
   (language-name :initarg :language-name
                  :initform "")
   (description :initarg :description
                :initform "")
   (reference :initarg :reference
              :initform nil)
   (before-action :initarg :before-action ; perform this action prior to expansion
                  :initform '())
   (after-action :initarg :after-action ;perform this action after expansion
                 :initform '())
   (insert-text :initarg :insert-text
                :initform '())
   (file-name :initarg :file-name
              :initform nil)
   (definition-line-number :initarg :definition-line-number
     :initform nil))
  "Placeholder Class base."
  :abstract "inheritance only")

(defclass else-placeholder-base (else-base)
  ((substitution :initarg :substitution
                 :initform nil)
   (substitution-count :initarg :substitution-count
                       :initform 1)
   (duplication :initarg :duplication
                :initform 'vertical)
   (separator   :initarg :separator
                :initform ""))
  "Base class for placeholders."
  :abstract "Base for self-insert, menu or terminal placeholders")

(defclass else-terminal-placeholder (else-placeholder-base)
  ((prompt :initarg :prompt
           :initform nil))
  "Terminal placeholder.")

(defclass else-non-terminal-placeholder (else-placeholder-base)
  ()
  "Non-Terminal placeholder")

(defclass else-menu-placeholder (else-placeholder-base)
  ((menu :initarg :menu
         :initform '()))
  "Menu Placeholder.")

(cl-defmethod build-menu ((obj else-menu-placeholder))
  "Build a menu list.
  (assoc list of form (menu-item . menu-entry) by following (or
   not) all of the menu entries to their ultimate expansion."
  (let ((definition nil)
        (menu-list nil)
        (this-list nil)
        (this-language (access-language else-Language-Repository (oref obj :language-name))))
    (dolist (item (oref obj :menu))
      (if (and (menu-entry-type item) (eq (menu-entry-follow item) 'follow))
          (progn
            (setq this-list (build-menu (lookup this-language (menu-entry-text item))))
            (if this-list
                (setq menu-list (append menu-list this-list))
              (setq menu-list (append menu-list (list (cons (make-menu-item :text (menu-entry-text item)
                                                                            :summary (menu-entry-description item))
                                                            item))))))
        (setq menu-list (append menu-list (list (cons (make-menu-item :text (menu-entry-text item)
                                                                      :summary (menu-entry-description item))
                                                      item))))))
    menu-list))

(cl-defmethod build-menu ((obj else-non-terminal-placeholder))
  "Build a 'menu' entry from a non-terminal definition. By it's
  very nature, this HAS to be a single line entry, so throw an
  error message if the definition contains more than one line of
  text."
  (let ((the-text (oref obj :insert-text)))
    (if (> (length the-text) 1)
        ;; throw an error
        (throw 'else-runtime-error
               (concat "ELSE run-time error: there is a menu entry with a '/FOLLOW' pointing to NON-TERMINAL placeholder "
                       (oref obj :name)
                       " (the NON-TERMINAL placeholder must have a single line of text only, this has multiple lines)"))
      (list (cons (make-menu-item :text (insert-line-text (car the-text))
                                  :summary nil)
                  (make-menu-entry :text (insert-line-text (car the-text))))))))

(cl-defmethod build-menu ((obj else-terminal-placeholder))
  "This is a catch-all error point i.e. we should never be asked
  to make a menu from a terminal placeholder."
  (throw 'else-runtime-error
         (concat "ELSE run-time error: there is a menu entry with a '/FOLLOW' pointing to TERMINAL placeholder "
                 (oref obj :name)
                 " (you can't make a menu from TERMINAL placeholders)")))

(cl-defmethod execute-before ((obj else-base))
  "If the placeholder has a /BEFORE action specified then execute it"
  (condition-case err
      (if (oref obj :before-action)
          (funcall (intern-soft (oref obj :before-action))))
    (void-function
     (message "Symbol's function definition is void: %s"
              (oref obj :before-action)))
    (error
     (message "%s" (error-message-string err)))))

(cl-defmethod execute-after ((obj else-base))
  "If the placeholder has a /AFTER action specified then execute it"
  (condition-case err
      (if (oref obj :after-action)
          (funcall (intern-soft (oref obj :after-action))))
    (void-function
     (message "Symbol's function definition is void: %s"
              (oref obj :after-action)))
    (error
     (message "%s" (error-message-string err)))))

(cl-defmethod expand ((obj else-menu-placeholder) insert-column)
   "Expand a MENU type placeholder."
  (let ((insert-position (point))
        (menu-list '())
        (selection nil)
        (selected-menu-entry nil))
    (setq menu-list (build-menu obj))
    ;; display the keys of the menu-list, return the index'd element (selected)
    ;; and access the menu-entry for expansion.
    (setq selected-menu-entry (cdr (nth (else-display-menu (cl-loop for entry in menu-list
                                                                    collect (car entry)))
                                        menu-list)))
    (if (menu-entry-type selected-menu-entry) ; placeholder?
        (expand (lookup else-Current-Language (menu-entry-text selected-menu-entry)) insert-column)
      (insert (menu-entry-text selected-menu-entry))
      (goto-char insert-position))))

(cl-defmethod expand ((obj else-terminal-placeholder) insert-column)
  "Expand a TERMINAL type placeholder"
  (let ((prompt-string nil))
    (else-display-menu (dolist (line (oref obj :prompt) prompt-string)
                         (setq prompt-string (concat prompt-string line "\n"))) t)))

(cl-defmethod expand ((obj else-base) insert-column)
  "Expand the self-insert text of a placeholder."
  (let ((tab-size (oref else-Current-Language :tab-size))
        (first-line t)
        (line nil)
        (this-buffer (current-buffer))
        (text (oref obj :insert-text)))
    (save-match-data
      (dolist (current-line text)
        ;; copy the entity because the following hard space conversion will
        ;; modify the original definition in the language instance
        (setq line (substring (insert-line-text current-line) 0))
        ;; convert any "hard spaces" to real spaces i.e. @ -> ' '
        (when (string-match "^@+" line)
          (store-substring line (match-beginning 0) (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
        (when (eq (compare-strings line 0 2 "\\@" 0 2) t)
          (setq line (substring line 1)))
        (unless first-line
          (princ (make-string (* tab-size (insert-line-indent current-line)) ?\ ) this-buffer))
        (princ line this-buffer)
        (setq first-line nil)
        (unless (memq current-line (last text))
          (terpri this-buffer)
          (indent-to insert-column))))))

(cl-defmethod add-line ((obj else-base) text)
  "Add an 'insert' line of text to the definition."
  (oset obj :insert-text (append (oref obj :insert-text) (list (make-insert-line :indent 0 :text text)))))

(cl-defmethod add-line ((obj else-terminal-placeholder) text)
  "Add a descriptive line of text to the definition."
  (oset obj :prompt (append (oref obj :prompt) (list text))))

(cl-defmethod add-line ((obj else-menu-placeholder) text type follow description)
  "Add a menu selection to the definition."
  (oset obj :menu (append (oref obj :menu) (list (make-menu-entry :text text
                                                                  :type type
                                                                  :follow follow
                                                                  :description description)))))

(cl-defmethod normalise-indentation ((obj else-base) tab-size)
  "Normalise indentation for the template.
Iterate over the lines of text that define the body and
'normalise' the indentation (spacing) of each line relative to
each other and against the tab-size for the language."
  (let ((smallest-indent 100)
        (this-indent 0)
        (this-entry nil)
        (lines-have-indentation nil))
    ;; scan the list for the smallest indent - 'normalise' to this value
    (dolist (this-line (oref obj :insert-text))
      (setq this-indent (string-match "\\S-" (insert-line-text this-line)))
      (when (and this-indent (> this-indent 0) (< this-indent smallest-indent))
        (setq smallest-indent this-indent)
        (setq lines-have-indentation t)))
    ;; calculate (normalised) indentation for each line of text
    (dotimes (index (length (oref obj :insert-text)))
      (setq this-entry (nth index (oref obj :insert-text)))
      (setq this-indent (string-match "\\S-" (insert-line-text this-entry)))
      (when (and this-indent
                 (> this-indent 0))
        (cond ((= this-indent smallest-indent)
               (setf (insert-line-indent this-entry) 1
                     (insert-line-text this-entry) (substring (insert-line-text this-entry) this-indent)))

              ((> this-indent smallest-indent)
               (if (= (% this-indent smallest-indent) 0)
                   (setf (insert-line-indent this-entry) (/ this-indent smallest-indent)
                         (insert-line-text this-entry) (substring (insert-line-text this-entry) this-indent))
                 (setf (insert-line-indent this-entry) (1+ (/ this-indent smallest-indent))
                       (insert-line-text this-entry) (substring (insert-line-text this-entry) this-indent))))
              (t
               nil))))))

(defvar else-Language-Repository (make-instance 'else-repository)
  "Instance of a ‘else-repository’.
Contains all of the template languages for this edit session.")

(defun else-compile-buffer (&optional start-at-point-min)
  "Compile the language template definitions from 'point' to the end."
  (interactive "P")
  (let ((operation nil)
        (err-msg nil)
        (op nil)
        (this-token nil)
        (previous-value nil)
        (end-of-buffer (point-max))
        (the-lexer (make-instance 'lexer))
        (languages-changed nil)
        (case-fold-search t))
    (condition-case err
        (progn
          (when start-at-point-min
            (goto-char (point-min)))
          (load-buffer the-lexer)
          (setq this-token (get-token the-lexer))
          (push-back-token the-lexer this-token)
          (while (not (eq (token-type this-token) 'end-file))
            (setq this-token (get-token the-lexer))
            (cl-case (token-type this-token)
              ('define-placeholder
                (push-back-token the-lexer this-token)
                (setq languages-changed (push (else-define-placeholder the-lexer) languages-changed)))

              ('delete-placeholder
               (setq previous-value (token-value this-token))
               (setq this-token (get-token the-lexer))
               (unless (eq (token-type this-token) 'language)
                 (setq err-msg "Expected a /LANGUAGE statement")
                 (signal 'else-compile-error (list err-msg (current-buffer))))
               (delete-element (access-language else-Language-Repository (token-value this-token)) previous-value)
               (setq languages-changed (push (token-value this-token) languages-changed)))

              ('define-language
                (push-back-token the-lexer this-token)
                (else-define-language the-lexer))

              ('delete-language
               (delete-language else-Language-Repository (else-strip-quotes (token-value this-token)))))))

      ((else-compile-error)
       (if (called-interactively-p 'any)
           (progn
             ;; handle the error here
             (message (format "Compile aborted - %s" (nth 1 err)))
             (set-buffer (nth 2 err)))
         ;; otherwise re-throw the error
         (signal 'else-compile-error (cdr err)))))
    (when languages-changed
      (delete-dups languages-changed)
      (save-current-buffer
        (dolist (name languages-changed)
          (dolist (this-buffer (buffer-list))
            (set-buffer this-buffer)
            (when (and else-Current-Language
                       (string= (oref else-Current-Language :name) (upcase name)))
              (setq else-Current-Language (access-language else-Language-Repository name)))))))))

(defun else-define-language (the-lexer)
  "Parse a language definition."
  (let ((this-language (make-instance 'else-language))
        (this-token (get-token the-lexer))
        (type nil)
        (value nil)
        (temp nil))
    (while (not (eq (token-type this-token) 'end-define))
      (setq type (token-type this-token)
            value (token-value this-token))
      (cl-case type
        ('define-language
          (oset this-language :name (else-strip-quotes value)))
        ('initial-string
         (oset this-language :initial-string value))
        ('punctuation-characters
         (dotimes (index (length value))
           (set-char-table-range (oref this-language :punctuation-characters)
                                 (aref value index)
                                 t)))
        ('valid-identifier-characters
         (oset this-language :valid-identifier-characters value))
        ('indent-size
         (oset this-language :tab-size (string-to-number value)))
        ('version
         (oset this-language :version value)))
      (setq this-token (get-token the-lexer)))
    ;; note: it is legal to (re)define a language in order to change the
    ;; attributes - specifically the tab-size, so make an exception if the
    ;; language definition is already present by issuing a warning message
    (if (not (access-language else-Language-Repository (oref this-language :name)))
        (add-language else-Language-Repository this-language)
      (message (format "Warning: Language %s is already defined, assuming modification of tab size" (oref this-language :name)))
      (setq temp (access-language else-Language-Repository (oref this-language :name)))
      (when (> (oref this-language :tab-size) 0)
        (oset temp :tab-size (oref this-language :tab-size))))))

(defun else-define-placeholder (the-lexer )
  "Parse a placeholder definition."
  (let ((this-token (get-token the-lexer))
        (this-placeholder nil)
        (this-language nil)
        (placeholder-name nil)
        (language-name nil)
        (substitution 'auto-substitute)
        (sub-count 1)
        (description "")
        (duplication 'context-dependent)
        (separator "")
        (before-act nil)
        (after-act nil)
        (placeholder-reference nil)
        (type nil)
        (value nil)
        (definition-line-no nil)
        (definition-file nil))
    (setq placeholder-name (token-value this-token))
    (setq definition-line-no (token-line-no this-token)
          definition-file (file-name-nondirectory (buffer-file-name)))
    (setq this-token (get-token the-lexer))
    (while (and (not (eq (token-type this-token) 'end-define))
                (not (eq (token-type this-token) 'end-file)))
      (setq type (token-type this-token)
            value (token-value this-token))
      (cl-case type
        ('language
         (setq this-language (access-language else-Language-Repository value)
               language-name value)
         (unless this-language
           (signal 'else-compile-error (list (format "Language %s has not been defined yet - aborting" value)
                                             (current-buffer)))))

        ('noauto-substitute
         (setq substitution type))

        ('auto-substitute
         (setq substitution type))

        ('substitution-count
         (setq sub-count (string-to-number value)))

        ('description
         (setq description value))

        ('duplication
         (setq duplication value))

        ('separator
         (setq separator value))

        ('before-action
         (setq before-act value))

        ('after-action
         (setq after-act value))

        ('type
         (push-back-token the-lexer this-token)
         (cl-case value
           ('nonterminal
            (setq this-placeholder (make-instance 'else-non-terminal-placeholder
                                                  :name placeholder-name
                                                  :language-name language-name
                                                  :description description
                                                  :substitution substitution
                                                  :substitution-count sub-count
                                                  :duplication duplication
                                                  :separator separator
                                                  :before-action before-act
                                                  :after-action after-act
                                                  :file-name definition-file
                                                  :definition-line-number definition-line-no))
            (setq this-placeholder (else-scan-non-terminal-body the-lexer this-placeholder (oref this-language :tab-size))))

           ('terminal
            (setq this-placeholder (make-instance 'else-terminal-placeholder
                                                  :name placeholder-name
                                                  :language-name language-name
                                                  :description description
                                                  :substitution substitution
                                                  :substitution-count sub-count
                                                  :duplication duplication
                                                  :separator separator
                                                  :before-action before-act
                                                  :after-action after-act
                                                  :file-name definition-file
                                                  :definition-line-number definition-line-no))
            (setq this-placeholder (else-scan-terminal-body the-lexer this-placeholder)))

           ('menu
            (setq this-placeholder (make-instance 'else-menu-placeholder
                                                  :name placeholder-name
                                                  :language-name language-name
                                                  :description description
                                                  :substitution substitution
                                                  :substitution-count sub-count
                                                  :duplication duplication
                                                  :separator separator
                                                  :before-action before-act
                                                  :after-action after-act
                                                  :file-name definition-file
                                                  :definition-line-number definition-line-no))
            (setq this-placeholder (else-scan-menu-body the-lexer this-placeholder)))))

        ('placeholder
         (setq this-placeholder (make-instance 'else-non-terminal-placeholder
                                               :name placeholder-name
                                               :language-name language-name
                                               :reference value
                                               :file-name definition-file
                                               :definition-line-number definition-line-no))))
      (setq this-token (get-token the-lexer)))
    (add-element this-language this-placeholder)
    (oref this-placeholder :language-name)))

(defun else-extract-placeholder ()
  "Place the definition of an placeholder into the buffer at point."
  (interactive)
  (let ((selected-definition nil)
        (name nil))
    (else-run-when-active
     (setq name (upcase (completing-read "Name: " (get-names else-Current-Language))))
     (when name
       (dump (lookup else-Current-Language name t) (oref else-Current-Language :tab-size))))))

(cl-defun else-extract-all (&optional language)
  "Extract the full language definition at point.
Request the language name from the user and extract the entire
language template set into the current buffer"
  (interactive)
  (let ((name language)
        (else-Language-Specifics nil))
    (unless name
      (setq name (completing-read "Language: " (get-language-names else-Language-Repository))))
    (when name
      (dump (access-language else-Language-Repository name)))))

(provide 'else-structs)

;;; else-structs.el ends here
