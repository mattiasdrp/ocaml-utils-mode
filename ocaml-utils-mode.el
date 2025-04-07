;;; ocaml-utils-mode.el --- Emacs Minor Mode for OCaml utilities -*- lexical-binding: t -*-

;; Copyright (C) 2024 mattiasdrp


;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 26 february 2024
;; Version: 0.1.0
;; Licence: MIT
;; Keywords: emacs, tools, ocaml
;; URL: https://github.com/mattiasdrp/ocaml-utils-mode
;; Copyright (c) 2024 mattiasdrp and contributors.
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; This minor mode adds some utilities to OCaml in emacs

;;; Code:

(require 'ace-window)
(require 'caml nil t)
(require 'dash)
(require 'ht)
(require 'lsp-mode)

(defvar ocaml-utils-dune-history nil
  "The history list for dune watch builds.")

(defcustom ocaml-utils-lock-dune nil
  "Should dune watch take the lock?")

;;; DUNE WATCH

(defun ocaml-utils--generate-command (build-system command)
  "Generate a dune or make COMMAND depending on BUILD-SYSTEM."
  (concat
   (pcase build-system
     (`make (format "BUILD-EXTRA=--watch make %s" command))
     (`dune (format "opam exec -- dune %s --watch" command)))))

(defcustom ocaml-utils--dune-watch-buffer "*dune watch*"
  "Buffer for dune watch results.")

(defvar-local ocaml-utils--dune-watch-process nil
  "Currently running dune watch process.")

(defun ocaml-utils--display-dune-watch-buffer (buffer)
  "Displays the buffer in a new dedicated window.
If DISPLAY-BUFFER-ALIST contains a rule for such buffers, displays it in this window instead"
  (if (display-buffer-assq-regexp buffer display-buffer-alist nil)
      (display-buffer buffer)
    (display-buffer-at-bottom
     buffer
     '((window-height . 0.2)))))

;;;###autoload
(defun ocaml-utils-start-dune-watch (build-system)
  "Start a subprocess to run the BUILD-SYSTEM applied to a rule using the --watch flag."
  (interactive
   (list
    (intern
     (completing-read "Choose a build system: "
                      '("make" "dune")))))
  (let* ((command (read-from-minibuffer "Build name: " nil nil nil 'ocaml-utils-dune-history))
         (command (ocaml-utils--generate-command build-system command)))
    (message "starting process to watch %s task..."  command)
    (projectile-run-async-shell-command-in-root command))
  (add-hook 'lsp-diagnostics-updated-hook 'ocaml-utils--report-diagnostics nil t))

(defun ocaml-utils--report-diagnostics ()
  "Report newly received diagnostics in a dune-watch buffer that behaves like the compilation buffer."
  (let* ((diagnostics (lsp-diagnostics t))
         (formatted-diagnostics (ocaml-utils--format-diagnostics diagnostics))
         (buffer (get-buffer-create ocaml-utils--dune-watch-buffer))
         (window (if-let* ((window (get-buffer-window buffer)))
                     window
                   (ocaml-utils--display-dune-watch-buffer buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (with-selected-window window
          (erase-buffer)
          (dolist (diagnostic (-flatten formatted-diagnostics))
            (insert diagnostic))
          (compilation-mode t))))))

;; (add-hook 'lsp-diagnostics-updated-hook 'ocaml-utils--report-diagnostics nil t)

(defun ocaml-utils--format-per-file-diagnostic (file messages)
  (--map (-let* (((&Diagnostic :message :severity?
                               :range (range &as &Range
                                             :start (&Position :line start-line :character)
                                             :end (&Position :line end-line))) it)
                 ((start . end) (lsp--range-to-region range))
                 (severity-string (if (= severity? 1) "Error" "Warning")))
           (format "%s:%d:%d: %s %s\n\n" file start-line character severity-string message))
         messages))

(defun ocaml-utils--format-diagnostics (diagnostics)
  "Convert LSP diagnostics to a format suitable for the Emacs compilation buffer."
  (ht-map #'ocaml-utils--format-per-file-diagnostic diagnostics))

(defun ocaml-utils--transform-constructor (value)
  "Transform an OCaml constructor in an elisp cons cell.

If the constructor is a constant, returns a cons with its car set to it
and its cdr set to nil.
Otherwise, the cdr is the parameters attached to the constructor."
  (let* ((lvalue (string-split value "of"))
         (left (string-trim (car lvalue)))
         (right (nth 1 lvalue)))
    (if right
        (cons left (string-trim right))
      (cons left nil))))

(defun ocaml-utils--pp-variant (value)
  "For each constructors in the type declaration, create a pattern matching branch.

If the constructor expects arguments, add a ~_~."
  (let* ((values (s-split "|" value t))
         (values (mapcar #'ocaml-utils--transform-constructor values))
         (txt
          (seq-reduce (lambda (acc val)
                        (pcase val
                          (`(,left . nil)  (concat acc "\n| " left " -> _"))
                          (`(,left . ,right)  (concat acc "\n| " left " _ -> _")))) values "" )))
    (string-trim-left txt)))

(defun ocaml-utils--lsp-type-at-point ()
  "Queries LSP for the type at point."
  (-some->> (lsp--text-document-position-params)
    (lsp--make-request "textDocument/hover")
    (lsp--send-request)
    (lsp:hover-contents)))

(defun ocaml-utils--parse-type (content index)
  "Parses the type returned by LSP."
  (let* ((value (plist-get content :value))
         (regexp "[\n\r]+\\|```\\(?:ocaml\\)?\\|.*=")
         (value (replace-regexp-in-string regexp "" (or value "")))
         (value (string-trim value)))
    `(:index ,index
             :value ,value
             :type ,(ocaml-utils--get-type value))))

(defun ocaml-utils--get-type (string)
  "Returns the kind of type.

A variant, a record, an option a result or anything else."
  (cond
   ((string-prefix-p "|" string) :variant)
   ((string-prefix-p "{" string) :record)
   ((string-suffix-p "option" string) :option)
   ((string-suffix-p "result" string) :result)
   ((string-match-p "|" string) :variant)
   (t :other)))

;;; TYPES

(defun ocaml-utils--types-alist ()
  "Returns the list of all types declared in the current buffer before point."
  (save-excursion
    (let ((type-alist (make-hash-table :test 'equal)))
      (goto-char (line-beginning-position 2))
      ;; collect definitions
      (while (caml-prev-index-position-function)
        (let* ((name (caml-match-string 5)))
          (when (looking-at "[ \t]*type")
            (save-excursion
              (let* ((_ (forward-char 5))
                     (index (point))
                     (content (ocaml-utils--lsp-type-at-point))
                     (type (ocaml-utils--parse-type content index)))
                (puthash name type type-alist))))
          (when (looking-at "[ \t]*and")
            (save-excursion
              (let* ((_ (forward-char 4))
                     (index (point))
                     (content (ocaml-utils--lsp-type-at-point))
                     (type (ocaml-utils--parse-type content index)))
                (puthash name type type-alist))))))
      type-alist)))

(defun ocaml-utils--insert-type (value word)
  "If the value represents an variant, inserts it's destruction as a pattern matching."
  (when (eq (plist-get value :type) :variant)
    (let* ((txt (ocaml-utils--pp-variant (plist-get value :value)))
           (word (or word "_"))
           (txt (concat "match " word " with\n" txt))
           (bounds (bounds-of-thing-at-point 'symbol))
           (start (point)))
      (when bounds (delete-region (car bounds) (cdr bounds)))
      (insert txt)
      (indent-region start (point)))))

(defun ocaml-utils--complete-type (word)
  "Asks the user for a type and if this type is a variant, destruct it."
  (let* ((start (point))
         (types-alist (ocaml-utils--types-alist))
         (type-chosen (completing-read "Choose a type: " types-alist))
         (value (gethash type-chosen types-alist)))
    (ocaml-utils--insert-type value word)))

;;; HELPERS

(defun ocaml-utils--extract-name (content)
  (let* ((value (plist-get content :value))
         (re_name "[\n\r]*```ocaml[[:space:]]\\(.*\\)[[:space:]]")
         (_ (string-match re_name value))
         (name (match-string 1 value)))
    name))

;;; MAIN FUNCTIONS

;;;###autoload
(defun ocaml-utils-destruct ()
  "Destruct the variable at point.

If no type can be inferred for the variable, asks the user for a type."
  (interactive)
  (let ((content (ocaml-utils--lsp-type-at-point))
        (word (thing-at-point 'symbol)))
    (if content
        (let* ((types-alist (ocaml-utils--types-alist))
               (name (ocaml-utils--extract-name content))
               (value (gethash name types-alist)))
          (if value
              (ocaml-utils--insert-type value word)
            (ocaml-utils--complete-type word)))
      (ocaml-utils--complete-type word))))

(defcustom ocaml-utils-keymap-prefix "\C-c \C-o"
  "The prefix for ocaml-utils-mode key bindings."
  :type 'key
  :group 'ocaml-utils)

(defun ocaml-utils--key (key)
  (kbd (concat ocaml-utils-keymap-prefix " " key)))

;;; MODE

;;;###autoload
(define-minor-mode ocaml-utils-mode
  "Toggles buffer local ocaml-utils-mode."
  :init-value nil
  :global nil
  :group 'ocaml-utils
  :lighter " ocaml-utils"
  :keymap
  (list
   (cons (ocaml-utils--key "w") #'ocaml-utils-dune-watch)
   (cons (ocaml-utils--key "a") #'ocaml-utils-destruct)))

(provide 'ocaml-utils-mode)
;;; ocaml-utils-mode.el ends here
