;;; ocaml-utils-mode.el --- Emacs Minor Mode for OCaml utilitites

;; Copyright (C) 2024  mattiasdrp


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

(defvar ocaml-utils-dune-history nil
  "The history list for dune watch builds.")

(defcustom ocaml-utils-lock-dune nil
  "Should dune watch take the lock?")

;;;###autoload
(defun ocaml-utils-dune-watch ()
  "Will call dune build -w BUILD on an async process."
  (interactive)
  (cond
   ((and-let* ((window (get-buffer-window "*dune watch*")))
      (aw-switch-to-window window)))
   ((and-let* ((buffer (get-buffer "*dune watch*")))
      (with-current-buffer buffer
        (with-selected-window
            (display-buffer-at-bottom (current-buffer)
                                      '((window-height . 0.2)))
          (set-window-dedicated-p (selected-window) t)
          (compilation-minor-mode t)))))
   ((let ((build (read-from-minibuffer "Build name: " nil nil nil 'ocaml-utils-dune-history))
          (buffer (get-buffer-create "*dune watch*"))
          (inhibit-read-only t))
      (with-current-buffer buffer
        (projectile-run-async-shell-command-in-root
         (concat (if ocaml-utils-lock-dune
                     ""
                   "DUNE_CONFIG__GLOBAL_LOCK=disabled ")
                 "dune build -w " build) buffer)
        ;; Make this process non blocking for killing
        ;; (defun ocaml-utils-erase-and-fill-buffer-no-lambda ()
        ;;   "Wrapper to avoid using lambda"
        ;;   (ocaml-utils-erase-and-fill-buffer buffer))
        ;; (add-hook 'after-save-hook #'ocaml-utils-erase-and-fill-buffer-no-lambda)
        (with-selected-window
            (display-buffer-at-bottom (current-buffer)
                                      '((window-height . 0.2)))
          (set-window-dedicated-p (selected-window) t)
          (compilation-minor-mode t))
        (set-process-query-on-exit-flag (get-buffer-process buffer) nil))))))

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

(defun ocaml-utils--extract-name (content)
  (let* ((value (plist-get content :value))
         (re_name "[\n\r]*```ocaml[[:space:]]\\(.*\\)[[:space:]]")
         (_ (string-match re_name value))
         (name (match-string 1 value)))
    name))

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
