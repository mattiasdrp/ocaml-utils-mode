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
(require 'transient)
(require 'compile)
(require 'projectile)

(defvar ocaml-utils--dune-history nil
  "The history list for dune watch builds.")

(defcustom ocaml-utils-lock-dune nil
  "Should dune watch take the lock?")

(defcustom ocaml-utils--debug nil
  "Are we debugging ocaml-utils?")

;;; DUNE WATCH

(defun ocaml-utils--generate-command (build-system watch alias lock command)
  "Generate a dune or make COMMAND depending on BUILD-SYSTEM.

- WATCH adds the `--watch` option
- ALIAS is a dune built-in alias
- LOCK make dune build lock the build to prevent other dune build to run"
  (let ((watch (if watch "--watch" ""))
        (alias (if alias alias ""))
        (lock (if lock "" "DUNE_CONFIG__GLOBAL_LOCK=disabled")))
    (concat
     (pcase build-system
       (`make (format "BUILD_EXTRA='%s %s' %s %s" watch alias lock command))
       (`dune (format "%s opam exec -- %s %s %s" lock command watch alias))))))

(defconst ocaml-utils--dune-aliases '("@check" "@ocaml-index" "@runtest" "@fmt" "@lint") "Dune built-in aliases.")

;;;###autoload (autoload 'ocaml-utils-compile "ocaml-utils-compile" nil t)
(transient-define-prefix ocaml-utils-compile ()
  "OCaml compilation utilities."
  :info-manual "(ocaml-utils)Build with dune"
  :man-page "dune build"
  :value '("--watch" "--diagnostics")
  [["Dune build Arguments"
    ("w" "Watch" "--watch")
    ("l" "Lock" "--lock")
    (ocaml-utils--alias :description "Choose an alias")]

   ["OCaml utils"
    ("D" "Enable lsp diagnostics" "--diagnostics")]]

  ["Commands"
   ("m" "make" ocaml-utils--build-with-make)
   ""
   ("d" "dune" ocaml-utils--build-with-dune)]
  (interactive)
  (transient-setup 'ocaml-utils-compile))

(defvar ocaml-utils--build-system nil "Chosen build-system.")
(defvar ocaml-utils--args nil "Build arguments.")
(defvar ocaml-utils--command nil "Registered command.")

;;;###autoload
(defun ocaml-utils-build-with (command &optional args)
  "Build COMMAND ARGS according to a chosen build-system."
  (interactive
   (let* ((project-root (projectile-project-root))
          (default-directory (projectile-compilation-dir)))
     (list (projectile-read-command
            "Compile command: "
            (if (eq ocaml-utils--build-system 'dune) "dune " "make "))
           (transient-args 'ocaml-utils-compile))))
  (setq ocaml-utils--args args)
  (let* ((watch (member "--watch" args))
         (lock (member "--lock" args))
         (diagnostics (member "--diagnostics" args))
         (alias (transient-arg-value "--alias=" args)))
    (setq ocaml-utils--command (ocaml-utils--generate-command ocaml-utils--build-system watch alias lock command))
    (message "starting process to watch %s task..." ocaml-utils--command)
    (let ((display-buffer-alist
           (add-to-list 'display-buffer-alist
                        (cons shell-command-buffer-name-async
                              (cons #'display-buffer-no-window nil)))))
      (when (or (not ocaml-utils--dune-watch-process)
                (and ocaml-utils--dune-watch-process
                     (y-or-n-p "A dune process is already running, kill it?")))
        (ignore-errors (quit-process ocaml-utils--dune-watch-process))
        (while (get-buffer-process ocaml-utils--dune-watch-process)
          (sleep-for 0.1))
        (setq ocaml-utils--dune-watch-process nil)
        (projectile-run-async-shell-command-in-root ocaml-utils--command)
        (setq ocaml-utils--dune-watch-process (get-buffer-process (get-buffer shell-command-buffer-name-async))))
      (when diagnostics
        (add-hook 'lsp-diagnostics-updated-hook 'ocaml-utils--report-diagnostics nil t)))))

;;;###autoload
(defun ocaml-utils-kill-dune-build ()
  "Kill the currently running dune process."
  (interactive)
  (when ocaml-utils--dune-watch-process
    (kill-process ocaml-utils--dune-watch-process)))

(defun ocaml-utils-restart-dune-build ()
  "Restart the currently running dune process."
  (interactive)
  (funcall-interactively #'ocaml-utils-build-with ocaml-utils--command ocaml-utils--args))

(defun ocaml-utils--build-with-dune ()
  "Build with dune."
  (interactive)
  (setq ocaml-utils--build-system `dune)
  (call-interactively #'ocaml-utils-build-with))

(defun ocaml-utils--build-with-make ()
  "Build with make."
  (interactive)
  (setq ocaml-utils--build-system `make)
  (call-interactively #'ocaml-utils-build-with))

(defun ocaml-utils--transient-read-alias (prompt initial-input history)
  "Reader for dune built-in aliases.

  See `completing-read' for PROMPT, INITIAL-INPUT and HISTORY arguments."
  (completing-read prompt ocaml-utils--dune-aliases nil nil initial-input history))

(transient-define-argument ocaml-utils--alias ()
  :description "Dune built-in alias"
  :argument "--alias="
  :class 'transient-option
  :key "A"
  :reader #'ocaml-utils--transient-read-alias)

(defcustom ocaml-utils--dune-watch-buffer "*dune watch*"
  "Buffer for dune watch results.")

(defvar-local ocaml-utils--dune-watch-process nil
  "Currently running dune watch process.")

(defun ocaml-utils--display-dune-watch-buffer (buffer)
  "Display BUFFER in a new dedicated window.

  If DISPLAY-BUFFER-ALIST contains a rule for BUFFER, displays it in this window instead"
  (if (display-buffer-assq-regexp buffer display-buffer-alist nil)
      (display-buffer buffer)
    (display-buffer-at-bottom
     buffer
     '((window-height . 0.2)))))

(defun ocaml-utils--report-diagnostics ()
  "Report newly received diagnostics in a dune-watch buffer that behaves like the compilation buffer."
  (let* ((diagnostics (lsp-diagnostics t))
         (formatted-diagnostics (ocaml-utils--format-diagnostics diagnostics))
         (buffer (get-buffer-create ocaml-utils--dune-watch-buffer))
         (window (if-let* ((window (get-buffer-window buffer)))
                     window
                   (ocaml-utils--display-dune-watch-buffer buffer))))
    (when ocaml-utils--debug (message "received %S\n-------------" diagnostics))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert ocaml-utils--command ": \n\n")
        (dolist (diagnostic (-flatten formatted-diagnostics))
          (insert diagnostic))
        (compilation-mode)
        (setq next-error-last-buffer buffer)
        (ocaml-utils--bold-quoted-substrings-in-buffer)
        (goto-char (point-min))))))

(defun ocaml-utils--bold-quoted-substrings-in-buffer ()
  "Make substrings in double quotes bold in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (text (match-string 1)))
          ;; Replace the whole match with the inner text (no quotes)
          (replace-match text t t)
          ;; Apply bold face to the newly inserted text
          (put-text-property start (- end 2) 'font-lock-face 'bold))))))

(defun ocaml-utils--make-relative-file (file)
  "Make FILE path relative to the project root dir."
  (car (projectile-make-relative-to-root (list file))))

(defun ocaml-utils--make-relative-uri (uri)
  "Transform URI in a path relative to the project root dir."
  (ocaml-utils--make-relative-file (lsp--uri-to-path uri)))

(defun ocaml-utils--format-lines (line-start line-end &optional increment)
  "Pretty-print LINE-START and LINE-END as line(s).

If INCREMENT is t, increments the line by 1."
  (let ((line-start (if increment (1+ line-start) line-start))
        (line-end (if increment (1+ line-end) line-end)))
    (if (eq line-start line-end)
        (format "line %d" line-start)
      (format "lines %d-%d" line-start line-end))))

(defun ocaml-utils--pretty-print-with-vertical-box (msg &optional indent)
  "Pretty-print MSG with each line being indented by INDENT."
  (let ((indent (or indent 4))
        (prefix (make-string (or indent 4) ?\s))
        (lines (split-string msg "\n")))
    (concat (car lines)
            "\n"
            (mapconcat (lambda (line) (concat prefix line)) (cdr lines) "\n"))))

(defun ocaml-utils--format-related-info (related-info)
  "Pretty-print RELATED-INFO which are information linked to a diagnostic error."
  (-let* (((&DiagnosticRelatedInformation?
            :message msg
            :location
            (&Location
             :uri
             :range
             (&Range
              :start (&Position :line line-start :character char-start)
              :end (&Position :line line-end :character char-end)))) related-info)
          (file (ocaml-utils--make-relative-uri uri)))
    (format "    File \"%s\", %s, characters %d-%d:%s"
            file (ocaml-utils--format-lines line-start line-end) char-start char-end (ocaml-utils--pretty-print-with-vertical-box (concat "\n" msg) 6))))

(defun ocaml-utils--export-snippet (file line-start line-end)
  "Display the code snippet from FILE between LINE-START and LINE-END."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((line-start (1+ line-start))
           (line-end (1+ line-end))
           (char-start (line-beginning-position line-start))
           (char-end (line-end-position line-end))
           (content (buffer-substring-no-properties char-start char-end))
           (lines (split-string content "\n"))
           (result ""))
      (cl-loop for line in lines
               for num from line-start
               do (setq result (concat result (format "%2d | %s\n" num line))))
      result)))

(defun ocaml-utils--horizontal-line ()
  "Insert an horizontal line at the current point."
  (make-string 80 ?-))

(defun ocaml-utils--format-ocaml-diagnostic (file message)
  "Pretty-print a MESSAGE associated to a FILE."
  (-let* (((&Diagnostic
            :message main-msg
            :severity?
            :source?
            :range (&Range
                    :start (&Position :line line-start :character char-start)
                    :end (&Position :line line-end :character char-end))
            :related-information?) message))
    (if (and source? (string-equal source? "ocamllsp"))
        (let ((severity-string (if severity?
                                   (if (= severity? 1) "Error: " "Warning: ")
                                 ""))
              (snippet (ocaml-utils--export-snippet file line-start line-end))
              (related-infos (if related-information?
                                 (mapconcat #'ocaml-utils--format-related-info related-information? "\n")
                               "")))
          (format "File \"%s\", %s, characters %d-%d:\n%s\n%s: %s\n%s\n\n%s\n\n"
                  (ocaml-utils--make-relative-file file) (ocaml-utils--format-lines line-start line-end t) char-start char-end
                  snippet severity-string (ocaml-utils--pretty-print-with-vertical-box main-msg) related-infos (ocaml-utils--horizontal-line)))
      nil)))

(defun ocaml-utils--format-per-file-diagnostic (file messages)
  "Pretty-print MESSAGES associated to a FILE.

  Calls `ocaml-utils--format-ocaml-diagnostic'."
  (let ((formatted-messages
         (-map (apply-partially #'ocaml-utils--format-ocaml-diagnostic file) messages)))
    (-non-nil formatted-messages)))

(defun ocaml-utils--format-diagnostics (diagnostics)
  "Pretty-print dune watch DIAGNOSTICS to be suitable for the Emacs compilation buffer.

  Calls `ocaml-utils--format-per-file-diagnostic'."
  (ht-map #'ocaml-utils--format-per-file-diagnostic diagnostics))

(defun ocaml-utils--transform-constructor (value)
  "Transform VALUE, an OCaml constructor in an elisp cons cell.

  If the constructor is a constant, returns a `cons' with its `car' set to it
  and its `cdr' set to nil.
  Otherwise, the `cdr' is the parameters attached to the constructor."
  (let* ((lvalue (string-split value "of"))
         (left (string-trim (car lvalue)))
         (right (nth 1 lvalue)))
    (if right
        (cons left (string-trim right))
      (cons left nil))))

(defun ocaml-utils--pp-variant (value)
  "For each constructors in VALUE (a type declaration), create a pattern matching branch.

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
