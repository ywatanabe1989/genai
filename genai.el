;;; -*- lexical-binding: t -*-
;;; Author: ywatanabe
;;; Time-stamp: <2024-11-14 22:58:11 (ywatanabe)>
;;; File: ./genai/genai.el


;; Copyright (C) 2024 Yusuke Watanabe

;; Author: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Created: 7 Jul 2024
;; URL: https://github.com/ywatanabe1989/genai
;; Version: 1.0
;; License: GNU General Public License (GPL) version 3
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.6") (pulse "1.0"))

;; Keywords: tools, LLM, template

;;; Commentary:
;;
;; `genai.el` provides an interface to interact with large language models
;; (LLMs) such as ChatGPT, Gemini, Claude, Llama.

;; Features:
;;
;; - LLM queries are constructed from the region or manual typing
;;
;; - Templates for prompts are customizable
;;
;; - LLM outputs are displayed in a streaming manner
;;
;; - Markup is enabled using markdown mode
;;
;; - Code blocks provided by LLM can be navigated and automatically
;;   copied to the kill ring
;;
;; - Conversation history as both human- and AI-readable files is stored,
;;   independently from chat history applied to LLMs

;; Usage:
;;
;; - M-x genai-on-region:
;;       Run GenAI on selected text. If no region is selected,
;;       a manual prompt will be requested.
;;
;; - M-x genai-show-history :
;;       Display conversation history
;;
;; - M-n (on "GenAI" buffer):
;;       Navigate to the "next" code block and copy the content to the kill ring
;;
;; - M-p (on "GenAI" buffer):
;;       Navigate to the "previous" code block and copy the content to the kill ring

;;; Sample configuration:
;;
;; (require 'genai)
;;
;; ;; Model (OpenAI, Gemini, Claude, Perplexity, or Llama models)
;; (setq genai-api-key (getenv "GENAI_API_KEY")) ; OPENAI_API_KEY
;; (setq genai-engine (getenv "GENAI_ENGINE")) ; "gpt-4o"
;;
;; ;; Model Parameters
;; (setq genai-n-history "5")
;; (setq genai-max-tokens "2000")
;; (setq genai-temperature "0")
;;
;; ;; PATH
;; (setq genai-home-dir (getenv "EMACS_GENAI_DIR"))
;; (setq genai-python-bin-path
;;     (concat (getenv "HOME") "/proj/env-3.11/bin/python3"))
;; (setq genai-python-script-path
;;     (concat (getenv "EMACS_GENAI_DIR") "genai.py"))
;; (setq genai-human-history-path
;;     (concat (getenv "EMACS_GENAI_DIR") "history-human-secret.json"))
;;
;; ;; Templates:
;;
;; Templates are markdown files (*.md) stored in './templates' directory
;; (default: $HOME/.emacs.d/lisp/emacs-genai/templates/).
;;
;; Template Selection:
;; 1. Default: First uppercase letter of filename is used as shortcut
;; 2. Custom: Define your own shortcuts in genai-template-mapping:
;;    (setq genai-template-mapping
;;          '(("p" . "Program")                ; p -> Program.md
;;            ("e" . "Email")                  ; s -> SciWrite.md
;;            ("c" . "Correct")                ; c -> Correct.md
;;            ("my" . "MyAwesomeTemplate")))   ; my -> MyAwesometemplate.md
;;
;; Note: Templates should contain "PLACEHOLDER" where your input will be inserted.
;;
;; ;; Key Bindings
;; (define-key global-map (kbd "C-M-g") 'genai-on-region)



;;; Code:
(require 'markdown-mode)
(require 'cl-lib)
(require 'async)
(require 'ansi-color)

(defvar genai-dependencies-checked nil
  "Whether Python dependencies have been checked.")

(defvar genai-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `genai-mode'.")

(add-to-list 'auto-mode-alist '("\\.genai\\'" . genai-mode))

(defgroup genai nil
  "Customization group for GenAI."
  :group 'applications
  :prefix "genai-")

(defvar genai--process nil
  "Process object for the GenAI process.")

(define-derived-mode genai-mode markdown-mode "GenAI"
  "Major mode for GenAI interactions.")

(defcustom genai-home-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The home directory of genai.el."
  :type 'string)

(defcustom genai-python-bin-path "/usr/bin/python3"
  "Path to the Python binary used by genai.el."
  :type 'string)

(defcustom genai-python-script-path (concat genai-home-dir "genai.py")
  "Path to the Python script used by genai.el."
  :type 'string)

(defcustom genai-python-script-path-show-history (concat genai-home-dir "show-history.py")
  "Path to the Python script used by genai.el."
  :type 'string)

(defcustom genai-templates-dir (concat genai-home-dir "templates/")
  "The path to the Python script used by genai.el."
  :type 'string)

(defcustom genai-history-human-path (concat genai-home-dir "history-human-secret.json")
  "Path to the history file used by genai.el."
  :type 'string)

(defcustom genai-history-ai-path (replace-regexp-in-string "human" "ai" genai-history-human-path)
  "Path to the history file used by genai.el."
  :type 'string)

(defcustom genai-api-keys ""
  "A string containing GenAI API keys, separated by commas."
  :type 'string
  :group 'genai)

(defcustom genai-api-keys-parsed nil
  "List of GenAI API keys parsed from `genai-api-keys`."
  :type '(repeat string)
  :group 'genai)

(defcustom genai-engine (getenv "GENAI_ENGINE")
  "The LLM engine to use, such as ChatGPT, Claude, Gemini."
  :type 'string
  :group 'genai)

(defcustom genai-max-tokens "2000"
  "Maximum number of tokens used with the engine."
  :type 'string)

(defcustom genai-n-history "5"
  "Number of history entries to keep."
  :type 'string)

(defcustom genai-temperature "0"
  "Temperature setting used with the engine."
  :type 'string)

(defvar genai--progress-reporter nil
  "Progress reporter for GenAI process.")

(defconst genai--splitter
  "--------------------------------------------------------------------------------"
  "Splitter for GenAI outputs in buffer.")

(defconst genai--splitter-human-readable
  "============================================================"
  "Splitter for human-readable history file.")

(defconst genai--code-block-start-delimiter
  "^\\s-*```\\s-*\\([a-zA-Z0-9_+-]+\\)$"
  "Regex for code block start delimiter.")

(defconst genai--code-block-end-delimiter
  "^\\s-*```\\s-*$"
  "Regex for code block end delimiter.")

(defvar genai--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar genai--spinner-timer nil
  "Timer for the spinner animation.")

(defvar genai--spinner-index 0
  "Current index in the spinner animation.")

(defvar genai--spinner-marker nil
  "Marker for spinner position.")

(defvar genai--fixed-spinner-position nil
  "Fixed position for the spinner.")

(defvar-local genai-input-marker nil
  "Marker for the input position in GenAI buffer.")


(defvar genai-template-mapping nil
  "Mapping between shortcuts and their corresponding readme files.
Example: Template shortcuts can be customized as:
  '((\"p\" . \"Program\")              ; p -> Program.md
    (\"e\" . \"Email\")                ; e -> Email.md
    (\"c\" . \"Correct\")              ; c -> Correct.md
    (\"my\" . \"MyAwesomeTemplate\"))  ; my -> MyAwesomeTemplate.md")


(defun genai-send-buffer-input ()
  "Send text from the last separator as input to GenAI."
  (interactive)
  (with-current-buffer "*GenAI*"
    (let* ((input-start (save-excursion
                         (goto-char (point-max))
                         (or (re-search-backward genai--splitter nil t)
                             (point-min))))
           (input-text (buffer-substring-no-properties
                       (save-excursion
                         (goto-char input-start)
                         (forward-line 2)
                         (point))
                       (point-max))))
      (when (not (string-empty-p (string-trim input-text)))
        (genai--insert-prompt-template-type-and-engine input-text "chat")
        (genai--run input-text)))))

;; Functions
(defun genai--init ()
  "Initialize the GenAI package and check Python dependencies."
  (unless genai-dependencies-checked
    (genai--check-python-dependencies)
    (setq genai-dependencies-checked t)))

(cl-defun genai--ensure-dependencies ()
  "Ensure Python dependencies are checked
before running any GenAI functionality."
  (unless genai-dependencies-checked
    (genai--check-python-dependencies)
    (setq genai-dependencies-checked t)))

(cl-defun genai--check-python-dependencies ()
  "Check if python mngs package is installed."
  (interactive)
  (let ((check-command (format "%s -c 'import pkg_resources; pkg_resources.require(\"mngs>=1.5.5\")'"
                               genai-python-bin-path)))
    (async-start
     `(lambda () (shell-command-to-string ,check-command))
     (lambda (result)
       (if (string-match "DistributionNotFound\\|VersionConflict" result)
           (when (yes-or-no-p (format "The required Python package 'mngs>=1.5.5' is not installed or outdated for %s. Install/upgrade it now?" genai-python-bin-path))
             (async-start
              `(lambda () (shell-command-to-string ,(format "%s -m pip install 'mngs>=1.5.5'" genai-python-bin-path)))
              (lambda (_) (message "Package installed/upgraded."))))
         )))))

(cl-defun genai--create-buffer-file (buffer)
  "Create a temporary file with the text in BUFFER and return its file name.
The file will have a .genai extension.
BUFFER can be a buffer object or a buffer name."
  (let ((file (make-temp-file "genai-prompt-" nil ".md")))
    (condition-case err
        (progn

          (with-temp-file file
            (with-current-buffer (get-buffer buffer)
              (insert-buffer-substring buffer)))

          (let ((temp-buffer (find-file-noselect file)))
            (with-current-buffer temp-buffer
              (text-mode)))
          file)

      (error
       (when file
         (delete-file file))
       (message "Error creating buffer file: %s" (error-message-string err))
       nil))))

(cl-defun --genai-find-first-capital (string)
  "Find first capital letter in STRING and return cons of (letter . position).
Example: (--genai-find-first-capital \"parapHrase.md\") => (h . 5)"
  (interactive)
  (let* ((name (file-name-sans-extension string))
         (case-fold-search nil)
         (capital-pos (string-match "[A-Z]" name)))
    (when capital-pos
      (cons (downcase (substring name capital-pos (1+ capital-pos)))
            capital-pos))))
;; (--genai-find-first-capital "parapHrase.md") ; => (h . 5)

;; (cl-defun genai--fetch-templates (dir)
;;   "Return list of formatted template names from DIR that contain capital letters.
;; For template starting with capital (e.g., Program.md) returns just the name.
;; For template with internal capital (e.g., parapHrase.md) returns 'h parapHrase'.
;; Example: (genai--fetch-templates \"templates/\") => (\"t prinT\" \"h parapHrase\" \"Visa\" \"SciWrite\" \"Remember\" \"Program\" \"Email\" \"Correct\" \"Alternative\")"
;;   (interactive)
;;   (when (file-exists-p dir)
;;     (let ((files (directory-files dir nil ".*[A-Z].*\\.md$")))
;;       (sort (mapcar (lambda (f)
;;                       (let* ((name-without-ext (substring f 0 (string-match "\\.md" f)))
;;                              (capital-info (--genai-find-first-capital f))
;;                              (first-capital (car capital-info))
;;                              (capital-pos (cdr capital-info)))
;;                         (cond
;;                          ((= capital-pos 0)
;;                           (format "%s" name-without-ext))
;;                          (capital-pos
;;                           (format "%s %s" first-capital name-without-ext))
;;                          (t nil))))
;;                     files)
;;             'string>))))



;; (cl-defun genai--fetch-templates (dir)
;;   "Return list of formatted template names from DIR that contain capital letters.
;; For template starting with capital (e.g., Program.md) returns just the name.
;; For template with internal capital (e.g., parapHrase.md) returns 'h parapHrase'.
;; Example: (genai--fetch-templates \"templates/\") => (\"t prinT\" \"h parapHrase\" \"Visa\" \"SciWrite\" \"Remember\" \"Program\" \"Email\" \"Correct\" \"Alternative\")"
;;   (interactive)
;;   (when (file-exists-p dir)
;;     (let ((files (sort (directory-files dir nil ".*[A-Z].*\\.md$")
;;                       (lambda (a b)
;;                         (if (/= (length a) (length b))
;;                             (< (length a) (length b))
;;                           (string< a b))))))
;;       (mapcar (lambda (f)
;;                 (let* ((name-without-ext (substring f 0 (string-match "\\.md" f)))
;;                        (capital-info (--genai-find-first-capital f))
;;                        (first-capital (car capital-info))
;;                        (capital-pos (cdr capital-info)))
;;                   (cond
;;                    ((= capital-pos 0)
;;                     (format "%s" name-without-ext))
;;                    (capital-pos
;;                     (format "%s %s" first-capital name-without-ext))
;;                    (t nil))))
;;               files))))

(cl-defun genai--fetch-templates (dir)
  "Return list of formatted template names from DIR that contain capital letters.
For template starting with capital (e.g., Program.md) returns just the name.
For template with internal capital (e.g., parapHrase.md) returns 'h parapHrase'.
Example: (genai--fetch-templates \"templates/\") => (\"t prinT\" \"h parapHrase\" \"Visa\" \"SciWrite\" \"Remember\" \"Program\" \"Email\" \"Correct\" \"Alternative\")"
  (interactive)
  (when (file-exists-p dir)
    (sort
     (delq nil
           (mapcar (lambda (f)
                    (let* ((name-without-ext (substring f 0 (string-match "\\.md" f)))
                           (capital-info (--genai-find-first-capital f))
                           (first-capital (car capital-info))
                           (capital-pos (cdr capital-info)))
                      (cond
                       ((= capital-pos 0)
                        (format "%s" name-without-ext))
                       (capital-pos
                        (format "%s %s" first-capital name-without-ext))
                       (t nil))))
                  (directory-files dir nil ".*[A-Z].*\\.md$")))
     #'string<)))
;; (genai--fetch-templates "/home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/templates")

;; (cl-defun genai--create-shortcuts (templates)
;;   "Generate shortcuts for templates using numbers for duplicates (e.g., s1, s2)."
;;   (let ((shortcuts (make-hash-table :test 'equal))
;;         (counts (make-hash-table :test 'equal)))

;;     (message "Initial templates: %S" templates)

;;     ;; First, apply predefined mappings
;;     (when (boundp 'genai-template-mapping)
;;       (message "Using predefined mappings: %S" genai-template-mapping)
;;       (dolist (mapping genai-template-mapping)
;;         (let* ((key (car mapping))
;;                (template-name (cdr mapping)))
;;           (message "Processing mapping: %s -> %s" key template-name)
;;           (when-let ((template (cl-find template-name templates
;;                                       :key #'car
;;                                       :test #'string=)))
;;             (puthash key (car template) shortcuts)
;;             (puthash (substring key 0 1) 1 counts)))))

;;     ;; Then handle remaining templates
;;     (setq templates (sort templates (lambda (a b) (string< (car a) (car b)))))
;;     (message "Sorted templates: %S" templates)

;;     (dolist (template templates)
;;       (let* ((name (car template))
;;              (suffix (cdr template))
;;              (last-capital (if suffix
;;                              (downcase suffix)
;;                            (if (string-match "[A-Z]" name)
;;                                (downcase (substring (match-string 0 name) 0 1))
;;                              (downcase (substring name 0 1)))))
;;              (count (gethash last-capital counts 0))
;;              (new-key (if (= count 0)
;;                          last-capital
;;                        (format "%s%d" last-capital count))))

;;         (message "Processing template: %s (capital: %s, count: %d, key: %s)"
;;                 name last-capital count new-key)

;;         (unless (gethash new-key shortcuts)
;;           (puthash new-key name shortcuts)
;;           (puthash last-capital (1+ count) counts))))

;;     (message "Final shortcuts: %S" shortcuts)
;;     shortcuts))

;; ;; working but completion is split into shortcuts and tempalte name separately
;; (cl-defun genai--create-shortcuts (templates)
;;   "Generate shortcuts for templates using numbers for duplicates (e.g., s1, s2)."
;;   (let ((shortcuts (make-hash-table :test 'equal))
;;         (counts (make-hash-table :test 'equal))
;;         (completion-alist nil))

;;     (message "Initial templates: %S" templates)

;;     ;; First, apply predefined mappings
;;     (when (boundp 'genai-template-mapping)
;;       (message "Using predefined mappings: %S" genai-template-mapping)
;;       (dolist (mapping genai-template-mapping)
;;         (let* ((key (car mapping))
;;                (template-name (cdr mapping)))
;;           (message "Processing mapping: %s -> %s" key template-name)
;;           (when-let ((template (cl-find template-name templates
;;                                       :key #'car
;;                                       :test #'string=)))
;;             (puthash key (car template) shortcuts)
;;             (push (cons key (car template)) completion-alist)
;;             (puthash (substring key 0 1) 1 counts)))))

;;     ;; Then handle remaining templates
;;     (setq templates (sort templates (lambda (a b) (string< (car a) (car b)))))
;;     (message "Sorted templates: %S" templates)

;;     (dolist (template templates)
;;       (let* ((name (car template))
;;              (suffix (cdr template))
;;              (last-capital (if suffix
;;                              (downcase suffix)
;;                            (if (string-match "[A-Z]" name)
;;                                (downcase (substring (match-string 0 name) 0 1))
;;                              (downcase (substring name 0 1)))))
;;              (count (gethash last-capital counts 0))
;;              (new-key (if (= count 0)
;;                          last-capital
;;                        (format "%s%d" last-capital count))))

;;         (message "Processing template: %s (capital: %s, count: %d, key: %s)"
;;                 name last-capital count new-key)

;;         (unless (gethash new-key shortcuts)
;;           (puthash new-key name shortcuts)
;;           (push (cons new-key name) completion-alist)
;;           (push (cons name name) completion-alist)
;;           (puthash last-capital (1+ count) counts))))

;;     (message "Final shortcuts: %S" shortcuts)
;;     (set-default 'genai--completion-alist completion-alist)
;;     (setq genai--completion-function
;;           (lambda (string pred action)
;;             (if (eq action 'metadata)
;;                 '(metadata (category . genai))
;;               (complete-with-action action genai--completion-alist string pred))))
;;     shortcuts))


;; () formatting
(cl-defun genai--create-shortcuts (templates)
  "Generate shortcuts for templates using numbers for duplicates."
  (let ((shortcuts (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal))
        (completion-alist nil))

    ;; Apply predefined mappings first
    (when (boundp 'genai-template-mapping)
      (dolist (mapping genai-template-mapping)
        (let* ((key (car mapping))
               (template-name (cdr mapping)))
          (when-let ((template (cl-find template-name templates
                                      :key #'car
                                      :test #'string=)))
            (puthash key (car template) shortcuts)
            ;; Add combined format for completion
            (push (cons (format "%s - %s" key (car template)) (car template)) completion-alist)
            (puthash (substring key 0 1) 1 counts)))))

    ;; Handle remaining templates
    (setq templates (sort templates (lambda (a b) (string< (car a) (car b)))))

    (dolist (template templates)
      (let* ((name (car template))
             (suffix (cdr template))
             (last-capital (if suffix
                             (downcase suffix)
                           (if (string-match "[A-Z]" name)
                               (downcase (substring (match-string 0 name) 0 1))
                             (downcase (substring name 0 1)))))
             (count (gethash last-capital counts 0))
             (new-key (if (= count 0)
                         last-capital
                       (format "%s%d" last-capital count))))

        (unless (gethash new-key shortcuts)
          (puthash new-key name shortcuts)
          ;; Add combined format for completion
          (push (cons (format "%s - %s" new-key name) name) completion-alist)
          (puthash last-capital (1+ count) counts))))

    (set-default 'genai--completion-alist completion-alist)
    (setq genai--completion-function
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (category . genai))
              (complete-with-action action genai--completion-alist string pred))))
    shortcuts))

;; (cl-defun genai--select-template ()
;;   "Prompt the user to select a template type for the GenAI model."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let* ((capital-templates (genai--fetch-templates genai-templates-dir))
;;            (templates-with-shortcuts
;;             (mapcar (lambda (template)
;;                      (if (string-match "^\\([a-z]\\) \\(.+\\)$" template)
;;                          (cons (match-string 2 template) (match-string 1 template))
;;                        (cons template (downcase (substring template 0 1)))))
;;                    capital-templates))
;;            (shortcuts (genai--create-shortcuts templates-with-shortcuts))
;;            (prompt-parts nil))

;;       ;; Build prompt
;;       (maphash (lambda (key value)
;;                  (push (format "(%s) %s" key value) prompt-parts))
;;                shortcuts)

;;       ;; Sort prompt-parts alphabetically by the template name
;;       (setq prompt-parts (sort prompt-parts
;;                               (lambda (a b)
;;                                 (string< (cadr (split-string a ")" t))
;;                                        (cadr (split-string b ")" t))))))

;;       (let* ((prompt (concat "Select template or Enter additional message: \n"
;;                             (mapconcat 'identity prompt-parts "  ")
;;                             "\n"))
;;              (input (read-string prompt))
;;              (template-type (or (gethash input shortcuts)
;;                               (if (string-blank-p input) "None" input))))

;;         (unless (string= input "r")
;;           (display-buffer (get-buffer-create "*GenAI*")))

;;         template-type))))


;; ;; working but no completion available
;; (cl-defun genai--select-template ()
;;   "Prompt the user to select a template type for the GenAI model."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let* ((capital-templates (genai--fetch-templates genai-templates-dir))
;;            (templates-with-shortcuts
;;             (mapcar (lambda (template)
;;                      (if (string-match "^\\([a-z][0-9]?\\) \\(.+\\)$" template)
;;                          (cons (match-string 2 template) (match-string 1 template))
;;                        (cons template (downcase (substring template 0 1)))))
;;                    capital-templates))
;;            (shortcuts (genai--create-shortcuts templates-with-shortcuts))
;;            (prompt-parts nil)
;;            (completion-list (list "")))

;;       (maphash (lambda (key value)
;;                  (push (format "(%s) %s" key value) prompt-parts)
;;                  (push value completion-list))
;;                shortcuts)

;;       (setq prompt-parts (sort prompt-parts
;;                               (lambda (a b)
;;                                 (string< (downcase (cadr (split-string a ")" t)))
;;                                        (downcase (cadr (split-string b ")" t)))))))

;;       (let* ((prompt (concat "Select template or Enter additional message: \n"
;;                             (mapconcat 'identity prompt-parts "  ")
;;                             "\n"))
;;              (input (read-string prompt))
;;              (template-type (or (gethash input shortcuts)
;;                               (if (string-blank-p input) "None" input))))

;;         (unless (string= input "r")
;;           (display-buffer (get-buffer-create "*GenAI*")))

;;         template-type))))

(cl-defun genai--select-template ()
  "Prompt the user to select a template type for the GenAI model."
  (interactive)
  (unless (minibufferp)
    (let* ((capital-templates (genai--fetch-templates genai-templates-dir))
           (templates-with-shortcuts
            (mapcar (lambda (template)
                     (if (string-match "^\\([a-z][0-9]?\\) \\(.+\\)$" template)
                         (cons (match-string 2 template) (match-string 1 template))
                       (cons template (downcase (substring template 0 1)))))
                   capital-templates))
           (key-count (make-hash-table :test 'equal))
           (shortcuts (make-hash-table :test 'equal))
           (prompt-parts nil))

      (dolist (pair templates-with-shortcuts)
        (let* ((value (car pair))
               (key (cdr pair))
               (count (gethash key key-count 0))
               (numbered-key (if (> count 0)
                               (format "%s%d" key (1+ count))
                             key)))
          (puthash key (1+ count) key-count)
          (puthash numbered-key value shortcuts)
          (push (format "(%s) %s" numbered-key value) prompt-parts)))

      (setq prompt-parts (sort prompt-parts 'string<))

      (let* ((prompt (concat "Template or Manual Instruction:\n"
      (mapconcat 'identity prompt-parts " ")
                            "\n"))
             (input (read-string prompt))
             (template-type (or (gethash input shortcuts)
                              (if (string-blank-p input) "None" input))))

        (unless (string= input "r")
          (display-buffer (get-buffer-create "*GenAI*")))

        template-type))))
;; (genai--select-template)


(cl-defun genai--safe-shell-quote-argument (arg)
  "Safely shell-quote ARG if non-nil and non-empty, else return an empty string."
  (if (and arg (not (string-empty-p arg)))
      (shell-quote-argument arg)
    ""))

(cl-defun genai--insert-prompt-template-type-and-engine (prompt template-type)
  "Insert prompt, template type, and engine into *GenAI* buffer.
PROMPT is the user's input, TEMPLATE-TYPE is the selected template."
  (with-current-buffer (get-buffer-create "*GenAI*")
    (goto-char (point-max))
    (insert "\n\n")
    (insert genai--splitter)
    (insert "\n\n")
    (insert "| ")
    (insert "YOU")
    (insert "\n\n")
    (insert "| ")
    (insert template-type)
    (insert "\n\n")
    (insert "| ")
    (insert prompt)
    (insert "\n\n")
    (insert genai--splitter)
    (insert "\n\n")
    (insert "| ")
    (insert (upcase genai-engine))
    (insert " ")
    (insert "\n\n")
    (goto-char (point-max))
    (run-at-time "0 sec" nil #'genai--scroll)))

(defun genai--parse-api-keys ()
  "Parse API keys from `genai-api-keys` and store them in `genai-api-keys-parsed`."
  (interactive)

  ;; Check if the API keys string is not empty
  (unless (string-empty-p genai-api-keys)
    (let ((keys-list (split-string genai-api-keys ":")))
      (setq genai-api-keys-parsed keys-list)
      ;; (message "Parsed API Keys: %S" genai-api-keys-parsed)
      )))

(defun genai--construct-python-command (prompt)
  "Construct complete command string for starting the GenAI Python process."
  (interactive "sEnter prompt: ")

  (genai--parse-api-keys)

  (let* ((template-type (genai--select-template))
         (prompt-arg (if (or (null prompt) (string-empty-p prompt))
                         "''"
                       (genai--safe-shell-quote-argument prompt)))

         (command (format "%s \
                           %s \
                           %s \
                           --engine %s \
                           --max_tokens %s \
                           --temperature %s \
                           --human_history_path %s \
                           --n_history %s \
                           --template_type %s \
                           --prompt %s"
                          (genai--safe-shell-quote-argument genai-python-bin-path)
                          (genai--safe-shell-quote-argument genai-python-script-path)
                          (mapconcat
                           (lambda (api-key)
                             (concat "--api_key " (genai--safe-shell-quote-argument api-key)))
                           genai-api-keys-parsed " ")
                          (genai--safe-shell-quote-argument genai-engine)
                          (genai--safe-shell-quote-argument genai-max-tokens)
                          (genai--safe-shell-quote-argument genai-temperature)
                          (genai--safe-shell-quote-argument genai-history-human-path)
                          (genai--safe-shell-quote-argument genai-n-history)
                          (genai--safe-shell-quote-argument template-type)
                          prompt-arg)))

    (genai--insert-prompt-template-type-and-engine prompt template-type)

    command))


;;;###autoload
(defun genai-show-history (&optional num-interactions)
  "Show the GenAI history in a temporary buffer. NUM-INTERACTIONS limits the number of interactions shown."
  (interactive "sEnter the number of latest interactions (default: 1024): ")
  (let* ((genai-all-history-buffer (get-buffer-create "*GenAI All History*"))
         (num-interactions (cond
                          ((null num-interactions) 1024)
                          ((numberp num-interactions) (number-to-string num-interactions))
                          ((stringp num-interactions) (if (string-empty-p num-interactions)
                                                        "1024"
                                                        num-interactions))
                          (t "1024"))))
    (with-current-buffer genai-all-history-buffer
      (erase-buffer))

    (display-buffer genai-all-history-buffer)

    (let ((command-list (list genai-python-bin-path
                             genai-python-script-path-show-history
                             "--human_history_path" genai-history-human-path
                             "--n_interactions" num-interactions)))
      (make-process
       :name "genai-show-history"
       :buffer genai-all-history-buffer
       :command command-list
       :sentinel (lambda (process event)
                   (when (string= event "finished\n")
                     (with-current-buffer genai-all-history-buffer
                       (goto-char (point-min))
                       (genai--scroll-history)
                       (markdown-mode))))))
    (message "Loading history to *GenAI All History* buffer...")))

;;;###autoload
(defun genai-reset-history ()
  "Backup and reset the GenAI history files."
  (interactive)
  (let* ((backup-dir (concat genai-home-dir "histories"))
         (timestamp (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (human-backup (concat backup-dir "/history-human-" timestamp ".json"))
         (ai-backup (concat backup-dir "/history-ai-" timestamp ".json")))

    ;; Create backup directory if it doesn't exist
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))

    ;; Backup existing history files
    (when (file-exists-p genai-history-human-path)
      (rename-file genai-history-human-path human-backup))
    (when (file-exists-p genai-history-ai-path)
      (rename-file genai-history-ai-path ai-backup))

    ;; Create new empty history files
    (with-temp-file genai-history-human-path
      (insert "[]"))
    (with-temp-file genai-history-ai-path
      (insert "[]"))

    (message "GenAI history has been reset. Backups created in %s" backup-dir)))

(defun genai--start-spinner ()
  "Start the spinner animation in the GenAI buffer."
  (when (and (get-buffer "*GenAI*")
             (not genai--spinner-timer))
    (with-current-buffer "*GenAI*"
      (goto-char (point-max))
      (search-backward (upcase genai-engine))
      (goto-char (line-end-position))
      (setq genai--fixed-spinner-position (point-marker))
      (let ((font-lock-mode nil))
        (setq genai--spinner-timer
              (run-with-timer 0 0.1
                             (lambda ()
                               (with-current-buffer "*GenAI*"
                                 (save-excursion
                                   (goto-char genai--fixed-spinner-position)
                                   (let ((inhibit-read-only t))
                                     (delete-region (point) (line-end-position))
                                     (insert (propertize
                                            (nth genai--spinner-index genai--spinner-frames)
                                            'face '(:foreground "blue")))
                                     (setq genai--spinner-index
                                           (mod (1+ genai--spinner-index)
                                                (length genai--spinner-frames)))))))))))))

(defun genai--stop-spinner ()
  "Stop the spinner animation."
  (when genai--spinner-timer
    (cancel-timer genai--spinner-timer)
    (setq genai--spinner-timer nil)
    (with-current-buffer "*GenAI*"
      (save-excursion
        (when genai--fixed-spinner-position
          (goto-char genai--fixed-spinner-position)
          (let ((inhibit-read-only t))
            (delete-region (point) (line-end-position))
            (delete-char -1)
            (goto-char (point-max))
            (insert "\n\n")
            (insert genai--splitter)
            (insert "\n\n")
            (goto-char (point-max))
            ))
        (setq genai--fixed-spinner-position nil)))))

;; Update process sentinel
(defun genai--process-sentinel (_process msg)
  "Custom sentinel for the GenAI process."
  (cond
   ((string-match-p "finished\\|exited" msg)
    (progress-reporter-done genai--progress-reporter)
    (genai--stop-spinner)
    (message "GenAI process finished.")
    (genai--clean-up-all))
   ((string-match-p "error" msg)
    (progress-reporter-done genai--progress-reporter)
    (genai--stop-spinner)
    (message "GenAI process encountered an error: %s" msg))
   (t
    (message "GenAI process: %s" msg))))

(cl-defun genai--clean-up-all ()
  (interactive)
  (genai--clean-up-output)
  )

(cl-defun genai--update-progress ()
  "Update the progress reporter."
  (when genai--progress-reporter
    (progress-reporter-update genai--progress-reporter (random 100))
    (when (process-live-p genai--process)
      (run-with-timer 0.5 nil #'genai--update-progress))))

;; Update start-python-process
(cl-defun genai--start-python-process (prompt)
  "Start the GenAI process with the given PROMPT using a shell command."
  (interactive "sEnter prompt: ")
  (when (and genai--process (process-live-p genai--process))
    (interrupt-process genai--process)
    (delete-process genai--process))

  (let* ((command (genai--construct-python-command prompt))
         (process (start-process-shell-command "genai--process" "*GenAI*" command)))
    (setq genai--process process)
    (set-process-sentinel process #'genai--process-sentinel)
    (setq genai--progress-reporter (make-progress-reporter "GenAI Processing..." 0 100))
    (genai--update-progress)
    (genai--start-spinner)
    process))


(cl-defun genai--stop-python-process ()
  "Stop the GenAI process if it is running."
  (interactive)
  (when (and genai--process (process-live-p genai--process))
    (interrupt-process genai--process)
    (message "GenAI process interrupted.")))

(cl-defun genai--clean-up-output ()
  "Remove ANSI escape codes and specific unwanted messages from the *GenAI* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GenAI*")
    (ansi-color-apply-on-region (point-min) (point-max))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\x1b\\[\\([0-9;]*\\)m" nil t)
        (replace-match "")))))

(cl-defun genai--run (prompt)
  "Run GenAI command with prompt."
  (interactive)
  (if (equal prompt "g")
      (progn
        (switch-to-buffer-other-window "*GenAI*")
        (keyboard-quit)
        (message "Jump to the \"*GenAI*\" buffer"))

    (with-current-buffer (get-buffer-create "*GenAI*")
      ;; (goto-char (point-max))
      (font-lock-ensure)
      (message "GenAI: Running...")
      (genai--start-python-process prompt))))


;;;###autoload
(defun genai-on-region ()
  "Run GenAI command on selected region or prompt for input.
If a region is selected, use that text as the prompt.
Otherwise, prompt the user to enter a prompt.
The response will be displayed in the *GenAI* buffer."
  (interactive)
  (genai--init)
  (genai--ensure-dependencies)
  (let* ((region-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (read-string "Enter prompt: " "")))
         (buffer (get-buffer-create "*GenAI*")))

    ;; Prepare the buffer for output
    (with-current-buffer buffer
      (unless (eq major-mode 'genai-mode)
        (genai-mode)))

    ;; Here, disable text selection (region) if exist
    (when (use-region-p)
      (deactivate-mark))

    ;; Run the Gen AI
    (genai--run region-text)

    ;; Display the output to the *GenAI* buffer
    ;; (display-buffer buffer)
    ))


(cl-defun genai--scroll ()
  "Scrolls to the first genai--splitter from the end of the *GenAI* buffer."
  (interactive)
  (if-let ((genai-buffer (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char (point-max))
        ;; Search for the splitter and update found position
        (when (re-search-backward genai--splitter nil t)
          (beginning-of-line))
        (let ((pos (point)))
          (when-let ((window (get-buffer-window genai-buffer 0)))
            (set-window-point window pos)
            (with-selected-window window
              (recenter 0)))))))

(cl-defun genai--scroll-history ()
  "Scrolls to the first genai--splitter from the end of the *GenAI* buffer."
  (interactive)
  (if-let ((genai-buffer (get-buffer "*GenAI All History*")))
      (with-current-buffer genai-buffer
        (goto-char (point-max))
        ;; Search for the splitter and update found position
        (when (re-search-backward genai--splitter-human-readable nil t)
          (beginning-of-line))
        (let ((pos (point)))
          (when-let ((window (get-buffer-window genai-buffer 0)))
            (set-window-point window pos)
            (with-selected-window window
              (recenter 0)))))))

(cl-defun genai--insert-splitter-after-run ()
  "Insert splitter after retrieving the Gen AI output"
  (interactive)
  (if-let ((genai-buffer (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char (point-max))
        (insert genai--splitter)
        (insert "\n\n"))))

;;;###autoload
(defun genai-copy-last ()
  "Copy the last LLM output into kill ring"
  (interactive)
  (if-let ((genai-buffer (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char (point-max))
        (set-mark (point))
        (goto-char (point-min))
        (kill-ring-save (region-beginning) (region-end)))))

;;;###autoload
(defun genai-copy-code-blocks ()
  "Copy code blocks in the last LLM output into kill ring"
  (interactive)
  (with-current-buffer (get-buffer "*GenAI*")
    (goto-char (point-max))
    (let ((count nil)
          blocks start end)
      (when (search-backward genai--splitter nil t)
        (goto-char (match-end 0))
        (while (re-search-forward genai--code-block-start-delimiter nil t)
          (forward-line 1)
          (setq start (point))
          (if (re-search-forward genai--code-block-end-delimiter nil t)
              (progn
                (setq end (match-beginning 0))
                (push (buffer-substring-no-properties start end) blocks))))
        ;; Remove nreverse to keep the original order of discovery
        (dolist (block blocks)
          (kill-new block))
        (setq count (length blocks))
        (message "Code blocks copied (n = %d)" count)))))


;;;###autoload
(defun genai-next-code-block ()
  "Navigate to the next code block and select the content"
  (interactive)
  (with-current-buffer "*GenAI*"
    (end-of-line)
    (when (looking-at-p genai--code-block-end-delimiter)
      (forward-line)
      (beginning-of-line))
    (when (re-search-forward genai--code-block-start-delimiter nil t)
      (let ((start (point)))
        (when (re-search-forward genai--code-block-end-delimiter nil t)
          (let ((end (match-beginning 0)))
            (goto-char end)
            (set-mark (1+ start))
            (activate-mark)
            (kill-ring-save start end)
            (pulse-momentary-highlight-region (1+ start) end)
            (message "Copied."))))
      (deactivate-mark))))

;;;###autoload
(defun genai-previous-code-block ()
  "Navigate to the previous code block and select the content"
  (interactive)
  (with-current-buffer "*GenAI*"

    ;; Find the previous code block end delimiter
    (when (re-search-backward genai--code-block-end-delimiter nil t)
      (backward-char)
      (let ((end (point)))

        ;; Find the previous code block start delimiter
        (if (re-search-backward genai--code-block-start-delimiter nil t)
            (progn
              (forward-line 1)
              (set-mark end)
              (activate-mark)
              (kill-ring-save (point) end)
              (pulse-momentary-highlight-region (+ (point) 1) end)
              (message "Copied."))
          (deactivate-mark)
          )))))

(add-hook 'comint-output-filter-functions 'genai--clean-up-output)

;; Keybindings
(define-key genai-mode-map (kbd "M-n") 'genai-next-code-block)
(define-key genai-mode-map (kbd "M-p") 'genai-previous-code-block)
(define-key genai-mode-map (kbd "C-c C-c") 'genai-send-buffer-input)

(provide 'genai)

;;; genai.el ends here


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
