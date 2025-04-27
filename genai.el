;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 11:23:53>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Copyright (C) 2024 Yusuke Watanabe
;; Author: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Maintainer: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; Created: 7 Jul 2024
;; URL: https://github.com/ywatanabe1989/genai
;; Version: 1.3
;; License: GNU General Public License (GPL) version 3
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.6") (pulse "1.0") (async "1.9.4"))
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
;; - M-x genai-show-history | M-x genai-on-region -> g -> RET:
;;       Open the "*GenAI*" buffer
;;
;; - M-x genai-show-history | M-x genai-on-region -> h -> RET:
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

;; `#'load`-check on each file:
;; ```
;; Loading genai.el
;; genai.el:Error: Emacs 29.4:
;; (file-missing "Cannot open load file" "No such file or directory" "async")
;; ```

(defvar genai-dependencies-checked nil
  "Whether Python dependencies have been checked.")

(defvar genai-mode-map
  (let
      ((map
        (make-sparse-keymap)))
    map)
  "Keymap for `genai-mode'.")

(add-to-list 'auto-mode-alist
             '("\\.genai\\'" . genai-mode))

(defgroup genai nil
  "Customization group for GenAI."
  :group 'applications
  :prefix "genai-")

(defvar genai--process nil
  "Process object for the GenAI process.")

;; Interactive mode

(define-minor-mode genai-interactive-mode
  "Temporary mode for interacting with GenAI while typing."
  :lighter " GenAI"
  :keymap
  (let
      ((map
        (make-sparse-keymap)))
    map))

;; Add keyboard-quit advice

(defadvice keyboard-quit
    (before genai-disable-interactive-mode activate)
  "Disable genai-interactive-mode when C-g is pressed."
  (when genai-interactive-mode
    (genai-interactive-mode -1)))

(define-derived-mode genai-mode markdown-mode
  "GenAI"
  "Major mode for GenAI interactions.")

(defcustom genai-home-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "The home directory of genai.el."
  :type 'string)

(defcustom genai-python-bin-path "/usr/bin/python3"
  "Path to the Python binary used by genai.el."
  :type 'string)

(defcustom genai-python-script-path
  (concat genai-home-dir "genai.py")
  "Path to the Python script used by genai.el."
  :type 'string)

;; (defcustom genai-python-script-path-show-history
;;   (concat genai-home-dir "show-history.py")
;;   "Path to the Python script used by genai.el."
;;   :type 'string)

(defcustom genai-templates-dir
  (concat genai-home-dir "templates/")
  "The path to the Python script used by genai.el."
  :type 'string)

(defcustom genai-temp-prompt-path
  (concat genai-home-dir ".temp-prompt.txt")
  "Temporally text path to the Python script used by genai.el."
  :type 'string)

(defcustom genai-history-human-path
  (concat genai-home-dir "history-human-secret.json")
  "Path to the history file used by genai.el."
  :type 'string)

(defcustom genai-history-human-readable-path
  (concat genai-home-dir "history-human-readable-secret.md")
  "Path to the human-readable history file used by genai.el."
  :type 'string)

(defcustom genai-history-ai-path
  (replace-regexp-in-string "human" "ai" genai-history-human-path)
  "Path to the history file used by genai.el."
  :type 'string)

(defvar genai-llm-provider ""
  "Switcher for GenAI provider.")

(defcustom genai-api-keys ""
  "A string containing GenAI API keys, separated by commas."
  :type 'string
  :group 'genai)

(defcustom genai-api-keys-parsed nil
  "List of GenAI API keys parsed from `genai-api-keys`."
  :type
  '(repeat string)
  :group 'genai)

(defcustom genai-engine
  (getenv "GENAI_ENGINE")
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

(defvar genai--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
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
\\=((\"p\" . \"Program\")              ; p -> Program.md

(\"e\" . \"Email\")                ; e -> Email.md

(\"c\" . \"Correct\")              ; c -> Correct.md

(\"my\" . \"MyAwesomeTemplate\"))  ; my -> MyAwesomeTemplate.md")

(defcustom genai-buffer-max-lines (expt 2 14)
  "Maximum number of lines to keep in *GenAI* buffer."
  :type 'integer
  :group 'genai)

;;;###autoload

(defun genai-switch
    (provider)
  "Switch the GenAI provider and select a model."
  (interactive
   (list
    (completing-read "Select GenAI provider: "
                     '("anthropic" "google" "deepseek" "groq" "openai")
                     nil t)))
  (setq genai-llm-provider provider)
  (let*
      ((provider-models
        (cond
         ((string= provider "anthropic")
          '("claude-3-7-sonnet-20250219"
            "claude-3-5-sonnet-20241022"
            "claude-3-5-haiku-20241022"))
         ((string= provider "google")
          '("gemini-2.5-pro-exp-03-25"
            "gemini-2.0-flash-exp"
            "gemini-2.0-flash"
            "gemini-2.0-flash-lite-preview-02-05"
            "gemini-2.0-pro-exp-02-05"
            "gemini-2.0-flash-thinking-exp-01-21"
            ))
         ((string= provider "deepseek")
          '("deepseek-chat" "deepseek-coder" "deepseek-reasoner"))
         ((string= provider "groq")
          '("llama-3.3-70b-versatile" "deepseek-r1-distill-llama-70b"))
         ((string= provider "openai")
          '("o4-mini" "o4-mini-low" "o4-mini-medium" "o4-mini-high"
            "o3" "o3-low" "o3-medium" "o3-high"
            "o3-mini" "o3-mini-low" "o3-mini-medium" "o3-mini-high"
            "o1" "o1-low" "o1-medium" "o1-high"
            "o1-mini" "o1-mini-low" "o1-mini-medium" "o1-mini-high"
            "gpt-4.1" "gpt-4.1-mini" "gpt-4.1-nano"
            "gpt-4o" "gpt-4o-mini"))))
       (selected-model
        (completing-read "Select model: " provider-models nil t)))
    (setq genai-api-keys
          (getenv
           (format "%s_API_KEYS"
                   (upcase provider))))
    (setq genai-engine selected-model)
    (message "Switched to %s using model: %s" provider selected-model)))

;; (defun genai-send-buffer-input
;;     ()
;;   "Send text from the last separator as input to GenAI."
;;   (with-current-buffer "*GenAI*"
;;     (let*
;;         ((input-start
;;           (save-excursion
;;             (goto-char
;;              (point-max))
;;             (or
;;              (re-search-backward genai--splitter nil t)
;;              (point-min))))
;;          (input-text
;;           (buffer-substring-no-properties
;;            (save-excursion
;;              (goto-char input-start)
;;              (forward-line 2)
;;              (point))
;;            (point-max))))
;;       (when
;;           (not
;;            (string-empty-p
;;             (string-trim input-text)))
;;         (genai--insert-prompt-template-type-and-engine input-text
;;                                                        "chat")
;;         (genai--run input-text)))))

(defun genai-send-buffer-input ()
  "Send text from the last separator as input to GenAI."
  (with-current-buffer "*GenAI*"
    (let* ((input-start
            (save-excursion
              (goto-char (point-max))
              (or (re-search-backward genai--splitter nil t)
                  (point-min))))
           (input-text
            (buffer-substring-no-properties
             (save-excursion
               (goto-char input-start)
               (forward-line 2)
               (point))
             (point-max))))
      (unless (string-blank-p input-text)
        (genai--insert-prompt-template-type-and-engine input-text
                                                       "chat")
        (genai--run input-text)))))

(defun genai--init
    ()
  "Initialize the GenAI package and check Python dependencies."
  (unless genai-dependencies-checked
    (genai--check-python-dependencies)
    (setq genai-dependencies-checked t)))

(cl-defun genai--ensure-dependencies
    ()
  "Ensure Python dependencies are checked
before running any GenAI functionality."
  (unless genai-dependencies-checked
    (genai--check-python-dependencies)
    (setq genai-dependencies-checked t)))

(cl-defun genai--check-python-dependencies
    ()
  "Check if python mngs package is installed."
  (let*
      ((mngs-version "1.9.8")
       (check-command
        (format
         "%s -c 'import pkg_resources; pkg_resources.require(\"mngs==%s\")'"
         genai-python-bin-path mngs-version)))
    (async-start
     `(lambda
        ()
        (shell-command-to-string ,check-command))
     (lambda
       (result)
       (if
           (string-match "DistributionNotFound\\|VersionConflict"
                         result)
           (when
               (yes-or-no-p
                (format
                 "The required Python package 'mngs>=%s' is not installed or outdated for %s. Install/upgrade it now?"
                 mngs-version genai-python-bin-path))
             (async-start
              `(lambda
                 ()
                 (shell-command-to-string
                  ,(format "%s -m pip install 'mngs>=%s'"
                           genai-python-bin-path mngs-version)))
              (lambda
                (_)
                (message "Package installed/upgraded.")))))))))

(cl-defun genai--create-buffer-file
    (buffer)
  "Create a temporary file with the text in BUFFER and return its file name.
The file will have a .genai extension.
BUFFER can be a buffer object or a buffer name."
  (let
      ((file
        (make-temp-file "genai-prompt-" nil ".md")))
    (condition-case err
        (progn
          (with-temp-file file
            (with-current-buffer
                (get-buffer buffer)
              (insert-buffer-substring buffer)))
          (let
              ((temp-buffer
                (find-file-noselect file)))
            (with-current-buffer temp-buffer
              (text-mode)))
          file)
      (error
       (when file
         (delete-file file))
       (message "Error creating buffer file: %s"
                (error-message-string err))
       nil))))

(cl-defun --genai-find-first-capital
    (string)
  "Find first capital letter in STRING and return cons of (letter . position).
Example: (--genai-find-first-capital \"parapHrase.md\") => (h . 5)"
  (let*
      ((name
        (file-name-sans-extension string))
       (case-fold-search nil)
       (capital-pos
        (string-match "[A-Z]" name)))
    (when capital-pos
      (cons
       (downcase
        (substring name capital-pos
                   (1+ capital-pos)))
       capital-pos))))

(cl-defun genai--fetch-templates
    (dir)
  "Return list of formatted template names from DIR that contain capital letters."
  (when
      (file-exists-p dir)
    (sort
     (delq nil
           (mapcar
            (lambda
              (f)
              (let*
                  ((name-without-ext
                    (substring f 0
                               (string-match "\\.md" f)))
                   (capital-info
                    (--genai-find-first-capital f))
                   (first-capital
                    (car capital-info))
                   (capital-pos
                    (cdr capital-info)))
                (cond
                 ((= capital-pos 0)
                  (format "%s" name-without-ext))
                 (capital-pos
                  (format "%s %s" first-capital name-without-ext))
                 (t nil))))
            (directory-files dir nil ".*[A-Z].*\\.md$")))
     #'string<)))

(cl-defun genai--create-shortcuts
    (templates)
  "Generate shortcuts for templates using numbers for duplicates."
  (let
      ((shortcuts
        (make-hash-table :test 'equal))
       (counts
        (make-hash-table :test 'equal))
       (completion-alist nil))

    ;; Apply predefined mappings first
    (when
        (boundp 'genai-template-mapping)
      (dolist
          (mapping genai-template-mapping)
        (let*
            ((key
              (car mapping))
             (template-name
              (cdr mapping)))
          (when-let
              ((template
                (cl-find template-name templates
                         :key #'car
                         :test #'string=)))
            (puthash key
                     (car template)
                     shortcuts)

            ;; Add combined format for completion
            (push
             (cons
              (format "%s - %s" key
                      (car template))
              (car template))
             completion-alist)
            (puthash
             (substring key 0 1)
             1 counts)))))

    ;; Handle remaining templates
    (setq templates
          (sort templates
                (lambda
                  (a b)
                  (string<
                   (car a)
                   (car b)))))
    (dolist
        (template templates)
      (let*
          ((name
            (car template))
           (suffix
            (cdr template))
           (last-capital
            (if suffix
                (downcase suffix)
              (if
                  (string-match "[A-Z]" name)
                  (downcase
                   (substring
                    (match-string 0 name)
                    0 1))
                (downcase
                 (substring name 0 1)))))
           (count
            (gethash last-capital counts 0))
           (new-key
            (if
                (= count 0)
                last-capital
              (format "%s%d" last-capital count))))
        (unless
            (gethash new-key shortcuts)
          (puthash new-key name shortcuts)

          ;; Add combined format for completion
          (push
           (cons
            (format "%s - %s" new-key name)
            name)
           completion-alist)
          (puthash last-capital
                   (1+ count)
                   counts))))
    (defvar genai--completion-alist nil)
    (setq-default genai--completion-alist completion-alist)
    (defvar genai--completion-function nil)
    (setq genai--completion-function
          (lambda
            (string pred action)
            (if
                (eq action 'metadata)
                '(metadata
                  (category . genai))
              (complete-with-action action genai--completion-alist
                                    string pred))))
    shortcuts))

(cl-defun genai--select-template
    ()
  "Prompt the user to select a template type for the GenAI model."
  (unless
      (minibufferp)
    (let*
        ((capital-templates
          (genai--fetch-templates genai-templates-dir))
         (shortcuts
          (make-hash-table :test 'equal))
         (key-count
          (make-hash-table :test 'equal))
         (prompt-parts nil))

      ;; Handle mapped templates first
      (when
          (boundp 'genai-template-mapping)
        (dolist
            (mapping genai-template-mapping)
          (let*
              ((key
                (car mapping))
               (value
                (cdr mapping))
               (base-key
                (substring key 0 1))
               (count
                (gethash base-key key-count 0)))
            (when
                (member value capital-templates)
              (puthash base-key
                       (1+ count)
                       key-count)
              (puthash key value shortcuts)
              (push
               (format "(%s) %s" key value)
               prompt-parts)))))

      ;; Handle unmapped templates with auto-numbering
      (dolist
          (template capital-templates)
        (unless
            (rassoc template genai-template-mapping)
          (let*
              ((base-key
                (downcase
                 (substring template 0 1)))
               (count
                (gethash base-key key-count 0))
               (key
                (if
                    (> count 0)
                    (format "%s%d" base-key
                            (1+ count))
                  base-key)))
            (puthash base-key
                     (1+ count)
                     key-count)
            (puthash key template shortcuts)
            (push
             (format "(%s) %s" key template)
             prompt-parts))))
      (setq prompt-parts
            (sort prompt-parts 'string<))
      (let*
          ((prompt
            (concat "Template or Manual Instruction:\n"
                    (mapconcat 'identity prompt-parts " ")
                    "\n"))
           (input
            (read-string prompt))
           (template-type
            (or
             (gethash input shortcuts)
             (if
                 (string-blank-p input)
                 "None"
               input))))
        (unless
            (string= input "r")
          (display-buffer
           (get-buffer-create "*GenAI*")))
        template-type))))

(cl-defun genai--safe-shell-quote-argument
    (arg)
  "Safely shell-quote ARG if non-nil and non-empty, else return an empty string."
  (if
      (and arg
           (not
            (string-empty-p arg)))
      (shell-quote-argument arg)
    ""))

(cl-defun genai--insert-prompt-template-type-and-engine
    (prompt template-type)
  "Insert prompt, template type, and engine into *GenAI* buffer.
PROMPT is the user's input, TEMPLATE-TYPE is the selected template."
  (with-current-buffer
      (get-buffer-create "*GenAI*")
    (goto-char
     (point-max))
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
    (insert
     (upcase genai-engine))
    (insert " ")
    (insert "\n\n")
    (goto-char
     (point-max))
    (run-at-time "0 sec" nil #'genai--scroll)))

(defun genai--parse-api-keys
    ()
  "Parse API keys from `genai-api-keys` and store them in `genai-api-keys-parsed`."
  (unless
      (string-empty-p genai-api-keys)
    (let
        ((keys-list
          (split-string genai-api-keys ":")))
      (setq genai-api-keys-parsed keys-list))))

(defun genai--construct-python-command
    (prompt)
  "Construct complete command string..."
  (genai--parse-api-keys)
  (let*
      ((template-type
        (genai--select-template))
       (tmp-prompt-file
        (make-temp-file "genai-prompt-" nil ".txt"))
       (command nil))
    (unwind-protect
        (progn

          ;; Write to file using with-temp-file to handle file operations safely
          (with-temp-file tmp-prompt-file
            (insert prompt))
          (setq command
                (concat
                 (genai--safe-shell-quote-argument
                  genai-python-bin-path)
                 " "
                 (genai--safe-shell-quote-argument
                  genai-python-script-path)
                 " "
                 (mapconcat
                  (lambda
                    (api-key)
                    (concat "--api_key "
                            (genai--safe-shell-quote-argument api-key)))
                  genai-api-keys-parsed " ")
                 " "
                 "--engine "
                 (genai--safe-shell-quote-argument genai-engine)
                 " "
                 "--max_tokens "
                 (genai--safe-shell-quote-argument genai-max-tokens)
                 " "
                 "--temperature "
                 (genai--safe-shell-quote-argument genai-temperature)
                 " "
                 "--human_history_path "
                 (genai--safe-shell-quote-argument
                  genai-history-human-path)
                 " "
                 "--n_history "
                 (genai--safe-shell-quote-argument genai-n-history)
                 " "
                 "--template_type "
                 (genai--safe-shell-quote-argument template-type)
                 " "
                 "--prompt_file "
                 (genai--safe-shell-quote-argument tmp-prompt-file))))

      (genai--insert-prompt-template-type-and-engine prompt
                                                     template-type)
      command)))

;; ;;;###autoload

;; (defun genai-show-history
;;     (&optional num-interactions)
;;   "Show NUM-INTERACTIONS lines of chat history in a temporary buffer."
;;   (interactive
;;    "sEnter the number of latest interactions (default: 64): ")
;;   (let*
;;       ((genai-all-history-buffer
;;         (get-buffer-create "*GenAI All History*"))
;;        (num-interactions
;;         (cond
;;          ((null num-interactions)
;;           "64")
;;          ((numberp num-interactions)
;;           (number-to-string num-interactions))
;;          ((stringp num-interactions)
;;           (if
;;               (string-empty-p num-interactions)
;;               "1024"
;;             num-interactions))
;;          (t "1024"))))
;;     (with-current-buffer genai-all-history-buffer
;;       (erase-buffer))
;;     (display-buffer genai-all-history-buffer)
;;     (let
;;         ((command-list
;;           (list genai-python-bin-path
;;                 genai-python-script-path-show-history
;;                 "--human_history_path" genai-history-human-path
;;                 "--n_interactions" num-interactions)))
;;       (make-process
;;        :name "genai-show-history"
;;        :buffer genai-all-history-buffer
;;        :command command-list
;;        :sentinel
;;        (lambda
;;          (_process event)
;;          (when
;;              (string= event "finished\n")
;;            (with-current-buffer genai-all-history-buffer
;;              (goto-char
;;               (point-min))
;;              (when
;;                  (search-forward genai--splitter nil t)
;;                (goto-char
;;                 (match-beginning 0))
;;                (delete-region
;;                 (point)
;;                 (point-max)))
;;              (genai--scroll-history)
;;              (markdown-mode))))))
;;     (message "Loading history to *GenAI All History* buffer...")))

(defun genai-show-history ()
  "Open and auto-revert the human-readable history markdown."
  (interactive)
  (let ((history-buf
         (find-file-noselect genai-history-human-readable-path)))
    (with-current-buffer history-buf
      (auto-revert-mode 1)
      (genai-mode)
      (goto-char (point-max)))
    (display-buffer history-buf)))

;;;###autoload

;; (defun genai-reset-history
;;     ()
;;   "Backup and reset the GenAI history files."
;;   (interactive)
;;   (let*
;;       ((backup-dir
;;         (concat genai-home-dir "histories"))
;;        (timestamp
;;         (format-time-string "%Y-%m-%d-%H-%M-%S"))
;;        (human-backup
;;         (concat backup-dir "/history-human-" timestamp ".json"))
;;        (ai-backup
;;         (concat backup-dir "/history-ai-" timestamp ".json")))

;;     ;; Create backup directory if it doesn't exist
;;     (unless
;;         (file-exists-p backup-dir)
;;       (make-directory backup-dir t))

;;     ;; Backup existing history files
;;     (when
;;         (file-exists-p genai-history-human-path)
;;       (rename-file genai-history-human-path human-backup))
;;     (when
;;         (file-exists-p genai-history-ai-path)
;;       (rename-file genai-history-ai-path ai-backup))

;;     ;; Create new empty history files
;;     (with-temp-file genai-history-human-path
;;       (insert "[]"))
;;     (with-temp-file genai-history-ai-path
;;       (insert "[]"))
;;     (message "GenAI history has been reset. Backups created in %s"
;;              backup-dir)))

(defun genai-reset-history ()
  "Backup and reset JSON and human-readable history."
  (interactive)
  (let* ((backup-dir (concat genai-home-dir "histories/"))
         (timestamp (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (human-json-bkp
          (concat backup-dir "history-human-" timestamp ".json"))
         (human-md-bkp
          (concat backup-dir "history-human-readable" timestamp ".md"))
         (ai-json-bkp
          (concat backup-dir "history-ai-"    timestamp ".json")))
    ;; ensure backup directory exists
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))
    ;; backup existing files
    (when (file-exists-p genai-history-human-path)
      (rename-file genai-history-human-path human-json-bkp))
    (when (file-exists-p genai-history-human-readable-path)
      (rename-file genai-history-human-readable-path human-md-bkp))
    (when (file-exists-p genai-history-ai-path)
      (rename-file genai-history-ai-path ai-json-bkp))
    ;; create fresh empty history
    (with-temp-file genai-history-human-path
      (insert "[]"))
    (with-temp-file genai-history-human-readable-path
      (insert ""))
    (with-temp-file genai-history-ai-path
      (insert "[]"))
    (message "History reset; backups in %s" backup-dir)))

(defun genai--start-spinner
    ()
  "Start the spinner animation in the GenAI buffer."
  (when
      (and
       (get-buffer "*GenAI*")
       (not genai--spinner-timer))
    (with-current-buffer "*GenAI*"
      (goto-char
       (point-max))
      (search-backward
       (upcase genai-engine))
      (goto-char
       (line-end-position))
      (setq genai--fixed-spinner-position
            (point-marker))
      (let
          ((font-lock-mode nil))
        (setq genai--spinner-timer
              (run-with-timer 0 0.1
                              (lambda
                                ()
                                (with-current-buffer "*GenAI*"
                                  (save-excursion
                                    (goto-char
                                     genai--fixed-spinner-position)
                                    (let
                                        ((inhibit-read-only t))
                                      (delete-region
                                       (point)
                                       (line-end-position))
                                      (insert
                                       (propertize
                                        (nth genai--spinner-index
                                             genai--spinner-frames)
                                        'face
                                        '(:foreground "blue")))
                                      (setq genai--spinner-index
                                            (mod
                                             (1+ genai--spinner-index)
                                             (length
                                              genai--spinner-frames)))))))))))))

(defun genai--stop-spinner
    ()
  "Stop the spinner animation."
  (when genai--spinner-timer
    (cancel-timer genai--spinner-timer)
    (setq genai--spinner-timer nil)
    (with-current-buffer "*GenAI*"
      (save-excursion
        (when genai--fixed-spinner-position
          (goto-char genai--fixed-spinner-position)
          (let
              ((inhibit-read-only t))
            (delete-region
             (point)
             (line-end-position))
            (delete-char -1)
            (goto-char
             (point-max))
            (insert "\n\n")
            (insert genai--splitter)
            (insert "\n\n")
            (goto-char
             (point-max))))
        (setq genai--fixed-spinner-position nil)))))

;; Update process sentinel

(defun genai--process-sentinel
    (_process msg)
  "Custom sentinel for the GenAI process."
  (genai-interactive-mode -1)
  (cond
   ((string-match-p "finished\\|exited" msg)
    (genai--stop-spinner)
    (genai-interactive-mode -1)
    (message "GenAI process finished.")
    (genai--clean-up-all))
   ((string-match-p "error" msg)
    (genai--stop-spinner)
    (genai-interactive-mode -1)
    (message "GenAI process encountered an error: %s" msg))
   (t
    (genai-interactive-mode -1)
    (message "GenAI process: %s" msg))))

(cl-defun genai--clean-up-all
    ()
  (genai--clean-up-output))

(cl-defun genai--start-python-process
    (prompt)
  "Start the GenAI process with the given PROMPT using a shell command."
  (when
      (and genai--process
           (process-live-p genai--process))
    (interrupt-process genai--process)
    (delete-process genai--process))
  (let*
      ((command
        (genai--construct-python-command prompt))
       (process
        (start-process-shell-command "genai--process" "*GenAI*"
                                     command)))
    (setq genai--process process)
    (set-process-sentinel process #'genai--process-sentinel)
    (genai--start-spinner)
    process))

(cl-defun genai--stop-python-process
    ()
  "Stop the GenAI process if it is running."
  (when
      (and genai--process
           (process-live-p genai--process))
    (interrupt-process genai--process)
    (genai-interactive-mode -1)
    (message "GenAI process interrupted.")))

(defun genai--trim-buffer
    ()
  "Trim *GenAI* buffer to keep only the last genai-buffer-max-lines lines."
  (with-current-buffer "*GenAI*"
    (save-excursion
      (goto-char
       (point-max))
      (forward-line
       (- genai-buffer-max-lines))
      (when
          (>
           (point)
           (point-min))
        (delete-region
         (point-min)
         (point))))))

(defun genai--clean-up-output
    ()
  "Remove ANSI escape codes and specific unwanted messages from the *GenAI* buffer."
  (with-current-buffer
      (get-buffer-create "*GenAI*")
    (ansi-color-apply-on-region
     (point-min)
     (point-max))
    (save-excursion
      (goto-char
       (point-min))
      (while
          (search-forward-regexp "\x1b\\[\\([0-9;]*\\)m" nil t)
        (replace-match "")))
    (genai--trim-buffer)))

;;;###autoload

(cl-defun genai--run
    (prompt)
  "Run GenAI command with prompt."
  (cond
   ((equal prompt "g")
    (switch-to-buffer-other-window "*GenAI*")
    (keyboard-quit)
    (message "Jump to the \"*GenAI*\" buffer"))
   ((equal prompt "h")
    (genai-show-history)
    (keyboard-quit))
   (t
    (with-current-buffer
        (get-buffer-create "*GenAI*")
      (font-lock-ensure)
      (message "GenAI: Running...")
      (genai--start-python-process prompt)))))

;; (defun genai--dired-get-contents ()
;;   "Recursively get contents of marked files on dired mode, handling only safe files.
;; If no files specified, just call ordinal genai-on-region"
;;   (let* ((marked-files
;;           (dired-get-marked-files nil nil
;;                                   (lambda (f)
;;                                     (dired-file-marker f))))
;;          (safe-extensions
;;           '(".el" ".py" ".sh" ".vba" ".ps1" ".src" ".txt" ".md" ".org"
;;             ".yml" ".yaml" ".json" ".conf"))
;;          (size-limit
;;           (* 1024 1024))
;;          (contents ""))
;;     (if (null marked-files)
;;         (read-string "Enter prompt: " "")
;;       (cl-labels
;;           ((process-file
;;              (file)
;;              (cond
;;               ((file-directory-p file)
;;                (dolist (f (directory-files file t "^[^.]"))
;;                  (process-file f)))
;;               ((and (file-regular-p file)
;;                     (or (null (file-name-extension file))  ;; Allow files with no extension
;;                         (member (file-name-extension file t)
;;                                 safe-extensions))
;;                     (< (file-attribute-size (file-attributes file))
;;                        size-limit))
;;                (setq contents
;;                      (concat contents
;;                              (format "\n\n;;; ----- %s -----\n\n" file)
;;                              (with-temp-buffer
;;                                (insert-file-contents file)
;;                                (buffer-string))))))))
;;         (dolist (file marked-files)
;;           (process-file file)))
;;       contents)))

(defcustom genai-whitelist-extensions
  '(".el" ".py" ".sh" ".vba" ".ps1" ".src" ".txt" ".md" ".org" ".yml"
    ".yaml" ".json" ".conf")
  "List of whitelisted file extensions for `genai--dired-get-contents'.")

(defcustom genai-blacklist-extensions
  nil
  "List of blacklisted file extensions for `genai--dired-get-contents'.")

(defcustom genai-whitelist-expressions
  nil
  "List of whitelisted path expressions (regexp strings or lambda) for `genai--dired-get-contents'.")

(defcustom genai-blacklist-expressions
  '("RUNNING" "FINISHED")
  "List of blacklisted path expressions (regexp strings or lambda) for `genai--dired-get-contents'.")

(defun genai--match-path-expressions (file exprs)
  ;; Return t if file matches any regexp in exprs
  (cl-some (lambda (expr)
             (and (stringp expr)
                  (string-match-p expr file)))
           exprs))

(defun genai--should-include-file
    (file white-ext black-ext white-expr black-expr size-limit)
  ;; Return t if file should be included, otherwise nil
  (and
   (file-regular-p file)
   (not (genai--match-path-expressions file black-ext))
   (not (genai--match-path-expressions file black-expr))
   (or
    (genai--match-path-expressions file white-ext)
    (genai--match-path-expressions file white-expr))
   (< (file-attribute-size (file-attributes file)) size-limit)))

(defun genai--dired-get-contents ()
  "Recursively get contents of marked files on dired mode, handling only whitelisted files.
If no files specified, just call ordinal genai-on-region"
  (let* ((marked-files
          (dired-get-marked-files nil nil
                                  (lambda (file)
                                    (dired-file-marker file))))
         (white-ext genai-whitelist-extensions)
         (black-ext genai-blacklist-extensions)
         (white-expr genai-whitelist-expressions)
         (black-expr genai-blacklist-expressions)
         (size-limit (* 1024 1024))
         (contents ""))
    (if (null marked-files)
        (read-string "Enter prompt: " "")
      (cl-labels
          ((process-file
             (file)
             (cond
              ((file-directory-p file)
               (dolist (subfile (directory-files file t "^[^.]"))
                 (process-file subfile)))
              ((genai--should-include-file
                file white-ext black-ext white-expr black-expr
                size-limit)
               (setq contents
                     (concat contents
                             (format "\n\n;;; ----- %s -----\n\n" file)
                             (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))))))))
        (dolist (file marked-files)
          (process-file file)))
      contents)))

;;;###autoload

(defun genai-on-region
    ()
  "Run GenAI command on selected region, dired files or prompt.
If a region is selected, use that text as the prompt.
If in dired-mode with marked files, concatenate their contents.
Otherwise, prompt the user to enter a prompt.
The response will be displayed in the *GenAI* buffer."
  (interactive)
  (genai--init)
  (genai--ensure-dependencies)
  (genai-interactive-mode 1)
  (let*
      ((region-text
        (cond
         ((use-region-p)
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
         ((eq major-mode 'dired-mode)
          (genai--dired-get-contents))
         (t
          (read-string "Enter prompt: " ""))))
       (genai-interactive-mode -1)
       (buffer
        (get-buffer-create "*GenAI*")))
    (with-current-buffer buffer
      (unless
          (eq major-mode 'genai-mode)
        (genai-mode)))
    (when
        (use-region-p)
      (deactivate-mark))
    (genai--run region-text)))

;; (defun genai-on-region
;;     ()
;;   "Run GenAI command on selected region, dired files or prompt."
;;   (interactive)
;;   (genai--init)
;;   (genai--ensure-dependencies)
;;   (genai-interactive-mode 1)
;;   (let*
;;       ((region-text
;;         (cond
;;          ((use-region-p)
;;           (buffer-substring-no-properties
;;            (region-beginning)
;;            (region-end)))
;;          ((eq major-mode 'dired-mode)
;;           (genai--dired-get-contents))
;;          (t
;;           (read-string "Enter prompt: " ""))))
;;        (buffer
;;         (get-buffer-create "*GenAI*")))
;;     (genai-interactive-mode -1)
;;     (with-current-buffer buffer
;;       (unless
;;           (eq major-mode 'genai-mode)
;;         (genai-mode)))
;;     (when
;;         (use-region-p)
;;       (deactivate-mark))
;;     (genai--run region-text)))

(cl-defun genai--scroll
    ()
  "Scrolls to the first genai--splitter from the end of the *GenAI* buffer."
  (if-let
      ((genai-buffer
        (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char
         (point-max))

        ;; Search for the splitter and update found position
        (when
            (re-search-backward genai--splitter nil t)
          (beginning-of-line))
        (let
            ((pos
              (point)))
          (when-let
              ((window
                (get-buffer-window genai-buffer 0)))
            (set-window-point window pos)
            (with-selected-window window
              (recenter 0)))))))

(cl-defun genai--scroll-history
    ()
  "Scrolls to the first genai--splitter from the end of the *GenAI* buffer."
  (if-let
      ((genai-buffer
        (get-buffer "*GenAI History*")))
      (with-current-buffer genai-buffer
        (goto-char
         (point-max))

        ;; Search for the splitter and update found position
        (when
            (re-search-backward genai--splitter-human-readable nil t)
          (beginning-of-line))
        (let
            ((pos
              (point)))
          (when-let
              ((window
                (get-buffer-window genai-buffer 0)))
            (set-window-point window pos)
            (with-selected-window window
              (recenter 0)))))))

(cl-defun genai--insert-splitter-after-run
    ()
  "Insert splitter after retrieving the Gen AI output"
  (if-let
      ((genai-buffer
        (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char
         (point-max))
        (insert genai--splitter)
        (insert "\n\n"))))

;;;###autoload

(defun genai-copy-last
    ()
  "Copy the last LLM output into kill ring"
  (interactive)
  (if-let
      ((genai-buffer
        (get-buffer "*GenAI*")))
      (with-current-buffer genai-buffer
        (goto-char
         (point-max))
        (set-mark
         (point))
        (goto-char
         (point-min))
        (kill-ring-save
         (region-beginning)
         (region-end)))))

;;;###autoload

(defun genai-copy-code-blocks
    ()
  "Copy code blocks in the last LLM output into kill ring"
  (interactive)
  (with-current-buffer
      (get-buffer "*GenAI*")
    (goto-char
     (point-max))
    (let
        ((count nil)
         blocks start end)
      (when
          (search-backward genai--splitter nil t)
        (goto-char
         (match-end 0))
        (while
            (re-search-forward genai--code-block-start-delimiter nil t)
          (forward-line 1)
          (setq start
                (point))
          (if
              (re-search-forward genai--code-block-end-delimiter nil t)
              (progn
                (setq end
                      (match-beginning 0))
                (push
                 (buffer-substring-no-properties start end)
                 blocks))))

        ;; Remove nreverse to keep the original order of discovery
        (dolist
            (block blocks)
          (kill-new block))
        (setq count
              (length blocks))
        (message "Code blocks copied (n = %d)" count)))))

;;;###autoload

(defun genai-next-code-block
    ()
  "Navigate to the next code block and select the content"
  (interactive)
  (with-current-buffer "*GenAI*"
    (end-of-line)
    (when
        (looking-at-p genai--code-block-end-delimiter)
      (forward-line)
      (beginning-of-line))
    (when
        (re-search-forward genai--code-block-start-delimiter nil t)
      (let
          ((start
            (point)))
        (when
            (re-search-forward genai--code-block-end-delimiter nil t)
          (let
              ((end
                (match-beginning 0)))
            (goto-char end)
            (set-mark
             (1+ start))
            (activate-mark)
            (kill-ring-save start end)
            (pulse-momentary-highlight-region
             (1+ start)
             end)
            (message "Copied."))))
      (deactivate-mark))))

;;;###autoload

(defun genai-previous-code-block
    ()
  "Navigate to the previous code block and select the content"
  (interactive)
  (with-current-buffer "*GenAI*"

    ;; Find the previous code block end delimiter
    (when
        (re-search-backward genai--code-block-end-delimiter nil t)
      (backward-char)
      (let
          ((end
            (point)))

        ;; Find the previous code block start delimiter
        (if
            (re-search-backward genai--code-block-start-delimiter nil
                                t)
            (progn
              (forward-line 1)
              (set-mark end)
              (activate-mark)
              (kill-ring-save
               (point)
               end)
              (pulse-momentary-highlight-region
               (+
                (point)
                1)
               end)
              (message "Copied."))
          (deactivate-mark))))))

(add-hook 'comint-output-filter-functions 'genai--clean-up-output)

;; Keybindings

(define-key genai-mode-map
            (kbd "M-n")
            'genai-next-code-block)

(define-key genai-mode-map
            (kbd "M-p")
            'genai-previous-code-block)

(define-key genai-mode-map
            (kbd "C-c C-c")
            'genai-send-buffer-input)

;;; genai.el ends here


(provide 'genai)

(when
    (not load-file-name)
  (message "genai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))