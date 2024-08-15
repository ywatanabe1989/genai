;;; genai.el --- Client for interactive with LLMs -*- lexical-binding: t; -*-

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
;; ;; Key Bindings
;; (define-key global-map (kbd "C-M-g") 'genai-on-region)

;;; Templates:
;;
;; Templates can be managed under the './templates' directory
;; (default: $HOME/.emacs.d/lisp/emacs-genai/templates/*.md).
;; Uppercase letters in the template file name are used as shortcut keys.
;; The "PLACEHOLDER" keyword in a template file will be replaced
;; with your input.


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
  (let ((check-command (format "%s -c 'import pkg_resources; pkg_resources.require(\"mngs==1.5.5\")'"
                               genai-python-bin-path)))
    (async-start
     `(lambda () (shell-command-to-string ,check-command))
     (lambda (result)
       (if (string-match "DistributionNotFound\\|VersionConflict" result)
           (when (yes-or-no-p (format "The required Python package 'mngs==1.5.5' is not installed or outdated for %s. Install/upgrade it now?" genai-python-bin-path))
             (async-start
              `(lambda () (shell-command-to-string ,(format "%s -m pip install mngs==1.5.5" genai-python-bin-path)))
              (lambda (_) (message "Package installed/upgraded."))))
         (message (format "All required Python packages are installed and up-to-date for %s." genai-python-bin-path)))))))

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

(cl-defun genai--fetch-templates (dir)
  "Return list of templates file names that start with capital letters."
  (when (file-exists-p dir)
    (let ((files (directory-files dir nil "^[A-Z].*\\.md$")))
      (sort (mapcar (lambda (f) (substring f 0 (string-match "\\.md" f))) files) 'string>))))


(cl-defun genai--create-shortcuts (templates)
  "Generate shortcuts for templates."
  (let ((shortcuts (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal)))

    ;; Sort templates first by their display name to ensure alphabetical precedence
    (setq templates (sort templates (lambda (a b) (string< (car a) (car b)))))
    (dolist (template templates)
      (let* ((name (car template))
             (base (downcase (substring name 0 1)))
             (count (gethash base counts 0))
             (new-key base))

        ;; Adjust key for duplicates
        (when (> count 0)
          (setq new-key (concat base (make-string count ?'))))

        ;; Update hash tables
        (puthash new-key name shortcuts)
        (puthash base (1+ count) counts)))

    shortcuts))

(cl-defun genai--select-template ()
  "Prompt the user to select a template type for the GenAI model.
If INITIAL-INPUT is non-nil, it returns it without prompting.
Otherwise, it prompts the user with available templates.
Returns the selected template type or None
if the input is non-standard or empty."
  (interactive)
  (unless (minibufferp)
    (let* ((capital-templates (genai--fetch-templates genai-templates-dir))
           (templates-with-shortcuts (mapcar (lambda (template) (cons template template)) capital-templates))
           (shortcuts (genai--create-shortcuts templates-with-shortcuts))
           (shortcut-list (hash-table-keys shortcuts))
           (prompt (concat "Enter or select preceeding prompt: "
                           (mapconcat (lambda (key) (concat "(" key ") " (gethash key shortcuts)))
                                      (reverse shortcut-list) ", ") ":\n"))
           (input (read-string prompt))

           (template-type (or (gethash input shortcuts) (if (string-blank-p input) "None" input))))

      (when (called-interactively-p 'interactive)
        (message "Template type selected: %s" template-type))

        template-type)))

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
    (insert "> ")
    (insert "YOU")
    (insert "\n\n")
    (insert "> ")
    (insert template-type)
    (insert "\n\n")
    (insert "> ")
    (insert prompt)
    (insert "\n\n")
    (insert genai--splitter)
    (insert "\n\n")
    (insert "> ")
    (insert (upcase genai-engine))
    (insert "\n\n")
    (goto-char (point-max))
    (run-at-time "0 sec" nil #'genai--scroll)))

(defun genai--parse-api-keys ()
  "Parse API keys from `genai-api-keys` and store them in `genai-api-keys-parsed`."
  (interactive)

  ;; Check if the API keys string is not empty
  (unless (string-empty-p genai-api-keys)
    (let ((keys-list (split-string genai-api-keys ":")))
      (setq genai-api-keys-parsed keys-list)  ;; Set the variable to the list of extracted API keys
      (message "Parsed API Keys: %S" genai-api-keys-parsed))))


(defun genai--construct-python-command (prompt)
  "Construct complete command string for starting the GenAI Python process."
  (interactive "sEnter prompt: ")

  (let* ((template-type (genai--select-template))
         (prompt-arg (if (or (null prompt) (string-empty-p prompt))
                         "''" ; Empty string between apostrophes
                       (shell-quote-argument (replace-regexp-in-string "'" "\\\\'" prompt))))
                         ;; (genai--safe-shell-quote-argument prompt)))
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



;; (cl-defun genai--construct-python-command (prompt)
;;   "Construct complete command string for starting the GenAI Python process."
;;   (interactive "sEnter prompt: ")

;;   (let* ((template-type (genai--select-template))
;;          (prompt-arg (if (or (null prompt) (string-empty-p prompt))
;;                          "''" ; Empty string between apostrophes
;;                          (genai--safe-shell-quote-argument prompt)))
;;          (command (format "%s \
;;                            %s \
;;                            --api_key %s \
;;                            --engine %s \
;;                            --max_tokens %s \
;;                            --temperature %s \
;;                            --human_history_path %s \
;;                            --n_history %s \
;;                            --template_type %s \
;;                            --prompt %s"
;;                           (genai--safe-shell-quote-argument genai-python-bin-path)
;;                           (genai--safe-shell-quote-argument genai-python-script-path)
;;                           (genai--safe-shell-quote-argument genai-api-key)
;;                           (genai--safe-shell-quote-argument genai-engine)
;;                           (genai--safe-shell-quote-argument genai-max-tokens)
;;                           (genai--safe-shell-quote-argument genai-temperature)
;;                           (genai--safe-shell-quote-argument genai-history-human-path)
;;                           (genai--safe-shell-quote-argument genai-n-history)
;;                           (genai--safe-shell-quote-argument template-type)
;;                           prompt-arg)))

;;     (genai--insert-prompt-template-type-and-engine prompt template-type)

;;     command))

;; (cl-defun genai--construct-python-command (prompt) ;
;;   "Construct complete command string for starting the GenAI Python process."
;;   (interactive "sEnter prompt: ")

;;   (let* ((template-type (genai--select-template))
;;          (command (format "%s \
;;                            %s \
;;                            --api_key %s \
;;                            --engine %s \
;;                            --max_tokens %s \
;;                            --temperature %s \
;;                            --human_history_path %s \
;;                            --n_history %s \
;;                            --template_type %s \
;;                            --prompt %s"
;;                           (genai--safe-shell-quote-argument genai-python-bin-path)
;;                           (genai--safe-shell-quote-argument genai-python-script-path)
;;                           (genai--safe-shell-quote-argument genai-api-key)
;;                           (genai--safe-shell-quote-argument genai-engine)
;;                           (genai--safe-shell-quote-argument genai-max-tokens)
;;                           (genai--safe-shell-quote-argument genai-temperature)
;;                           (genai--safe-shell-quote-argument genai-history-human-path)
;;                           (genai--safe-shell-quote-argument genai-n-history)
;;                           (genai--safe-shell-quote-argument template-type)
;;                           (genai--safe-shell-quote-argument prompt))))

;;     (genai--insert-prompt-template-type-and-engine prompt template-type)

;;     command))

;;;###autoload
(defun genai-show-history ()
  "Show the GenAI history in a temporary buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*GenAI All History*"))
        (temp-file (make-temp-file "genai-history-" nil ".md")))
    (with-current-buffer buffer
      (erase-buffer))
    (let ((command-list (list genai-python-bin-path
                              genai-python-script-path-show-history
                              "--human_history_path" genai-history-human-path
                              "--output" temp-file)))
      (make-process
       :name "genai-show-history"
       :buffer buffer
       :command command-list
       :sentinel (lambda (process event)
                   (when (string= event "finished\n")
                     (with-current-buffer (process-buffer process)
                       (insert-file-contents temp-file)
                       (delete-file temp-file)
                       (goto-char (point-min))
                       (genai--scroll-history)
                       (markdown-mode)
                       (display-buffer (current-buffer)))))))
    (message "Loading history...")))

;;;###autoload
(defun genai-reset-history ()
  "Resets the history files for human and AI, but asks for confirmation first."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to reset the history?")
      (with-temp-file genai-history-human-path
        (insert "[]"))
      (with-temp-file genai-history-ai-path
        (insert "[]"))
      (message "History reset successfully")))

(defun genai--process-sentinel (_process msg)
  "Custom sentinel for the GenAI process.
Handles different process states and calls cleanup when appropriate."
  (cond
   ((string-match-p "finished\\|exited" msg)
    (progress-reporter-done genai--progress-reporter)
    (message "GenAI process finished.")
    (genai--clean-up-all))
   ((string-match-p "error" msg)
    (progress-reporter-done genai--progress-reporter)
    (message "GenAI process encountered an error: %s" msg))
   (t
    (message "GenAI process: %s" msg))))

(cl-defun genai--clean-up-all ()
  (interactive)
  (genai--clean-up-output))

(cl-defun genai--update-progress ()
  "Update the progress reporter."
  (when genai--progress-reporter
    (progress-reporter-update genai--progress-reporter (random 100))
    (when (process-live-p genai--process)
      (run-with-timer 0.5 nil #'genai--update-progress))))

;; working, probably due to correct command parsing
(cl-defun genai--start-python-process (prompt)
  "Start the GenAI process with the given PROMPT using a shell command.
   If a process is already running, stop it before starting a new one."
  (interactive "sEnter prompt: ")

  ;; Stop existing process if running
  (when (and genai--process (process-live-p genai--process))
    (interrupt-process genai--process)
    (delete-process genai--process)
    (message "Existing GenAI process stopped."))

  (let* ((command (genai--construct-python-command prompt))
         (process (start-process-shell-command "genai--process" "*GenAI*" command)))
    (setq genai--process process)
    (set-process-sentinel process #'genai--process-sentinel)

    ;; Start the progress reporter
    (setq genai--progress-reporter (make-progress-reporter "GenAI Processing..." 0 100))
    (genai--update-progress)

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
    (goto-char (point-max))
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
  (genai--init)  ; Ensure initialization at first use.
  (genai--ensure-dependencies)
  (let* ((region-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (read-string "Enter prompt: " "")))
         (buffer (get-buffer-create "*GenAI*")))

    ;; Prepare the buffer for output
    (with-current-buffer buffer
      (unless (eq major-mode 'genai-mode)
        (genai-mode)))

    ;; Run the Gen AI
    (genai--run region-text)

    ;; Display the output to the *GenAI* buffer
    (display-buffer buffer)))

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

(provide 'genai)

;;; genai.el ends here
