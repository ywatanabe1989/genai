;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-30 18:25:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; variables.el --- GenAI customizable variables

(defgroup genai nil
  "Customization group for GenAI."
  :group 'applications
  :prefix "genai-")

(defcustom genai-home-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The home directory of genai.el."
  :type 'string)

(defcustom genai-python-bin-path "/usr/bin/python3"
  "Path to the Python binary."
  :type 'string)

(defcustom genai-python-script-path
  (concat genai-home-dir "genai.py")
  "Path to the Python script."
  :type 'string)

(defcustom genai-templates-dir
  (concat genai-home-dir "templates/")
  "Directory for prompt templates."
  :type 'string)

(defcustom genai-history-human-path
  (concat genai-home-dir "history-human-secret.json")
  "Path to human history JSON."
  :type 'string)

(defcustom genai-history-human-readable-path
  (concat genai-home-dir "history-human-readable-secret.md")
  "Path to human-readable history."
  :type 'string)

(defcustom genai-history-ai-path
  (replace-regexp-in-string "human" "ai" genai-history-human-path)
  "Path to the history file used by genai.el."
  :type 'string)

(defcustom genai-engine
  (getenv "GENAI_ENGINE")
  "LLM engine to use."
  :type 'string)

(defcustom genai-max-tokens "2000"
  "Maximum number of tokens."
  :type 'string)

(defcustom genai-n-history "5"
  "History entries to keep."
  :type 'string)

(defcustom genai-temperature "0"
  "Temperature parameter."
  :type 'string)

(defcustom genai-buffer-max-lines (expt 2 14)
  "Max lines in *GenAI* buffer."
  :type 'integer)

(defvar genai-dependencies-checked nil
  "Whether Python dependencies have been checked.")

(defvar genai--process nil
  "Process object for GenAI.")

(defconst genai--splitter "\n---\n"
  "Separator between GenAI responses.")

(defconst genai--code-block-start-delimiter "```.*$"
  "Start of code block.")

(defconst genai--code-block-end-delimiter "```$"
  "End of code block.")

;; File filtering configuration - ordered by priority (highest to lowest)

;; Priority 1: File size constraints (checked first for performance)

(defcustom genai-dired-filter-file-size-min 0
  "Minimum file size in bytes to process."
  :type 'integer
  :group 'genai)

(defcustom genai-dired-filter-file-size-max-mb 10
  "Maximum file size in megabytes to process."
  :type 'integer
  :group 'genai)

(defcustom genai-dired-filter-file-size-max
  (* genai-dired-filter-file-size-max-mb 1000000)
  "Maximum file size in bytes to process."
  :type 'integer
  :group 'genai)

;; Priority 2: Filename patterns (highest priority - overrides all other filters)

(defcustom genai-dired-filter-allowed-filename-patterns
  '()
  "Regexp patterns for filenames that are ALWAYS allowed (overrides all other filters)."
  :type '(repeat string)
  :group 'genai)

;; Priority 3: Extensions filtering (if filename patterns don't match)

(defcustom genai-dired-filter-allowed-extensions
  '(".el"
    ".py"
    ".ipynb"
    ".sh"
    ".vba"
    ".ps1"
    ".src"
    ".txt"
    ".md"
    ".org"
    ".yml"
    ".yaml"
    ".json"
    ".conf"
    ".log"
    ".js"
    ".ts"
    ".css"
    ".html"
    ".xml")
  "File extensions that are allowed for processing."
  :type '(repeat string)
  :group 'genai)

(defcustom genai-dired-filter-ignored-extensions
  '(".gz"
    ".pyc"
    ".pyo"
    ".pyd"
    ".so"
    ".dll"
    ".exe"
    ".zip"
    ".tar"
    ".rar"
    ".7z"
    ".iso"
    ".bin"
    ".dat"
    ".db"
    ".sqlite"
    ".pdf"
    ".jpg"
    ".jpeg"
    ".png"
    ".gif"
    ".mp3"
    ".mp4"
    ".avi"
    ".mov"
    ".bz2"
    ".xz"
    ".ttf"
    ".otf"
    ".eot"
    ".woff"
    ".woff2"
    ".class"
    ".jar"
    ".o"
    ".obj"
    ".lib"
    ".doc"
    ".docx"
    ".ppt"
    ".pptx"
    ".xls"
    ".xlsx"
    ".apk"
    ".ipa"
    ".dmg"
    ".deb"
    ".rpm"
    ".psd"
    ".xcf"
    "zotero_translators")
  "File extensions that are ignored during processing."
  :type '(repeat string)
  :group 'genai)

;; Priority 4: Filename ignore patterns

(defcustom genai-dired-filter-ignored-filename-patterns
  '(
    "\\.backup$"
    "\\.bak$"
    )
  "Regexp patterns for filenames that should be ignored."
  :type '(repeat string)
  :group 'genai)

;; Priority 5: Parent directory patterns

(defcustom genai-dired-filter-ignored-parents-patterns
  '()
  "Regexp patterns for parent directories that should be ignored."
  :type '(repeat string)
  :group 'genai)

(defcustom genai-dired-filter-allowed-parents-patterns
  '()
  "Regexp patterns for parent directories that are allowed."
  :type '(repeat string)
  :group 'genai)

;; Priority 6: Child directory patterns

(defcustom genai-dired-filter-ignored-children-patterns
  '(
    "/.git/"
    "/node_modules/"
    "/__pycache__/"
    "/.pytest_cache/"
    "/.mypy_cache/"
    "/venv/"
    "/.venv/"
    "/env/"
    "/.env/"
    "/build/"
    "/dist/"
    "/target/"
    "/.sass-cache/"
    "/coverage/"
    "/.coverage/"
    "/logs/"
    "/tmp/"
    "/temp/"
    "/.tmp/"
    "/RUNNING/"
    "/FINISHED/"
    "/FINISHED_SUCCESS/"
    "/FINISHED_ERROR/"
    "/2024Y"
    "/2025Y"
    "/legacy/"
    "/.legacy/"
    "/.old/"
    "/old/"
    )
  "Regexp patterns for child directories that should be ignored."
  :type '(repeat string)
  :group 'genai)

(defcustom genai-dired-filter-allowed-children-patterns
  '()
  "Regexp patterns for child directories that are allowed."
  :type '(repeat string)
  :group 'genai)

;; UI configuration

(defcustom genai-dired-buffer-name "*GenAI Files*"
  "Name of the buffer used for file selection."
  :type 'string
  :group 'genai)

(defcustom genai-buffer-name
  "*GenAI*"
  "Main buffer name for genai package"
  :type 'string
  :group 'genai)

(defcustom genai-buffer-name-history
  "*GenAI History*"
  "History buffer name for genai package"
  :type 'string
  :group 'genai)


(provide 'genai-variables)

(when
    (not load-file-name)
  (message "genai-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))