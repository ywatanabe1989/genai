;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 15:57:18>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-variables.el

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

(defcustom genai-whitelist-extensions
  '(".el" ".py" ".sh" ".vba" ".ps1" ".src" ".txt" ".md" ".org"
    ".yml"
    ".yaml" ".json" ".conf")
  "Allowed file extensions for dired prompts.")

(defcustom genai-whitelist-expressions
  '("__pycache__")
  "Additional regexp or lambda to include files.")

(defcustom genai-blacklist-extensions
  '(".gz" ".pyc" ".pyo" ".pyd" ".so" ".dll" ".exe" ".zip" ".tar"
    ".rar" ".7z" ".iso" ".bin" ".dat" ".db" ".sqlite" ".pdf"
    ".jpg" ".jpeg" ".png" ".gif" ".mp3" ".mp4" ".avi" ".mov"
    ;; added formats
    ".bz2" ".xz" ".ttf" ".otf" ".eot" ".woff" ".woff2"
    ".class" ".jar" ".o" ".obj" ".lib"
    ".doc" ".docx" ".ppt" ".pptx" ".xls" ".xlsx"
    ".apk" ".ipa" ".dmg" ".deb" ".rpm"
    ".psd" ".xcf")
  "Disallowed file extensions for dired prompts."
  :type '(repeat string)
  :group 'genai)

(defcustom genai-blacklist-expressions
  '("RUNNING" "FINISHED" "2024Y" "2025Y")
  "Regexp or lambda to exclude files.")

(defcustom genai-buffer-name
  genai-buffer-name
  "")

(defcustom genai-buffer-name-history
  genai-buffer-name-history
  "")


(provide 'genai-variables)

(when
    (not load-file-name)
  (message "genai-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))