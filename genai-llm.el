;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 14:27:13>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-llm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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

(defun genai--parse-api-keys
    ()
  "Parse API keys from `genai-api-keys` and store them in `genai-api-keys-parsed`."
  (unless
      (string-empty-p genai-api-keys)
    (let
        ((keys-list
          (split-string genai-api-keys ":")))
      (setq genai-api-keys-parsed keys-list))))

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


(provide 'genai-llm)

(when
    (not load-file-name)
  (message "genai-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))