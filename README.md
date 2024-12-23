<!-- ---
!-- title: 2024-12-23 14:33:19
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/README.md
!-- --- -->

# GenAI.el

An Emacs interface for interacting with Large Language Models (LLMs) like ChatGPT, Gemini, Claude, and Llama.

## Features

- Send queries from selected region, selected file in dired mode, or manual input 
- Customizable prompt templates for prompting
- Code block navigation and auto-copying
- Conversation history tracking 
- Streaming response display
- Markdown formatting support


## Installation
```bash
EMACS_GENAI_DIR=$HOME/.emacs.d/lisp/emacs-genai/ # Adjust this
git clone git@github.com:ywatanabe1989/emacs-genai.git $EMACS_GENAI_DIR
```

## Demo
![Demo](docs/demo-1920.gif)

## Dependencies
```bash
pip install "mngs==1.9.8"
```

## Functions
- `M-x genai-on-region`: Process selected text or selected files in the dired mode as prompt for LLM
- `M-x genai-show-history`: Display conversation history
- `M-x genai-backup-history`: Backup and reset conversation history


## Configurations
```elisp
;;; Basic Setup
(add-to-list 'load-path (getenv "EMACS_GENAI_DIR")) ; "/home/ywatanabe/.emacs.d/lisp/genai/"
(require 'genai)

;;; Model Configuration
;; API and Engine (Claude, OpenAI, Gemini, and Perplexity models are availabel)
(setq genai-api-key (getenv "GENAI_API_KEY")) ; Your API Key for LLM provider (e.g., "sk-OS****brr7" = OPENAI_API_KEY)
(setq genai-engine (getenv "GENAI_ENGINE"))   ; LLM name (e.g., "gpt-4o")

;;; Key Binding
(define-key global-map (kbd "C-M-g") 'genai-on-region)

;;; Optional
;; (setq genai-template-mapping
;;       '(("p" . "Program")
;;         ("e" . "Email")
;;         ("c" . "Correct")
;;         ("my" . "MyAwesomeTemplate")))
        
;; (setq genai-n-history "5")
;; (setq genai-max-tokens "2000")
;; (setq genai-temperature "0")

;; (setq genai-home-dir (getenv "EMACS_GENAI_DIR"))
;; (setq genai-python-bin-path 
;;       (concat (getenv "HOME") "/proj/env-3.11/bin/python3"))
;; (setq genai-python-script-path 
;;       (concat (getenv "EMACS_GENAI_DIR") "genai.py"))
;; (setq genai-human-history-path
;;       (concat (getenv "EMACS_GENAI_DIR") "history-human-secret.json"))
```

## Templates
Place templates in `$EMACS_GENAI_DIR/templates/*.md`. File name, the first uppercase letter, becomes the shortcut key in default. Use "PLACEHOLDER" to control manual prompt placement. Examples are available under ['./templates'](./templates).

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)