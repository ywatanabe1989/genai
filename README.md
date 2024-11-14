<!-- ---
!-- title: ./genai/README.md
!-- author: ywatanabe
!-- date: 2024-11-14 18:18:28
!-- --- -->


# Emacs client for LLM

## Installation
```bash
EMACS_GENAI_DIR=$HOME/.emacs.d/lisp/emacs-genai/
git clone git@github.com:ywatanabe1989/emacs-genai.git $EMACS_GENAI_DIR
```

## Demo
![Demo](docs/demo-1920.gif)

## Dependencies
```bash
pip install mngs
```

## Functions
- `M-x genai-on-region`: Process selected text as prompt for LLM
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
Place templates in `$HOME/.emacs.d/lisp/genai/templates/*.md`. First uppercase letter becomes the shortcut key. Use "PLACEHOLDER" to mark input position. Examples are available under ['./templates'](./templates).

## Contact
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
