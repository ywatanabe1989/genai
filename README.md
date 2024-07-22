# Emacs client for LLM

## Installation
```bash
EMACS_GENAI_DIR=$HOME/.emacs.d/lisp/emacs-genai/
# echo "export EMACS_GENAI_DIR=$EMACS_GENAI_DIR" >> ~/.bashrc
git clone git@github.com:ywatanabe1989/emacs-genai.git $EMACS_GENAI_DIR
```

## Demo
![Demo](docs/demo-1920.gif)

## Python Installation for mngs pip package
```bash
PROJ_DIR=$HOME/proj
cd $PROJ_DIR
sudo apt install python3.11 python3.11-dev python3.11-tk python3.11-venv
python3.11 -m venv env-3.11 && source env-3.11/bin/activate && pip install -U pip && pip install "mngs>=1.5.5"
```

## Configurations
```elisp
(add-to-list 'load-path (getenv "EMACS_GENAI_DIR")) ; "/home/ywatanabe/.emacs.d/lisp/genai/"
(require 'genai)

;; Model
(setq genai-api-key (getenv "GENAI_API_KEY")) ; "sk-OS****brr7" = OPENAI_API_KEY
(setq genai-engine (getenv "GENAI_ENGINE")) ; "gpt-4o"
;;; You can use OpenAI, Gemini, Claude, or Perplexity models

;; PATH
(setq genai-home-dir (getenv "EMACS_GENAI_DIR")) ; "/home/ywatanabe/.emacs.d/lisp/genai/"
(setq genai-python-bin-path (concat (getenv "HOME") "/proj/env-3.11/bin/python3")) ; default /usr/bin/python3
(setq genai-python-script-path (concat (getenv "EMACS_GENAI_DIR") "genai.py"))
(setq genai-human-history-path (concat (getenv "EMACS_GENAI_DIR") "history-human-secret.json"))
(setq genai-n-history "5")
(setq genai-max-tokens "2000")
(setq genai-temperature "0")

;; Key Bindings
(define-key global-map (kbd "C-M-g") 'genai-on-region)
```

## Templates
Templates can be managed under the ['./templates'](./templates) directory (default: $HOME/.emacs.d/lisp/emacs-genai/templates/*.md). Uppercase letters in the template file name are used as shortcut keys for selection. Your input to GenAI (selected region) is inserted into the "PLACEHOLDER" of the template.

## TODO

