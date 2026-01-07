;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-30 18:25:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(require 'genai-variables)
(require 'genai-mode)
(require 'genai-core)
(require 'genai-process)
(require 'genai-templates)
(require 'genai-dired)
(require 'genai-history)
(require 'genai-llm)
(require 'genai-spinner)


(provide 'genai)

(when
    (not load-file-name)
  (message "genai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))