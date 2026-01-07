;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-30 18:25:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-dired.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(require 'dired)
(require 'genai-file-collector)
(require 'genai-variables)

(defgroup genai-dired nil
  "Copy file contents from dired for GenAI prompts."
  :group 'dired
  :prefix "genai-dired-")

(defcustom genai-dired-buffer-name "*GenAI Files*"
  "Buffer name for GenAI file selection."
  :type 'string
  :group 'genai-dired)

(defun genai-dired--copy-contents (selected-entries)
  "Copy contents of SELECTED-ENTRIES to kill ring."
  (let ((files-string ""))
    (dolist (entry selected-entries)
      (setq files-string
            (concat files-string
                    (format "\n\n;; File: %s\n" (cadr entry))
                    (with-temp-buffer
                      (insert-file-contents-literally (cadr entry))
                      (buffer-string)))))
    (kill-new files-string)
    (length (split-string files-string "\n" t))))

(defun genai-dired--handle-selection (selected-entries total-words)
  "Handle selected entries for GenAI."
  (when (y-or-n-p (format
                   "Send %d files with total %d words to GenAI? "
                   (length selected-entries) total-words))
    (let ((copied-lines (genai-dired--copy-contents selected-entries))
          (files-content (current-kill 0)))
      (kill-buffer (current-buffer))
      (genai-interactive-mode 1)
      (let ((template-type (genai--select-template))
            (n-history (genai--read-n-history)))
        (genai-interactive-mode -1)
        (genai--ensure-dependencies)
        (genai--run-with-template files-content template-type
                                  n-history))
      (message "Sent %d files (%d lines and %d words) to GenAI"
               (length selected-entries) copied-lines total-words))))

;;;###autoload

(defun genai-dired-copy-contents ()
  "Copy contents of marked files from visible dired buffers for GenAI prompts."
  (interactive)
  (let ((genai-fc-exclude-path-patterns
         (append genai-fc-exclude-path-patterns
                 genai-dired-filter-ignored-parents-patterns
                 genai-dired-filter-ignored-children-patterns))
        (genai-fc-safe-extensions
         genai-dired-filter-allowed-extensions)
        (genai-fc-skip-extensions
         genai-dired-filter-ignored-extensions)
        (genai-fc-min-file-size genai-dired-filter-file-size-min)
        (genai-fc-max-file-size genai-dired-filter-file-size-max))
    (genai-fc-collect-and-display genai-dired-buffer-name
                                  #'genai-dired--handle-selection)))

(defun genai-on-region-list-files ()
  "Call genai-dired-copy-contents when multiple files are marked."
  (genai-dired-copy-contents))


(provide 'genai-dired)

(when
    (not load-file-name)
  (message "genai-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))