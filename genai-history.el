;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 15:46:51>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-history.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; history.el --- GenAI history commands

(require 'genai-variables)
(require 'genai-core)

;;;###autoload

(defun genai-show-history ()
  "Open human-readable history."
  (interactive)
  (find-file-read-only genai-history-human-readable-path)
  (genai-mode))

;;;###autoload

(defun genai-reset-history ()
  "Backup and reset JSON and human-readable history."
  (interactive)
  (let* ((backup-dir (concat genai-home-dir "histories/"))
         (timestamp (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (human-json-bkp
          (concat backup-dir "history-human-" timestamp ".json"))
         (human-md-bkp
          (concat backup-dir "history-human-readable-" timestamp ".md"))
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

(defun genai--history-reset-if-large ()
  "Backup and reset history files when size >1MB."
  (dolist (entry
           (list
            (cons genai-history-human-path "human")
            (cons genai-history-human-readable-path "human-readable")
            (cons genai-history-ai-path "ai")))
    (let ((file-path (car entry)))
      (when (and (file-exists-p file-path)
                 (> (nth 7 (file-attributes file-path))
                    (* 1 1024 1024)))
        (genai-reset-history)))))


(provide 'genai-history)

(when
    (not load-file-name)
  (message "genai-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))