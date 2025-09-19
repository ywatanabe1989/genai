;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-16 18:10:27>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-history.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; history.el --- GenAI history commands

(require 'genai-variables)
(require 'genai-core)

(defun genai--read-n-history ()
  "Interactively read n_history value."
  (let
      ((input
        (read-string
         (format "Number of history entries (default %s): "
                 genai-n-history))))
    (if (string-match-p "^[0-9]+$" input)
        input
      genai-n-history)))

;;;###autoload

;; (defun genai-show-history ()
;;   "Open human-readable history."
;;   (interactive)
;;   (find-file-read-only genai-history-human-readable-path)
;;   (genai-mode))

(defun genai-show-history ()
  "Display human-readable history in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create genai-buffer-name-history)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (file-exists-p genai-history-human-readable-path)
            (insert-file-contents genai-history-human-readable-path)
          (insert "No history file found."))
        (genai-mode)
        (setq buffer-read-only t)
        (goto-char (point-max))))
    (display-buffer buffer
                    '((display-buffer-reuse-window
                       display-buffer-pop-up-window)
                      (window-height . 0.4)
                      (reusable-frames . visible)))))
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

;; (defun genai--history-reset-if-large ()
;;   "Backup and reset history files when size >1MB."
;;   (dolist (entry
;;            (list
;;             (cons genai-history-human-path "human")
;;             (cons genai-history-human-readable-path "human-readable")
;;             (cons genai-history-ai-path "ai")))
;;     (let ((file-path (car entry)))
;;       (when (and (file-exists-p file-path)
;;                  (> (nth 7 (file-attributes file-path))
;;                     (* 1 1024 1024)))
;;         (genai-reset-history)))))

(defun genai--history-cycle-if-large ()
  "Cycle history files when they exceed size limit, keeping last N entries."
  (let ((cycle-size 20))
                                        ; Keep last 20 exchanges (40 entries)
    (dolist (entry
             (list
              (cons genai-history-human-path "human")
              (cons genai-history-ai-path "ai")))
      (let ((file-path (car entry)))
        (when (and (file-exists-p file-path)
                   (> (nth 7 (file-attributes file-path))
                      (* 1 1024 1024)))
          (let* ((history (condition-case nil
                              (json-read-file file-path)
                            (error [])))
                 (history-list (append history nil))
                 (total-entries (length history-list))
                 (keep-entries (* cycle-size 2))  ; user + assistant pairs
                 (cycled-history (if (> total-entries keep-entries)
                                     (last history-list keep-entries)
                                   history-list)))
            (with-temp-file file-path
              (insert (json-encode cycled-history)))
            (message "Cycled %s: kept last %d entries"
                     (file-name-nondirectory file-path)
                     (length cycled-history))))))))


(provide 'genai-history)

(when
    (not load-file-name)
  (message "genai-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))