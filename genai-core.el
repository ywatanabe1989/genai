;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 15:48:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; core.el --- Core commands

(require 'genai-mode)
(require 'genai-variables)

;;;###autoload

(defun genai-on-region ()
  "Run GenAI on region, dired or prompt."
  (interactive)
  (genai--init)
  (genai--ensure-dependencies)
  (genai-interactive-mode 1)
  (let ((marked-files
         (and (eq major-mode 'dired-mode)
              (condition-case nil
                  (dired-get-marked-files nil nil)
                (user-error nil)))))
    (cond
     ((< 1 (length marked-files))
      (genai-interactive-mode -1)
      (genai-on-region-list-files))
     ((use-region-p)
      (let ((text
             (buffer-substring-no-properties
              (region-beginning) (region-end))))
        (genai-interactive-mode -1)
        (deactivate-mark)
        (genai--run text)))
     (t
      (let ((input (read-string "Enter prompt: " "")))
        (genai-interactive-mode -1)
        (genai--run input))))))

(defun genai--run (prompt)
  "Dispatch PROMPT to process or history commands."
  (genai--history-reset-if-large)
  (cond
   ((equal prompt "g")
    (switch-to-buffer-other-window genai-buffer-name)
    (keyboard-quit) (message "Jumped to *GenAI*"))
   ((equal prompt "h")
    (genai-show-history) (keyboard-quit)
    (message "Showing history"))
   (t
    (with-current-buffer (get-buffer-create genai-buffer-name)
      (genai-mode)
      (font-lock-ensure)
      (genai--start-python-process prompt)))))

(defun genai--scroll ()
  "Scroll *GenAI* to most recent splitter."
  (when-let ((buf (get-buffer genai-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (when (re-search-backward genai--splitter nil t)
        (beginning-of-line))
      (when-let ((win (get-buffer-window buf)))
        (set-window-point win (point))
        (with-selected-window win (recenter 0))))))

;;;###autoload

(defun genai-copy-last ()
  "Copy last LLM output."
  (interactive)
  (with-current-buffer genai-buffer-name
    (goto-char (point-max))
    (push-mark (point) t)
    (goto-char (point-min))
    (kill-ring-save (region-beginning) (region-end))))

;;;###autoload

(defun genai-copy-code-blocks ()
  "Copy code blocks from last output."
  (interactive)
  (with-current-buffer genai-buffer-name
    (goto-char (point-max))
    (when (search-backward genai--splitter nil t)
      (let (blocks start end)
        (while
            (re-search-forward genai--code-block-start-delimiter nil t)
          (forward-line 1)
          (setq start (point))
          (when
              (re-search-forward genai--code-block-end-delimiter nil t)
            (setq end (match-beginning 0))
            (push (buffer-substring-no-properties start end) blocks)))
        (dolist (blk (reverse blocks))
          (kill-new blk))
        (message "Copied %d blocks" (length blocks))))))

;; (defun genai-next-code-block ()
;;   "Navigate to and copy next code block."
;;   (interactive)
;;   (with-current-buffer genai-buffer-name
;;     (end-of-line)
;;     (when (looking-at-p genai--code-block-end-delimiter)
;;       (forward-line) (beginning-of-line))
;;     (when (re-search-forward genai--code-block-start-delimiter nil t)
;;       (let ((start (point)))
;;         (re-search-forward genai--code-block-end-delimiter nil t)
;;         (kill-ring-save start (match-beginning 0))
;;         (message "Copied code block")))))

;; (defun genai-previous-code-block ()
;;   "Navigate to and copy previous code block."
;;   (interactive)
;;   (with-current-buffer genai-buffer-name
;;     (when (re-search-backward genai--code-block-end-delimiter nil t)
;;       (let ((end (point)))
;;         (when
;;             (re-search-backward genai--code-block-start-delimiter nil
;;                                 t)
;;           (forward-line 1)
;;           (kill-ring-save (point) end)
;;           (message "Copied code block"))))))

(defun genai-next-code-block
    ()
  "Navigate to the next code block and select the content"
  (interactive)
  (with-current-buffer genai-buffer-name
    (end-of-line)
    (when
        (looking-at-p genai--code-block-end-delimiter)
      (forward-line)
      (beginning-of-line))
    (when
        (re-search-forward genai--code-block-start-delimiter nil t)
      (let
          ((start
            (point)))
        (when
            (re-search-forward genai--code-block-end-delimiter nil t)
          (let
              ((end
                (match-beginning 0)))
            (goto-char end)
            (set-mark
             (1+ start))
            (activate-mark)
            (kill-ring-save start end)
            (pulse-momentary-highlight-region
             (1+ start)
             end)
            (message "Copied."))))
      (deactivate-mark))))

;;;###autoload

(defun genai-previous-code-block
    ()
  "Navigate to the previous code block and select the content"
  (interactive)
  (with-current-buffer genai-buffer-name

    ;; Find the previous code block end delimiter
    (when
        (re-search-backward genai--code-block-end-delimiter nil t)
      (backward-char)
      (let
          ((end
            (point)))

        ;; Find the previous code block start delimiter
        (if
            (re-search-backward genai--code-block-start-delimiter nil
                                t)
            (progn
              (forward-line 1)
              (set-mark end)
              (activate-mark)
              (kill-ring-save
               (point)
               end)
              (pulse-momentary-highlight-region
               (+
                (point)
                1)
               end)
              (message "Copied."))
          (deactivate-mark))))))


(provide 'genai-core)

(when
    (not load-file-name)
  (message "genai-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))