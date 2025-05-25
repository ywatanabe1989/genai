;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-28 15:39:58>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-mode.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'markdown-mode)
(require 'cl-lib)

(defvar genai-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for genai-mode.")

(define-derived-mode genai-mode markdown-mode
  "GenAI"
  "Major mode for GenAI buffer.")

(define-minor-mode genai-interactive-mode
  "Temporary interactive GenAI mode."
  :lighter " GenAI"
  :keymap (make-sparse-keymap))

(defadvice keyboard-quit
    (before genai-disable-interactive-mode activate)
  "Disable genai-interactive-mode on C-g."
  (when genai-interactive-mode
    (genai-interactive-mode -1)))

;;;###autoload
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

;; keybindings in genai-mode

(define-key genai-mode-map (kbd "M-n") 'genai-next-code-block)

(define-key genai-mode-map (kbd "M-p") 'genai-previous-code-block)

(define-key genai-mode-map (kbd "C-c C-c") 'genai-send-buffer-input)


(provide 'genai-mode)

(when
    (not load-file-name)
  (message "genai-mode.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
