;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-28 15:40:57>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'genai-mode)
(require 'genai-variables)

;;;###autoload
(defun genai-on-region ()
  "Run GenAI on region, dired or prompt."
  (interactive)
  (genai-interactive-mode 1)
  (let* ((marked-files
          (and (eq major-mode 'dired-mode)
               (condition-case nil
                  (dired-get-marked-files nil nil)
                  (user-error nil))))
         (prompt-text
          (cond
           ((< 1 (length marked-files))
            (genai-interactive-mode -1)
            (genai-on-region-list-files)
            nil)  ; Return nil to signal we're handling it separately
           ((use-region-p)
            (prog1
                (buffer-substring-no-properties
                 (region-beginning) (region-end))
              (deactivate-mark)))
           (t
            (read-string "Enter prompt: " "")))))
    
    ;; Special shortcut cases - handle before template selection
    (cond
     ((equal prompt-text "g")
      (genai-interactive-mode -1)
      (switch-to-buffer-other-window genai-buffer-name)
      (keyboard-quit) 
      (message "Jumped to *GenAI*"))
     ((equal prompt-text "h")
      (genai-interactive-mode -1)
      (genai-show-history) 
      (keyboard-quit)
      (message "Showing history"))
     ;; Continue with template selection for normal prompts
     ((and prompt-text
           (setq template-type (genai--select-template)))
      (genai-interactive-mode -1)
      (genai--ensure-dependencies)
      (genai--run-with-template prompt-text template-type)))))

;; (defun genai-on-region ()
;;   "Run GenAI on region, dired or prompt."
;;   (interactive)
;;   (genai-interactive-mode 1)
;;   (let* ((marked-files
;;           (and (eq major-mode 'dired-mode)
;;                (condition-case nil
;;                   (dired-get-marked-files nil nil)
;;                   (user-error nil))))
;;          (prompt-text
;;           (cond
;;            ((< 1 (length marked-files))
;;             (genai-interactive-mode -1)
;;             (genai-on-region-list-files)
;;             nil)  ; Return nil to signal we're handling it separately
;;            ((use-region-p)
;;             (prog1
;;                 (buffer-substring-no-properties
;;                  (region-beginning) (region-end))
;;               (deactivate-mark)))
;;            (t
;;             (read-string "Enter prompt: " ""))))
;;          (template-type
;;           (when prompt-text
;;             (genai--select-template))))
    
;;     ;; Only run if we have both text and template
;;     (when (and prompt-text template-type)
;;       (genai-interactive-mode -1)
;;       ;; Now do the dependency check after template selection
;;       (genai--ensure-dependencies)
;;       (genai--run-with-template prompt-text template-type))))

;; ;;;###autoload
;; (defun genai-on-region ()
;;   "Run GenAI on region, dired or prompt."
;;   (interactive)
;;   (genai--init)
;;   (genai--ensure-dependencies)
;;   (genai-interactive-mode 1)
;;   (let ((marked-files
;;          (and (eq major-mode 'dired-mode)
;;               (condition-case nil
;;                   (dired-get-marked-files nil nil)
;;                 (user-error nil)))))
;;     (cond
;;      ((< 1 (length marked-files))
;;       (genai-interactive-mode -1)
;;       (genai-on-region-list-files))
;;      ((use-region-p)
;;       (let ((text
;;              (buffer-substring-no-properties
;;               (region-beginning) (region-end))))
;;         (genai-interactive-mode -1)
;;         (deactivate-mark)
;;         (genai--run text)))
;;      (t
;;       (let ((input (read-string "Enter prompt: " "")))
;;         (genai-interactive-mode -1)
;;         (genai--run input))))))

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


(defun genai--run-with-template (prompt template-type)
  "Run GenAI with PROMPT and specified TEMPLATE-TYPE."
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
      (genai--start-python-process-with-template prompt template-type)))))

(defun genai--start-python-process-with-template (prompt template-type)
  "Start GenAI with PROMPT and TEMPLATE-TYPE."
  (when (process-live-p genai--process)
    (delete-process genai--process))
  (let* ((cmd (genai--construct-python-command-with-template prompt template-type))
         (proc
          (start-process-shell-command "genai" genai-buffer-name cmd)))
    (setq genai--process proc)
    (set-process-filter proc #'genai--process-filter)
    (set-process-sentinel proc #'genai--process-sentinel)
    (genai--start-spinner)))

(defun genai--construct-python-command-with-template (prompt template-type)
  "Construct command with PROMPT and TEMPLATE-TYPE."
  (genai--parse-api-keys)
  (let ((tmp-prompt-file
         (make-temp-file "genai-prompt-" nil ".txt"))
        (command nil))
    (unwind-protect
        (progn
          ;; write prompt to temp file
          (with-temp-file tmp-prompt-file
            (insert prompt))
          (setq command
                (concat
                 (genai--safe-shell-quote-argument
                  genai-python-bin-path)
                 " "
                 (genai--safe-shell-quote-argument
                  genai-python-script-path)
                 " "
                 (mapconcat
                  (lambda
                      (api-key)
                    (concat "--api_key "
                            (genai--safe-shell-quote-argument api-key)))
                  genai-api-keys-parsed " ")
                 " "
                 "--engine "
                 (genai--safe-shell-quote-argument genai-engine)
                 " "
                 "--max_tokens "
                 (genai--safe-shell-quote-argument genai-max-tokens)
                 " "
                 "--temperature "
                 (genai--safe-shell-quote-argument genai-temperature)
                 " "
                 "--human_history_path "
                 (genai--safe-shell-quote-argument
                  genai-history-human-path)
                 " "
                 "--n_history "
                 (genai--safe-shell-quote-argument genai-n-history)
                 " "
                 "--template_type "
                 (genai--safe-shell-quote-argument template-type)
                 " "
                 "--prompt_file "
                 (genai--safe-shell-quote-argument tmp-prompt-file))))
          (genai--insert-prompt-template-type-and-engine prompt template-type)
          command)))

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

;;;###autoload
(defun genai-next-code-block
    ()
  "Navigate to the next code block and select the content"
  (interactive)
  ;; (with-current-buffer genai-buffer-name
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
    (deactivate-mark)))
;; )

;;;###autoload
(defun genai-previous-code-block
    ()
  "Navigate to the previous code block and select the content"
  (interactive)
  ;; (with-current-buffer genai-buffer-name
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
        (deactivate-mark)))))
                                        ;)


(provide 'genai-core)

(when
    (not load-file-name)
  (message "genai-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
