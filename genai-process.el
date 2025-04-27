;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 15:44:52>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-process.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; process.el --- Process and spinner

(require 'genai-variables)
(require 'genai-core)
(require 'async)
(require 'genai-variables)
(require 'genai-spinner)
(require 'ansi-color)

(defun genai--init ()
  "Initialize GenAI (check deps once)."
  (unless genai-dependencies-checked
    (genai--check-python-dependencies)
    (setq genai-dependencies-checked t)))

(defun genai--ensure-dependencies ()
  "Ensure deps before run."
  (genai--init))

;; (defun genai--check-python-dependencies ()
;;   "Install or update mngs if needed."
;;   (let ((ver "1.9.8")
;;         (cmd (format
;;               "%s -c 'import pkg_resources; pkg_resources.require(\"mngs==%s\")'"
;;               genai-python-bin-path ver)))
;;     (async-start
;;      `(lambda () (shell-command-to-string ,cmd))
;;      (lambda (res)
;;        (when
;;            (string-match-p "DistributionNotFound\\|VersionConflict"
;;                            res)
;;          (when (yes-or-no-p (format "Install/upgrade mngs>=%s?" ver))
;;            (async-start
;;             `(lambda ()
;;                (shell-command-to-string
;;                 ,(format "%s -m pip install 'mngs>=%s'"
;;                          genai-python-bin-path ver)))
;;             (lambda (_) (message "mngs installed.")))))))))

(cl-defun genai--check-python-dependencies
    ()
  "Check if python mngs package is installed."
  (let*
      ((mngs-version "1.9.8")
       (check-command
        (format
         "%s -c 'import pkg_resources; pkg_resources.require(\"mngs==%s\")'"
         genai-python-bin-path mngs-version)))
    (async-start
     `(lambda
        ()
        (shell-command-to-string ,check-command))
     (lambda
       (result)
       (if
           (string-match "DistributionNotFound\\|VersionConflict"
                         result)
           (when
               (yes-or-no-p
                (format
                 "The required Python package 'mngs>=%s' is not installed or outdated for %s. Install/upgrade it now?"
                 mngs-version genai-python-bin-path))
             (async-start
              `(lambda
                 ()
                 (shell-command-to-string
                  ,(format "%s -m pip install 'mngs>=%s'"
                           genai-python-bin-path mngs-version)))
              (lambda
                (_)
                (message "Package installed/upgraded.")))))))))

(defun genai--safe-shell-quote-argument (arg)
  "Quote ARG if non-empty."
  (if (and arg (not (string-empty-p arg)))
      (shell-quote-argument arg)
    ""))

(cl-defun genai--insert-prompt-template-type-and-engine
    (prompt template-type)
  "Insert prompt, template type, and engine into *GenAI* buffer.
PROMPT is the user's input, TEMPLATE-TYPE is the selected template."
  (with-current-buffer
      (get-buffer-create "*GenAI*")
    (goto-char
     (point-max))
    (insert "\n\n")
    (insert genai--splitter)
    (insert "\n\n")
    (insert "| ")
    (insert "YOU")
    (insert "\n\n")
    (insert "| ")
    (insert template-type)
    (insert "\n\n")
    (insert "| ")
    (insert prompt)
    (insert "\n\n")
    (insert genai--splitter)
    (insert "\n\n")
    (insert "| ")
    (insert
     (upcase genai-engine))
    (insert " ")
    (insert "\n\n")
    (goto-char
     (point-max))
    (run-at-time "0 sec" nil #'genai--scroll)))

(defun genai--construct-python-command
    (prompt)
  "Construct complete command string..."
  (genai--parse-api-keys)
  (let*
      ((template-type
        (genai--select-template))
       (tmp-prompt-file
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

      (genai--insert-prompt-template-type-and-engine prompt
                                                     template-type)
      command)))

(defun genai--process-sentinel (proc msg)
  "Handle PROC sentinel MSG."
  (genai-interactive-mode -1)
  (when (string-match-p "finished\\|exited" msg)
    (genai--clean-up-output))
  (genai--stop-spinner)
  (when (string-match-p "error" msg)
    (genai--stop-spinner)
    (message "Error: %s" msg)))

(defun genai--process-filter (proc output)
  "Insert PROC OUTPUT and remove ANSI escapes immediately."
  (with-current-buffer (process-buffer proc)
    (let ((start (point-max)))
      (goto-char start)
      (insert output)
      (ansi-color-apply-on-region start (point-max)))))

;; (defun genai--start-python-process (prompt)
;;   "Start GenAI with PROMPT."
;;   (when (process-live-p genai--process)
;;     (delete-process genai--process))
;;   (let* ((cmd (genai--construct-python-command prompt))
;;          (proc
;;           (start-process-shell-command "genai" genai-buffer-name cmd)))
;;     (setq genai--process proc)
;;     (set-process-sentinel proc #'genai--process-sentinel)
;;     (genai--start-spinner)))

(defun genai--start-python-process (prompt)
  "Start GenAI with PROMPT."
  (when (process-live-p genai--process)
    (delete-process genai--process))
  (let* ((cmd (genai--construct-python-command prompt))
         (proc
          (start-process-shell-command "genai" genai-buffer-name cmd)))
    (setq genai--process proc)
    (set-process-filter proc #'genai--process-filter)
    (set-process-sentinel proc #'genai--process-sentinel)
    (genai--start-spinner)))

(defun genai--clean-up-output
    ()
  "Remove ANSI escape codes and specific unwanted messages from the *GenAI* buffer."
  (with-current-buffer
      (get-buffer-create genai-buffer-name)
    (genai--stop-spinner)))
;; (ansi-color-apply-on-region
;;  (point-min)
;;  (point-max))
;; (save-excursion
;;   (goto-char
;;    (point-min))
;;   (while
;;       (search-forward-regexp "\x1b\\[\\([0-9;]*\\)m" nil t)
;;     (replace-match "")))
;; (genai--trim-buffer)))


(provide 'genai-process)

(when
    (not load-file-name)
  (message "genai-process.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))