;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-15 13:05:29>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-dired.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; genai-dired.el --- Copy file contents from dired for GenAI prompts -*- lexical-binding: t -*-
;; Author: ywatanabe
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: dired, files, ai, genai
;; URL: https://github.com/ywatanabe/genai-dired
;;; Commentary:
;; This package provides functionality to copy contents of multiple files
;; from dired buffers, designed for feeding code to GenAI systems.
;;
;; Usage:
;; 1. Mark files in dired buffer(s)
;; 2. M-x genai-dired-copy-contents
;; 3. Edit the selection buffer if needed
;; 4. Press C-c C-c to copy to kill ring

;;; Code:
(require 'dired)
(require 'cl-lib)
(require 'genai-variables)

(defgroup genai-dired nil
  "Copy file contents from dired for GenAI prompts."
  :group 'dired
  :prefix "genai-dired-")

(defun genai-dired--get-file-hash (file)
  "Calculate SHA256 hash of FILE contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun genai-dired--get-exclusion-reason (file)
  "Return exclusion reason for FILE, or nil if file should be processed."
  (let* ((file-size (file-attribute-size (file-attributes file)))
         (extension (file-name-extension file t))
         (filename (file-name-nondirectory file))
         (filepath (directory-file-name file))
         (path-components (split-string filepath "/")))

    (cond
     ;; Must be a regular file
     ((not (file-regular-p file))
      "not a regular file")

     ;; File size constraints
     ((< file-size genai-dired-filter-file-size-min)
      (format "file too small (<%d bytes)"
              genai-dired-filter-file-size-min))
     ((> file-size genai-dired-filter-file-size-max)
      (format "file too large (>%dMB)"
              genai-dired-filter-file-size-max-mb))

     ;; Check allowed filename patterns (overrides other filters)
     ((and genai-dired-filter-allowed-filename-patterns
           (cl-some (lambda (pattern)
                      (string-match-p pattern filename))
                    genai-dired-filter-allowed-filename-patterns))
      nil)
                                        ; Allowed by filename override

     ;; Extension must be allowed
     ((not (member extension genai-dired-filter-allowed-extensions))
      (format "extension '%s' not allowed" extension))

     ;; Extension must not be ignored
     ((member extension genai-dired-filter-ignored-extensions)
      (format "extension '%s' ignored" extension))

     ;; Filename must not match ignored patterns
     ((cl-find-if (lambda (pattern)
                    (string-match-p pattern filename))
                  genai-dired-filter-ignored-filename-patterns)
      (format "filename matches ignored pattern: %s"
              (cl-find-if (lambda (pattern)
                            (string-match-p pattern filename))
                          genai-dired-filter-ignored-filename-patterns)))

     ;; No parent directory should match ignored patterns
     ((cl-find-if (lambda (pattern)
                    (cl-some (lambda (component)
                               (string-match-p pattern component))
                             path-components))
                  genai-dired-filter-ignored-parents-patterns)
      (format "parent directory matches ignored pattern: %s"
              (cl-find-if (lambda (pattern)
                            (cl-some (lambda (component)
                                       (string-match-p pattern
                                                       component))
                                     path-components))
                          genai-dired-filter-ignored-parents-patterns)))

     ;; No child directory should match ignored patterns
     ((cl-find-if (lambda (pattern)
                    (string-match-p pattern filepath))
                  genai-dired-filter-ignored-children-patterns)
      (format "child directory matches ignored pattern: %s"
              (cl-find-if (lambda (pattern)
                            (string-match-p pattern filepath))
                          genai-dired-filter-ignored-children-patterns)))

     ;; If there are allowed parent patterns, at least one must match
     ((and genai-dired-filter-allowed-parents-patterns
           (not (cl-some (lambda (pattern)
                           (cl-some (lambda (component)
                                      (string-match-p pattern
                                                      component))
                                    path-components))
                         genai-dired-filter-allowed-parents-patterns)))
      "parent directory doesn't match any allowed pattern")

     ;; If there are allowed child patterns, check them
     ((and genai-dired-filter-allowed-children-patterns
           (not (cl-some (lambda (pattern)
                           (string-match-p pattern filepath))
                         genai-dired-filter-allowed-children-patterns)))
      "child directory doesn't match any allowed pattern")

     ;; File should be processed
     (t nil))))

(defun genai-dired--should-process-file-p (file)
  "Return t if FILE should be processed based on hierarchical filtering criteria."
  (null (genai-dired--get-exclusion-reason file)))

(defun genai-dired--count-words-in-file (file)
  "Count words in FILE, handling TRAMP files."
  (message "DEBUG: Counting words in file: %s" file)
  (if (file-remote-p file)
      (progn
        (message "DEBUG: Remote file detected: %s" file)
        (let ((host (file-remote-p file 'host))
              (localname (file-remote-p file 'localname)))
          (message "DEBUG: SSH host: %s, localname: %s" host localname)
          (let
              ((cmd
                (format "ssh %s 'wc -w < %s'" host
                        (shell-quote-argument localname))))
            (message "DEBUG: SSH command: %s" cmd)
            (let ((result (shell-command-to-string cmd)))
              (message "DEBUG: SSH result: %s" (string-trim result))
              (string-to-number result)))))
    (progn
      (message "DEBUG: Local file: %s" file)
      (let ((cmd (format "wc -w < %s" (shell-quote-argument file))))
        (message "DEBUG: Local command: %s" cmd)
        (let ((result (shell-command-to-string cmd)))
          (message "DEBUG: Local result: %s" (string-trim result))
          (string-to-number result))))))

;; ;; 10 batch size?
;; (defun genai-dired--count-words-batch (files)
;;   "Count words in FILES using parallel wc command."
;;   (message "DEBUG: Starting word count for %d files" (length files))
;;   (let ((remote-files (cl-remove-if-not #'file-remote-p files))
;;         (local-files (cl-remove-if #'file-remote-p files))
;;         (word-counts (make-hash-table :test 'equal)))

;;     (message "DEBUG: Found %d local files, %d remote files" (length local-files) (length remote-files))

;;     (when local-files
;;       (message "DEBUG: Processing local files...")
;;       (let* ((batch-size 50)
;;              (batches (cl-loop for ii from 0 below (length local-files) by batch-size
;;                                collect (cl-subseq local-files ii (min (+ ii batch-size) (length local-files))))))
;;         (message "DEBUG: Split into %d batches" (length batches))
;;         (dolist (batch batches)
;;           (message "DEBUG: Processing batch of %d files" (length batch))
;;           (let* ((quoted-files (mapcar #'shell-quote-argument batch))
;;                  (cmd (format "printf '%%s\\0' %s | xargs -0 -P %d wc -w"
;;                               (mapconcat #'identity quoted-files " ")
;;                               (min 8 (length batch))))
;;                  (output (shell-command-to-string cmd))
;;                  (lines (split-string output "\n" t)))
;;             (message "DEBUG: Command executed, got %d lines of output" (length lines))
;;             (dotimes (idx (length batch))
;;               (let* ((line (nth idx lines))
;;                      (count (if (and line (string-match "^[[:space:]]*\\([0-9]+\\)" line))
;;                                 (string-to-number (match-string 1 line))
;;                               0)))
;;                 (puthash (nth idx batch) count word-counts)))
;;             (message "DEBUG: Batch completed")))))

;;     (when remote-files
;;       (message "DEBUG: Processing remote files...")
;;       (let* ((host (file-remote-p (car remote-files) 'host))
;;              (remote-batch-size 10)
;;              (remote-batches (cl-loop for ii from 0 below (length remote-files) by remote-batch-size
;;                                       collect (cl-subseq remote-files ii (min (+ ii remote-batch-size) (length remote-files))))))
;;         (message "DEBUG: Split remote files into %d batches" (length remote-batches))
;;         (dolist (batch remote-batches)
;;           (message "DEBUG: Processing remote batch of %d files" (length batch))
;;           (let* ((localnames (mapcar (lambda (f) (file-remote-p f 'localname)) batch))
;;                  (quoted-names (mapcar #'shell-quote-argument localnames))
;;                  (cmd (format "timeout 30 ssh %s 'printf \"%%s\\0\" %s | xargs -0 -P 2 wc -w'"
;;                               host (mapconcat #'identity quoted-names " ")))
;;                  (output (shell-command-to-string cmd))
;;                  (lines (split-string output "\n" t)))
;;             (message "DEBUG: Remote batch command executed, got %d lines" (length lines))
;;             (dotimes (idx (length batch))
;;               (let* ((line (nth idx lines))
;;                      (count (if (and line (string-match "^[[:space:]]*\\([0-9]+\\)" line))
;;                                 (string-to-number (match-string 1 line))
;;                               0)))
;;                 (puthash (nth idx batch) count word-counts)))
;;             (message "DEBUG: Remote batch completed")))))

;;     (message "DEBUG: Word counting completed, returning hash table")
;;     word-counts))

;; (defun genai-dired--count-words-batch (files)
;;   "Count words in FILES using file size estimation."
;;   (let ((word-counts (make-hash-table :test 'equal)))
;;     (dolist (file files)
;;       (let* ((size (file-attribute-size (file-attributes file)))
;;              (estimated-words (if size (/ size 6) 50)))
;;         (puthash file estimated-words word-counts)))
;;     word-counts))

(defun genai-dired--count-words-batch (files)
  "Count words/tokens in FILES using file size estimation.
For programming files, estimates tokens; for text files, estimates words."
  (let ((word-counts (make-hash-table :test 'equal)))
    (dolist (file files)
      (let* ((size (file-attribute-size (file-attributes file)))
             (extension (file-name-extension file))
             (is-code-file (member extension
                                   '("el" "lisp" "py" "js" "ts" "jsx"
                                     "tsx"
                                     "c" "cpp" "h" "hpp" "java" "rs"
                                     "go"
                                     "rb" "php" "swift" "kt" "scala"
                                     "clj"
                                     "sh" "bash" "zsh" "fish" "ps1"
                                     "html" "css" "scss" "sql" "r" "m")))
             (estimated-words
              (if size
                  (if is-code-file
                      ;; For code: ~0.3 tokens per byte (or divide by 3.3)
                      (/ size 13)
                    ;; For text: ~1 word per 6 bytes
                    (/ size 6))
                ;; Fallback for empty/inaccessible files
                (if is-code-file 100 50))))
        (puthash file estimated-words word-counts)))
    word-counts))

(defun genai-dired--collect-files-recursive
    (file processed-files content-hash)
  "Recursively collect files, separating valid and skipped files."
  (let* ((real-file (file-truename file))
         valid-files
         skipped-files)
    (unless (gethash real-file processed-files)
      (puthash real-file t processed-files)
      (cond
       ((file-directory-p real-file)
        (let ((exclusion-reason
               (cl-find-if (lambda (pattern)
                             (string-match-p pattern real-file))
                           genai-dired-filter-ignored-parents-patterns)))
          (if exclusion-reason
              (push (cons real-file (format "directory matches pattern: %s" exclusion-reason))
                    skipped-files)
            (dolist (f (directory-files real-file t "^[^.]"))
              (let ((result
                     (genai-dired--collect-files-recursive f
                                                           processed-files
                                                           content-hash)))
                (setq valid-files (append valid-files (car result)))
                (setq skipped-files (append skipped-files (cdr result))))))))
       ((file-regular-p real-file)
        (let ((exclusion-reason (genai-dired--get-exclusion-reason real-file)))
          (if exclusion-reason
              (push (cons real-file exclusion-reason) skipped-files)
            (unless (gethash real-file content-hash)
              (puthash real-file t content-hash)
              (push real-file valid-files)))))))
    (cons valid-files skipped-files)))

(defun genai-dired--get-selected-entries (candidates all-skipped)
  "Get selected entries from current buffer."
  (let ((selected-files '())
        (total-words 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (is-comment (string-prefix-p ";; " line))
               (clean-line (if is-comment (substring line 3) line))
               (entry (or (assoc clean-line candidates)
                          (assoc line candidates))))
          (when (and entry
                     (not is-comment)
                     (not (string= "" (string-trim line))))
            (cl-incf total-words (cddr entry))
            (push entry selected-files)))
        (forward-line 1)))
    (cons selected-files total-words)))

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

;;;###autoload

(defun genai-dired-copy-contents ()
  "Copy contents of marked files from visible dired buffers for GenAI prompts."
  (interactive)
  (let* ((all-files '())
         (processed-files (make-hash-table :test 'equal))
         (content-hash (make-hash-table :test 'equal))
         (all-valid-files '())
         (all-skipped-files '())
         (buf (get-buffer-create genai-dired-buffer-name)))
    (message "DEBUG: Starting genai-dired-copy-contents")
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (eq (buffer-local-value 'major-mode buffer) 'dired-mode)
          (with-current-buffer buffer
            (let
                ((marked-files
                  (dired-get-marked-files nil nil nil nil)))
              (setq all-files (append all-files marked-files)))))))
    (message "DEBUG: Found %d marked files" (length all-files))
    (if (null all-files)
        (message "No files marked in visible dired buffers")
      (let ((default-directory (file-truename default-directory)))
        (message "DEBUG: Collecting files...")
        (dolist (file all-files)
          (message "DEBUG: Processing file: %s" file)
          (let
              ((result
                (genai-dired--collect-files-recursive file
                                                      processed-files
                                                      content-hash)))
            (setq all-valid-files
                  (append all-valid-files (car result)))
            (setq all-skipped-files
                  (append all-skipped-files (cdr result)))))
        (message "DEBUG: Collection complete. Valid: %d, Skipped: %d"
                 (length all-valid-files) (length all-skipped-files))

        (let ((candidates '())
              (all-skipped '()))
          (when all-valid-files
            (message
             "DEBUG: Starting word count for %d valid files..."
             (length all-valid-files))
            (let
                ((word-counts
                  (genai-dired--count-words-batch all-valid-files)))
              (message
               "DEBUG: Word counting finished, building candidates list")
              (dolist (file all-valid-files)
                (let* ((word-count (gethash file word-counts 0))
                       (display-name (file-relative-name file))
                       (entry-data (cons file word-count)))
                  (push
                   (cons
                    (format "%s (~%d words)" display-name word-count)
                    entry-data)
                   candidates)))
              (message "DEBUG: Candidates list built with %d entries"
                       (length candidates))))

          (message "DEBUG: Building skipped list")
          (dolist (file-reason-pair all-skipped-files)
            (let* ((file (car file-reason-pair))
                   (reason (cdr file-reason-pair))
                   (display-name (file-relative-name file))
                   (entry-data (cons file 0)))
              (push
               (cons (format "SKIPPED: %s (%s)" display-name reason) entry-data)
               all-skipped)))

          (message "DEBUG: Creating buffer display")
          (with-current-buffer buf
            (erase-buffer)
            (insert
             (format ";; %d files from %s\n" (length candidates)
                     (file-name-nondirectory default-directory)))
            (dolist (candidate (reverse candidates))
              (insert (car candidate) "\n"))
            (when all-skipped
              (insert "\n;; Skipped files:\n")
              (dolist (skipped all-skipped)
                (insert ";; " (car skipped) "\n")))
            (insert "\n;; Instructions:\n")
            (insert
             ";; 1. Remove any files you don't want to include\n")
            (insert
             ";; 2. Press 'C-c C-c' when ready to send files to GenAI\n")
            (goto-char (point-min))
            (text-mode)
            (let ((buffer-candidates candidates)
                  (buffer-skipped all-skipped))
              (set (make-local-variable 'buffer-candidates)
                   buffer-candidates)
              (set (make-local-variable 'buffer-skipped)
                   buffer-skipped)
              (local-set-key (kbd "C-c C-c")
                             (lambda ()
                               (interactive)
                               (let*
                                   ((selection
                                     (genai-dired--get-selected-entries
                                      buffer-candidates buffer-skipped))
                                    (selected-files (car selection))
                                    (total-words (cdr selection)))
                                 (when (and selected-files
                                            (y-or-n-p (format
                                                       "Send %d files with total %d words to GenAI? "
                                                       (length
                                                        selected-files)
                                                       total-words)))
                                   (let
                                       ((copied-lines
                                         (genai-dired--copy-contents
                                          selected-files))
                                        (files-content
                                         (current-kill 0)))
                                     (kill-buffer (current-buffer))
                                     (genai-interactive-mode 1)
                                     (let
                                         ((template-type
                                           (genai--select-template)))
                                       (genai-interactive-mode -1)
                                       (genai--ensure-dependencies)
                                       (genai--run-with-template
                                        files-content template-type))
                                     (message
                                      "Sent %d files (%d lines and %d words) to GenAI"
                                      (length selected-files)
                                      copied-lines total-words))))))
              (display-buffer buf)
              (message "DEBUG: Buffer displayed successfully"))))))))

(defun genai-on-region-list-files ()
  "Call genai-dired-copy-contents when multiple files are marked."
  (genai-dired-copy-contents))


(provide 'genai-dired)

(when
    (not load-file-name)
  (message "genai-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))