;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-28 14:10:17>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-dired.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; dired.el --- GenAI dired support

(require 'dired)
(require 'genai-core)
(require 'genai-variables)

(defun genai--dired-match-path-expressions (file exprs)
  "Return t if FILE matches any pattern in EXPRS."
  (cl-some (lambda (expr)
             (and (stringp expr)
                  (if (string-prefix-p "." expr)
                      ;; Handle extensions with explicit end anchoring
                      (string-match-p (concat (regexp-quote expr) "$")
                                      file)
                    ;; Handle regular expressions
                    (string-match-p expr file))))
           exprs))

(defun genai--dired-file-detect-binary-p (file &optional num-bytes)
  "Return t if FILE contains NUL in first NUM-BYTES."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 (or num-bytes 512))
    (goto-char (point-min))
    (search-forward "\0" nil t)))

(defun genai--dired-file-detect-text-p (file &optional num-bytes)
  "Return t if FILE is likely text."
  (not (genai--dired-file-detect-binary-p file num-bytes)))

(defun genai--dired-should-include-file
    (file white-ext black-ext white-expr black-expr size-limit)
  "Return t if FILE passes all filters."
  (and (file-regular-p file)
       (genai--dired-file-detect-text-p file)
       (not (genai--dired-match-path-expressions file black-ext))
       (not (genai--dired-match-path-expressions file black-expr))
       (or (genai--dired-match-path-expressions file white-ext)
           (genai--dired-match-path-expressions file white-expr))
       (< (file-attribute-size (file-attributes file)) size-limit)))

(defun genai--dired-get-contents ()
  "Recursively collect marked files in dired, applying safety filters."
  (let* ((marked-files
          (dired-get-marked-files nil nil #'dired-file-marker))
         (white-ext genai-whitelist-extensions)
         (black-ext genai-blacklist-extensions)
         (white-expr genai-whitelist-expressions)
         (black-expr genai-blacklist-expressions)
         (size-limit (* 1024 1024))
         (contents ""))
    (if (null marked-files)
        (read-string "Enter prompt: " "")
      (cl-labels
          ((process-file
             (f)
             (cond
              ((file-directory-p f)
               (dolist (sub (directory-files f t "^[^.]"))
                 (process-file sub)))
              ((genai--dired-should-include-file
                f white-ext black-ext white-expr black-expr
                size-limit)
               (setq contents
                     (concat contents
                             (format "\n\n;;; ----- %s -----\n\n" f)
                             (with-temp-buffer
                               (insert-file-contents f)
                               (buffer-string))))))))
        (dolist (f marked-files)
          (process-file f))
        contents))))

;; (defun genai-on-region-list-files ()
;;   "List files from dired in a buffer for GenAI prompts."
;;   (interactive)
;;   (let* ((marked-files
;;           (dired-get-marked-files nil nil #'dired-file-marker))
;;          (white-ext genai-whitelist-extensions)
;;          (black-ext genai-blacklist-extensions)
;;          (white-expr genai-whitelist-expressions)
;;          (black-expr genai-blacklist-expressions)
;;          (size-limit (* 1024 1024))
;;          (candidates '())
;;          (buf (get-buffer-create "*GenAI Files*"))
;;          (file-count 0))
;;     (cl-labels ((process-file
;;                   (f)
;;                   (cond
;;                    ((file-directory-p f)
;;                     (dolist (sub (directory-files f t "^[^.]"))
;;                       (process-file sub)))
;;                    ((genai--dired-should-include-file
;;                      f white-ext black-ext white-expr black-expr
;;                      size-limit)
;;                     (push
;;                      (cons f (file-relative-name f default-directory))
;;                      candidates)
;;                     (setq file-count (1+ file-count))))))
;;       (dolist (f marked-files)
;;         (process-file f)))

;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (let ((region-name (buffer-name (window-buffer))))
;;         (insert
;;          (format ";; %d files from %s\n" file-count region-name)))

;;       ;; The order of files awkward
;;       ;; However, when I change to nreverse, it does not work
;;       ;; fix here
;;       ;; (dolist (file-entry (nreverse candidates))
;;       (dolist (file-entry (reverse candidates))
;;         (let ((abs-path (car file-entry))
;;               (rel-path (cdr file-entry)))
;;           (insert (format "%s\n" rel-path))))

;;       (insert
;;        "\n;; Instructions:\n"
;;        ";; 1. Remove any files you don't want to include\n"
;;        ";; 2. Press 'C-c C-c' when ready to send files to GenAI\n")

;;       (goto-char (point-min))
;;       (text-mode)
;;       (set (make-local-variable 'genai--dired-candidates) candidates)
;;       (local-set-key (kbd "C-c C-c") #'genai--dired-confirm-files))
;;     (switch-to-buffer-other-window buf)))

(defun genai-on-region-list-files ()
  "List files from dired in a buffer for GenAI prompts."
  (interactive)
  (let* ((marked-files
          (dired-get-marked-files nil nil #'dired-file-marker))
         (white-ext genai-whitelist-extensions)
         (black-ext genai-blacklist-extensions)
         (white-expr genai-whitelist-expressions)
         (black-expr genai-blacklist-expressions)
         (size-limit (* 1024 1024))
         (candidates '())
         (buf (get-buffer-create "GenAI Files"))
         (file-count 0))
    (cl-labels
        ((process-file
           (f)
           (cond
            ;; Recursively process directories
            ((file-directory-p f)
             (dolist (sub (directory-files f t "^[^.]"))
               (process-file sub)))
            ;; If it meets the file criteria, collect it
            ((genai--dired-should-include-file
              f white-ext black-ext white-expr black-expr
              size-limit)
             (push (cons f (file-relative-name f default-directory))
                   candidates)
             (setq file-count (1+ file-count))))))
      ;; Process each marked file (recursively if directory)
      (dolist (f marked-files)
        (process-file f)))

    ;; Since we used `push`, the list order is reversed from insertion.
    ;; To finalize the order, do `nreverse` once before iterating.
    (setq candidates (nreverse candidates))

    (with-current-buffer buf
      (erase-buffer)
      (let ((region-name (buffer-name (window-buffer))))
        (insert
         (format ";; %d files from %s\n" file-count region-name)))

      (dolist (file-entry candidates)
        (let ((abs-path (car file-entry))
              (rel-path (cdr file-entry)))
          (insert (format "%s\n" rel-path))))

      (insert
       "\n;; Instructions:\n"
       ";; 1. Remove any files you don't want to include\n"
       ";; 2. Press 'C-c C-c' when ready to send files to GenAI\n")

      (goto-char (point-min))
      (text-mode)
      (set (make-local-variable 'genai--dired-candidates) candidates)
      (local-set-key (kbd "C-c C-c") #'genai--dired-confirm-files))
    (switch-to-buffer-other-window buf)))

(defun genai--dired-confirm-files ()
  "Process edited file list in buffer and send to GenAI."
  (interactive)
  (let* ((candidates genai--dired-candidates)
         (buffer-content (buffer-string))
         (lines (split-string buffer-content "\n" t))
         (file-lines (seq-filter
                      (lambda (line)
                        (and (not (string-prefix-p ";;" line))
                             (not (string= "" (string-trim line)))))
                      lines))
         (abs-files nil))

    ;; Build list of absolute file paths
    (dolist (line file-lines)
      (let ((trimmed-line (string-trim line)))
        (dolist (candidate candidates)
          (when (string= (cdr candidate) trimmed-line)
            (push (car candidate) abs-files)))))

    (if abs-files
        (let ((contents
               (mapconcat
                (lambda (file)
                  (format "\n\n;;; ----- %s -----\n\n%s"
                          file
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
                abs-files
                "")))
          (kill-buffer)
          (genai--run contents))
      (message "No files selected."))))


(provide 'genai-dired)

(when
    (not load-file-name)
  (message "genai-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))