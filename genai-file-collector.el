;;; genai-file-collector.el --- Shared file selection for GenAI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Collect marked files from visible Dired buffers and present an editable
;; selection to a caller-supplied callback.

;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup genai-file-collector nil
  "Collect files selected in visible Dired buffers."
  :group 'dired
  :prefix "genai-fc-")

(defcustom genai-fc-exclude-path-patterns nil
  "Regular expressions matching paths that must not be collected."
  :type '(repeat regexp)
  :group 'genai-file-collector)

(defcustom genai-fc-safe-extensions nil
  "Allowed filename extensions, including the leading dot.
When nil, all extensions are allowed unless explicitly skipped."
  :type '(repeat string)
  :group 'genai-file-collector)

(defcustom genai-fc-skip-extensions
  '(".7z" ".avi" ".bin" ".bmp" ".bz2" ".class" ".dll" ".dylib"
    ".exe" ".gif" ".gz" ".ico" ".jar" ".jpeg" ".jpg" ".mov"
    ".mp3" ".mp4" ".o" ".pdf" ".png" ".pyc" ".so" ".tar"
    ".tiff" ".wav" ".webp" ".xz" ".zip")
  "Filename extensions excluded from collection."
  :type '(repeat string)
  :group 'genai-file-collector)

(defcustom genai-fc-min-file-size 0
  "Minimum collected file size in bytes."
  :type 'integer
  :group 'genai-file-collector)

(defcustom genai-fc-max-file-size 10000000
  "Maximum collected file size in bytes."
  :type 'integer
  :group 'genai-file-collector)

(defun genai-fc--estimate-words (file)
  "Estimate the number of words or code tokens in FILE."
  (let* ((size (file-attribute-size (file-attributes file)))
         (extension (downcase (or (file-name-extension file) "")))
         (code-extension-p
          (member extension
                  '("bash" "c" "clj" "cpp" "css" "el" "fish" "go"
                    "h" "hpp" "html" "java" "js" "jsx" "kt" "lisp"
                    "m" "php" "ps1" "py" "r" "rb" "rs" "scala"
                    "scss" "sh" "sql" "swift" "ts" "tsx" "zsh"))))
    (if size
        (max 1 (/ size (if code-extension-p 13 6)))
      (if code-extension-p 100 50))))

(defun genai-fc--exclusion-reason (file)
  "Return why FILE is excluded, or nil when it is collectable."
  (let* ((attributes (file-attributes file))
         (size (and attributes (file-attribute-size attributes)))
         (extension (downcase (or (file-name-extension file t) "")))
         (path (file-truename file)))
    (cond
     ((not (file-regular-p file)) "not a regular file")
     ((cl-some (lambda (pattern) (string-match-p pattern path))
               genai-fc-exclude-path-patterns)
      "path matches an excluded pattern")
     ((and size (< size genai-fc-min-file-size)) "file is too small")
     ((and size (> size genai-fc-max-file-size)) "file is too large")
     ((member extension genai-fc-skip-extensions) "extension is excluded")
     ((and genai-fc-safe-extensions
           (not (member extension genai-fc-safe-extensions)))
      "extension is not allowed")
     (nil))))

(defun genai-fc--collect-recursively (path visited)
  "Return (FILES . SKIPPED) below PATH, recording real paths in VISITED."
  (let ((real-path (file-truename path))
        files skipped)
    (unless (gethash real-path visited)
      (puthash real-path t visited)
      (cond
       ((file-directory-p real-path)
        (if (cl-some (lambda (pattern) (string-match-p pattern real-path))
                     genai-fc-exclude-path-patterns)
            (push (cons real-path "directory matches an excluded pattern") skipped)
          (dolist (child (directory-files real-path t
                                          directory-files-no-dot-files-regexp))
            (pcase-let ((`(,child-files . ,child-skipped)
                         (genai-fc--collect-recursively child visited)))
              (setq files (nconc files child-files)
                    skipped (nconc skipped child-skipped))))))
       ((file-regular-p real-path)
        (if-let ((reason (genai-fc--exclusion-reason real-path)))
            (push (cons real-path reason) skipped)
          (push real-path files)))
       (t (push (cons real-path "unsupported filesystem entry") skipped))))
    (cons files skipped)))

(defun genai-fc--marked-files-in-visible-dired-buffers ()
  "Return marked files from every visible Dired buffer."
  (let (files)
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'dired-mode)
          (setq files (nconc files (dired-get-marked-files))))))
    files))

(defun genai-fc--selected-entries (candidates)
  "Return (ENTRIES . TOTAL-WORDS) from CANDIDATES retained in this buffer."
  (let (entries (total-words 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (entry (assoc line candidates)))
          (when entry
            (push entry entries)
            (cl-incf total-words (cddr entry))))
        (forward-line 1)))
    (cons (nreverse entries) total-words)))

(defun genai-fc--display-selection (buffer-name candidates skipped callback)
  "Display CANDIDATES and SKIPPED in BUFFER-NAME, invoking CALLBACK on submit."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; %d collectable files\n" (length candidates)))
        (dolist (candidate candidates)
          (insert (car candidate) "\n"))
        (when skipped
          (insert "\n;; Skipped files:\n")
          (dolist (entry skipped)
            (insert (format ";; %s (%s)\n" (car entry) (cdr entry)))))
        (insert "\n;; Remove unwanted files, then press C-c C-c.\n")
        (text-mode)
        (setq-local genai-fc--buffer-candidates candidates)
        (setq-local genai-fc--buffer-callback callback)
        (local-set-key
         (kbd "C-c C-c")
         (lambda ()
           (interactive)
           (pcase-let ((`(,entries . ,total-words)
                        (genai-fc--selected-entries
                         genai-fc--buffer-candidates)))
             (if entries
                 (funcall genai-fc--buffer-callback entries total-words)
               (user-error "No files remain selected"))))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun genai-fc-collect-and-display (buffer-name callback)
  "Collect visible Dired selections and show them in BUFFER-NAME.
CALLBACK receives the retained entries and their estimated total word count."
  (let ((marked-files (genai-fc--marked-files-in-visible-dired-buffers))
        (visited (make-hash-table :test #'equal))
        files skipped)
    (unless marked-files
      (user-error "No files are marked in visible Dired buffers"))
    (dolist (path marked-files)
      (pcase-let ((`(,path-files . ,path-skipped)
                   (genai-fc--collect-recursively path visited)))
        (setq files (nconc files path-files)
              skipped (nconc skipped path-skipped))))
    (let ((candidates
           (mapcar (lambda (file)
                     (let ((words (genai-fc--estimate-words file)))
                       (cons (format "%s (~%d words)"
                                     (file-relative-name file) words)
                             (cons file words))))
                   (sort files #'string-lessp))))
      (genai-fc--display-selection buffer-name candidates skipped callback))))

(provide 'genai-file-collector)

;;; genai-file-collector.el ends here
