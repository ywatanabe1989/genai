;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-30 15:58:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/genai/genai-templates.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'genai-variables)

(cl-defun --genai-find-first-capital
    (string)
  "Find first capital letter in STRING and return cons of (letter . position).
Example: (--genai-find-first-capital \"parapHrase.md\") => (h . 5)"
  (let*
      ((name
        (file-name-sans-extension string))
       (case-fold-search nil)
       (capital-pos
        (string-match "[A-Z]" name)))
    (when capital-pos
      (cons
       (downcase
        (substring name capital-pos
                   (1+ capital-pos)))
       capital-pos))))

(cl-defun genai--fetch-templates
    (dir)
  "Return list of formatted template names from DIR that contain capital letters."
  (when
      (file-exists-p dir)
    (sort
     (delq nil
           (mapcar
            (lambda
              (f)
              (let*
                  ((name-without-ext
                    (substring f 0
                               (string-match "\\.md" f)))
                   (capital-info
                    (--genai-find-first-capital f))
                   (first-capital
                    (car capital-info))
                   (capital-pos
                    (cdr capital-info)))
                (cond
                 ((= capital-pos 0)
                  (format "%s" name-without-ext))
                 (capital-pos
                  (format "%s %s" first-capital name-without-ext))
                 (t nil))))
            (directory-files dir nil ".*[A-Z].*\\.md$")))
     #'string<)))

(cl-defun genai--select-template
    ()
  "Prompt the user to select a template type for the GenAI model."
  (unless
      (minibufferp)
    (let*
        ((capital-templates
          (genai--fetch-templates genai-templates-dir))
         (shortcuts
          (make-hash-table :test 'equal))
         (key-count
          (make-hash-table :test 'equal))
         (prompt-parts nil))

      ;; Handle mapped templates first
      (when
          (boundp 'genai-template-mapping)
        (dolist
            (mapping genai-template-mapping)
          (let*
              ((key
                (car mapping))
               (value
                (cdr mapping))
               (base-key
                (substring key 0 1))
               (count
                (gethash base-key key-count 0)))
            (when
                (member value capital-templates)
              (puthash base-key
                       (1+ count)
                       key-count)
              (puthash key value shortcuts)
              (push
               (format "(%s) %s" key value)
               prompt-parts)))))

      ;; Handle unmapped templates with auto-numbering
      (dolist
          (template capital-templates)
        (unless
            (rassoc template genai-template-mapping)
          (let*
              ((base-key
                (downcase
                 (substring template 0 1)))
               (count
                (gethash base-key key-count 0))
               (key
                (if
                    (> count 0)
                    (format "%s%d" base-key
                            (1+ count))
                  base-key)))
            (puthash base-key
                     (1+ count)
                     key-count)
            (puthash key template shortcuts)
            (push
             (format "(%s) %s" key template)
             prompt-parts))))
      (setq prompt-parts
            (sort prompt-parts 'string<))
      (let*
          ((prompt
            (concat "Template or Manual Instruction:\n"
                    (mapconcat 'identity prompt-parts " ")
                    "\n"))
           (input
            (read-string prompt))
           (template-type
            (or
             (gethash input shortcuts)
             (if
                 (string-blank-p input)
                 "None"
               input))))
        (unless
            (string= input "r")
          (display-buffer
           (get-buffer-create genai-buffer-name)))
        template-type))))

;; (cl-defun genai--select-template ()
;;   "Prompt the user to select a template type for the GenAI model."
;;   (unless (minibufferp)
;;     (let* ((capital-templates
;;             (genai--fetch-templates genai-templates-dir))
;;            (shortcuts
;;             (make-hash-table :test 'equal))
;;            (key-count
;;             (make-hash-table :test 'equal))
;;            (prompt-parts nil))
;;       ;; Handle mapped templates first
;;       (when (boundp 'genai-template-mapping)
;;         (dolist (mapping genai-template-mapping)
;;           (let* ((key
;;                   (car mapping))
;;                  (value
;;                   (cdr mapping))
;;                  (base-key
;;                   (substring key 0 1))
;;                  (count
;;                   (gethash base-key key-count 0)))
;;             (when (member value capital-templates)
;;               (puthash base-key
;;                        (1+ count)
;;                        key-count)
;;               (puthash key value shortcuts)
;;               (push
;;                (format "(%s) %s" key value)
;;                prompt-parts)))))
;;       ;; Handle unmapped templates with auto-numbering
;;       (dolist (template capital-templates)
;;         (unless (rassoc template genai-template-mapping)
;;           (let* ((base-key
;;                   (downcase
;;                    (substring template 0 1)))
;;                  (count
;;                   (gethash base-key key-count 0))
;;                  (key
;;                   (if (> count 0)
;;                       (format "%s%d" base-key (1+ count))
;;                     base-key)))
;;             (puthash base-key
;;                      (1+ count)
;;                      key-count)
;;             (puthash key template shortcuts)
;;             (push
;;              (format "(%s) %s" key template)
;;              prompt-parts))))
;;       (setq prompt-parts
;;             (sort prompt-parts 'string<))
;;       (let* ((prompt
;;               (concat "Template or Manual Instruction:\n"
;;                       (mapconcat 'identity prompt-parts " ")
;;                       "\n"))
;;              (input
;;               (read-string prompt))
;;              (template-type
;;               (cond
;;                ;; Special 'g' command to go to buffer
;;                ((string= input "g")
;;                 (when (get-buffer genai-buffer-name)
;;                   (display-buffer genai-buffer-name)
;;                   (with-current-buffer genai-buffer-name
;;                     (goto-char (point-max)))
;;                   (keyboard-quit))
;;                 nil)
;;                ;; Regular template selection
;;                (t
;;                 (or (gethash input shortcuts)
;;                     (if (string-blank-p input)
;;                         "None"
;;                       input))))))

;;         ;; Only switch to buffer for non-null templates and non-'r'
;;         (when (and template-type (not (string= input "r")))
;;           (display-buffer (get-buffer-create genai-buffer-name)))

;;         template-type))))


(provide 'genai-templates)

(when
    (not load-file-name)
  (message "genai-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))