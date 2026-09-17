;;; test-genai-file-collector.el --- Tests for file collection -*- lexical-binding: t; -*-

(require 'ert)
(require 'genai-file-collector)

(ert-deftest genai-fc-estimates-code-and-text-differently ()
  (let ((code (make-temp-file "genai-fc" nil ".py" (make-string 130 ?x)))
        (text (make-temp-file "genai-fc" nil ".txt" (make-string 120 ?x))))
    (unwind-protect
        (progn
          (should (= 10 (genai-fc--estimate-words code)))
          (should (= 20 (genai-fc--estimate-words text))))
      (delete-file code)
      (delete-file text))))

(ert-deftest genai-fc-filters-disallowed-extensions ()
  (let ((file (make-temp-file "genai-fc" nil ".png" "not-really-an-image")))
    (unwind-protect
        (should (equal "extension is excluded"
                       (genai-fc--exclusion-reason file)))
      (delete-file file))))

(ert-deftest genai-fc-recursion-deduplicates-real-paths ()
  (let* ((directory (make-temp-file "genai-fc" t))
         (file (expand-file-name "sample.txt" directory))
         (link (expand-file-name "alias.txt" directory))
         (visited (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region "hello" nil file nil 'silent)
          (make-symbolic-link file link)
          (pcase-let ((`(,files . ,_skipped)
                       (genai-fc--collect-recursively directory visited)))
            (should (= 1 (length files)))
            (should (equal (file-truename file) (car files)))))
      (delete-directory directory t))))

;;; test-genai-file-collector.el ends here
