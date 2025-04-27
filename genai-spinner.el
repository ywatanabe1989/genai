;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-27 14:53:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai-spinner.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defvar genai--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar genai--spinner-timer nil
  "Timer for the spinner animation.")

(defvar genai--spinner-index 0
  "Current index in the spinner animation.")

(defvar genai--spinner-marker nil
  "Marker for spinner position.")

(defvar genai--fixed-spinner-position nil
  "Fixed position for the spinner.")

(defvar-local genai-input-marker nil
  "Marker for the input position in GenAI buffer.")

(defun genai--start-spinner
    ()
  "Start the spinner animation in the GenAI buffer."
  (when
      (and
       (get-buffer genai-buffer-name)
       (not genai--spinner-timer))
    (with-current-buffer genai-buffer-name
      (goto-char
       (point-max))
      (search-backward
       (upcase genai-engine))
      (goto-char
       (line-end-position))
      (setq genai--fixed-spinner-position
            (point-marker))
      (let
          ((font-lock-mode nil))
        (setq genai--spinner-timer
              (run-with-timer 0 0.1
                              (lambda
                                ()
                                (with-current-buffer genai-buffer-name
                                  (save-excursion
                                    (goto-char
                                     genai--fixed-spinner-position)
                                    (let
                                        ((inhibit-read-only t))
                                      (delete-region
                                       (point)
                                       (line-end-position))
                                      (insert
                                       (propertize
                                        (nth genai--spinner-index
                                             genai--spinner-frames)
                                        'face
                                        '(:foreground "blue")))
                                      (setq genai--spinner-index
                                            (mod
                                             (1+ genai--spinner-index)
                                             (length
                                              genai--spinner-frames)))))))))))))

(defun genai--stop-spinner
    ()
  "Stop the spinner animation."
  (when genai--spinner-timer
    (cancel-timer genai--spinner-timer)
    (setq genai--spinner-timer nil)
    (with-current-buffer genai-buffer-name
      (save-excursion
        (when genai--fixed-spinner-position
          (goto-char genai--fixed-spinner-position)
          (let
              ((inhibit-read-only t))
            (delete-region
             (point)
             (line-end-position))
            (delete-char -1)
            (goto-char
             (point-max))
            (insert "\n\n")
            (insert genai--splitter)
            (insert "\n\n")
            (goto-char
             (point-max))))
        (setq genai--fixed-spinner-position nil)))))


(provide 'genai-spinner)

(when
    (not load-file-name)
  (message "genai-spinner.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))