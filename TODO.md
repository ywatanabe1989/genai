<!-- ---
!-- Timestamp: 2025-09-19 13:22:07
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/genai/TODO.md
!-- --- -->

- [ ] Fix this problem
let: Wrong number of arguments: #[(prompt template-type n-history) ((genai--history-cycle-if-large) (cond ((equal prompt "g") (switch-to-buffer-other-window genai-buffer-name) (keyboard-quit) (message "Jumped to *GenAI*")) ((equal prompt "h") (genai-show-history) (keyboard-quit) (message "Showing history")) (t (save-current-buffer (set-buffer (get-buffer-create genai-buffer-name)) (genai-mode) (font-lock-ensure) (genai--start-python-process-with-template prompt template-type n-history))))) (t) nil "Run GenAI with PROMPT and specified TEMPLATE-TYPE."], 2
user-error: No undo information in this buffer

- [ ] Increment default number of history when run
  - this enables natural conversation while updating to truncate old conversations outside of attention

<!-- EOF -->