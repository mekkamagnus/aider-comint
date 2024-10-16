(defun aider-comint-start ()
  "Start the aider process in a Comint buffer using Bash as the shell."
  (interactive)
  (let ((buffer-name "*Aider*")
        (aider-path "/usr/local/bin/aider")  ;; Update this with the actual path
        (aider-args '("--model" "gemini/gemini-1.5-flash"))
        ;; Force the use of Bash
        (shell-file-name "/bin/bash")  ;; Ensure Bash is used
        (explicit-shell-file-name "/bin/bash")
        (process-environment (cons "SHELL=/bin/bash" process-environment)))  ;; Set Bash in the environment
    (apply 'make-comint
           "aider"      ;; Process name
           aider-path   ;; Path to aider executable
           nil          ;; No additional arguments for now
           aider-args)  ;; Pass aider arguments
    (pop-to-buffer buffer-name)))  ;; Switch to the Aider-Chat buffer
