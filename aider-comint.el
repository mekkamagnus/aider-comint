(define-derived-mode aider-comint-mode comint-mode "Aider"
  "A major mode for interacting with Aider's REPL."
  ;; Rule: Initialize color handling properly
  (setq-local ansi-color-for-comint-mode t)
  (setq-local comint-output-filter-functions
              '(ansi-color-process-output
                comint-postoutput-scroll-to-bottom))
  (setq-local comint-process-echoes nil)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^[^>\n]*> *")
  ;; Add syntax table for better text properties
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\\ "\\")
  ;; Set up faces
  (setq-local font-lock-defaults '(nil t))
  (define-key aider-comint-mode-map (kbd "C-c C-c") 'aider-comint-chat)
  (define-key aider-comint-mode-map (kbd "C-c C-a") 'aider-comint-architect)
  (define-key aider-comint-mode-map (kbd "C-c C-m") 'aider-comint-model)
  (define-key aider-comint-mode-map (kbd "C-c C-s") 'aider-comint-select-model)
  (define-key aider-comint-mode-map (kbd "C-c C-t") 'aider-comint-test-colors))


(defun aider-comint-process-output (proc string)
  "Process and format the output STRING from the Aider process PROC.
Applies ANSI color processing before passing to comint output filter."
  (when (and (process-live-p proc)
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          ;; Rule: Modularity - Use `ansi-color-process-string` for processing ANSI color codes.
          (insert (ansi-color-process-string string nil (process-buffer proc)))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defvar aider-comint-last-directory nil
  "Stores the last directory used for an aider session.")

(defun aider-comint-start ()
  "Start the aider process in a Comint buffer using bash as the shell.
Prompts for a directory to start the aider session in, defaulting to the
last used directory or current directory."
  (interactive)
  (let* ((buffer-name "*aider*")
         (default-directory (aider-comint--select-directory)))
    (condition-case err
        (progn
          ;; Rule: Modularity - Use `make-comint` for creating the comint process.
          (make-comint "aider" buffer-name aider-executable nil "--model" "gemini/gemini-1.5-flash")
          (let* ((proc (get-buffer-process buffer-name))
                 (buffer (get-buffer buffer-name)))
            (if proc
                (progn
                  ;; Ensure ansi-color-for-comint-mode is buffer-local
                  (with-current-buffer buffer
                    (setq ansi-color-for-comint-mode t))
                  (set-process-filter proc 'aider-comint-process-output)
                  (message "Aider process started successfully!")
                  (pop-to-buffer buffer-name)
                  (aider-comint-mode))
              (message "Failed to start aider process."))))
      (file-error
       (message "Aider executable not found at %s. Please ensure aider is installed and in your PATH." aider-executable)))))

(defun aider-comint-send-command (command)
  "Send a COMMAND to the Aider REPL.
If COMMAND is empty, no action is taken."
  (interactive "sCommand: ")
  (unless (string-blank-p command)
    (comint-send-string (get-buffer-process "*aider*") (concat command "\n"))))


(defun aider-comint-code (message)
  "Send a /code MESSAGE to the aider REPL."
  (interactive "sCode message: ")
  (aider-comint-send-command (concat "/code " message)))

(defun aider-comint-ask ()
  "Open a temporary buffer to input a /ask message and send it to the Aider REPL."
  (interactive)
  (let ((buffer (get-buffer-create "*Aider Ask Message*")))
    ;; Switch to the temporary buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    ;; Set it to read chat message and display instructions
    ;; (insert "# Insert your ask message.\n")
    ;; (insert "# Finish with C-c C-c, or cancel with C-c C-k.\n\n")
    ;; Enable a minor mode for keybindings (finish/cancel)
    (aider-chat-input-mode)))

(defvar aider-chat-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'aider-chat-input-finish)
    (define-key map (kbd "C-c C-k") 'aider-chat-input-cancel)
    map)
  "Keymap for `aider-chat-input-mode`.")

(define-minor-mode aider-chat-input-mode
  "Minor mode for entering a chat message in a temporary buffer."
  :lighter " Aider-Chat"
  :keymap aider-chat-input-mode-map
  (evil-insert-state)
  (message "Enter your chat message, finish with C-c C-c, or cancel with C-c C-k."))

(defun aider-chat-input-finish ()
  "Send the input from the chat buffer to the Aider REPL."
  (interactive)
  (let ((message (buffer-substring-no-properties (point-min) (point-max))))
    ;; Send the message to the aider REPL
    (aider-comint-send-command (concat "/ask " message))
    ;; Kill the chat buffer after sending
    (kill-buffer (current-buffer))

    ))

;; (defun aider-comint-ask (message)
;;   "Send a /ask MESSAGE to the aider REPL."
;;   (interactive "sAsk message: ")
;;   (aider-comint-send-command (concat "/ask " message)))

(defun aider-comint-architect (message)
  "Send a /architect MESSAGE to the aider REPL."
  (interactive "sArchitect request: ")
  (aider-comint-send-command (concat "/architect " message)))

(defun aider-comint-add (message)
  "Send a /add MESSAGE to the aider REPL."
  (interactive "sAdd File: ")
  (aider-comint-send-command (concat "/add " message)))


(defun aider-comint-model (model-name)
  "Switch to a different model using the /model tag."
  (interactive "sModel name: ")
  (aider-comint-send-command (concat "/model " model-name)))

(defun aider-comint-select-model ()
  "Select a model from a dropdown menu."
  (interactive)
  (let* (
	 (models '("gemini/gemini-1.5-flash-latest"
		   "gemini/gemini-1.5-pro-latest"
		   "openai/o1-preview"
		   "openai/o1-mini"
		   "openai/gpt-4o"
		   "openai/gpt-4o-mini"
		   "deepseek/deepseek-chat"
		   "deepseek/deepseek-coder"
		   "anthropic/claude-3-5-sonnet-20240620"
		   "anthropic/claude-3-haiku-20240307"
		   "anthropic/claude-3-opus-20240229"
		   "groq/gemma-7b-it"
		   "groq/gemma2-9b-it"
		   "groq/llama-3.1-405b-reasoning"
		   "groq/llama-3.1-70b-versatile"
		   "groq/llama-3.1-8b-instant"
		   "groq/llama2-70b-4096"
		   "groq/llama3-70b-8192"
		   "groq/llama3-8b-8192"
		   "groq/llama3-groq-70b-8192-tool-use-preview"
		   "groq/llama3-groq-8b-8192-tool-use-preview"
		   "groq/mixtral-8x7b-32768"
		   ))
	 (selected-model (completing-read "Select a model: " models nil t)))
    (aider-comint-send-command (concat "/model " selected-model))))


(defun aider-comint-test-colors ()
  "Display all ANSI color codes to test theme interference."
  (interactive)
  (with-current-buffer (get-buffer-create "*ANSI Color Test*")
    (erase-buffer)
    (insert "ANSI Color Test:\n\n")
    ;; Basic colors
    (insert (propertize "Normal text\n" 'face 'default))
    (insert "\033[31mRed text\033[0m\n")
    (insert "\033[32mGreen text\033[0m\n")
    (insert "\033[33mYellow text\033[0m\n")
    (insert "\033[34mBlue text\033[0m\n")
    (insert "\033[35mMagenta text\033[0m\n")
    (insert "\033[36mCyan text\033[0m\n")
    (insert "\033[37mWhite text\033[0m\n\n")
    ;; Bright colors
    (insert "\033[91mBright red text\033[0m\n")
    (insert "\033[92mBright green text\033[0m\n")
    (insert "\033[93mBright yellow text\033[0m\n")
    (insert "\033[94mBright blue text\033[0m\n")
    (insert "\033[95mBright magenta text\033[0m\n")
    (insert "\033[96mBright cyan text\033[0m\n")
    (insert "\033[97mBright white text\033[0m\n\n")
    ;; Background colors
    (insert "\033[41mRed background\033[0m\n")
    (insert "\033[42mGreen background\033[0m\n")
    (insert "\033[43mYellow background\033[0m\n")
    (insert "\033[44mBlue background\033[0m\n")
    (insert "\033[45mMagenta background\033[0m\n")
    (insert "\033[46mCyan background\033[0m\n")
    (insert "\033[47mWhite background\033[0m\n")
    (ansi-color-apply-on-region (point-min) (point-max))
    (aider-comint-mode)
    (pop-to-buffer (current-buffer))))

(defvar aider-executable "/Users/mekael/.local/bin/aider"
  "Path to the aider executable.")

(defun aider-comint-check-color-support ()
  "Verify if ANSI color support is properly configured."
  (interactive)
  (message "ANSI color support: %s"
           (if (and (boundp 'ansi-color-for-comint-mode)
                   ansi-color-for-comint-mode)
               "Enabled" "Disabled")))

(defun aider-comint-check-color-support ()
  "Verify if ANSI color support is properly configured."
  (interactive)
  (message "ANSI color support: %s" 
           (if (and (boundp 'ansi-color-for-comint-mode)
                   ansi-color-for-comint-mode)
               "Enabled" "Disabled")))

(defun aider-comint--select-directory ()
  "Prompt for a directory with validation and history.
Defaults to last used directory or current directory."
  (let ((dir (read-directory-name 
              "Select directory: " 
              aider-comint-last-directory
              nil
              t)))
    (when (file-directory-p dir)
      (setq aider-comint-last-directory dir)
      dir)))
