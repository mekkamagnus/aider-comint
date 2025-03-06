(define-derived-mode aider-comint-mode comint-mode "Aider"
  "A major mode for interacting with Aider's REPL."
  (define-key aider-comint-mode-map (kbd "C-c C-c") 'aider-comint-chat)
  (define-key aider-comint-mode-map (kbd "C-c C-a") 'aider-comint-architect)
  (define-key aider-comint-mode-map (kbd "C-c C-m") 'aider-comint-model)
  (define-key aider-comint-mode-map (kbd "C-c C-s") 'aider-comint-select-model))

(defun aider-comint-format-output (output)
  "Apply basic formatting to OUTPUT from the Aider process.
Currently, this MVP implementation returns OUTPUT unchanged.
Future improvements can add syntax highlighting or keyword coloring."
  output)

(defun aider-comint-process-output (proc string)
  "Process and format the output STRING from the Aider process PROC.
This function applies basic formatting via `aider-comint-format-output`
and then delegates to the standard comint output filter."
  (let ((formatted (aider-comint-format-output string)))
    (comint-output-filter proc formatted)))

(defvar aider-comint-last-directory nil
  "Stores the last directory used for an aider session.")

(defun aider-comint-start ()
  "Start the aider process in a Comint buffer using bash as the shell.
Prompts for a directory to start the aider session in, defaulting to the
last used directory or current directory."
  (interactive)
  (let* ((buffer-name "*aider*")
         (default-directory (aider-comint--select-directory))
    (condition-case err
        (progn
          (make-comint buffer-name "/Users/mekael/.local/bin/aider" nil "--model" "gemini/gemini-1.5-flash")
          (let* ((proc (get-buffer-process (format "*%s*" buffer-name))))
            (if proc
                (progn
                  (set-process-filter proc 'aider-comint-process-output)
                  (message "Aider process started successfully!")
                  (pop-to-buffer (format "*%s*" buffer-name))
                  (aider-comint-mode))
              (message "Failed to start aider process."))))
      (file-error
       (message "Aider executable not found at /Users/mekael/.local/bin/aider. Please ensure aider is installed and in your PATH.")))))

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
