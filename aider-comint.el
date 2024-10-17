(define-derived-mode aider-comint-mode comint-mode "Aider"
  "A major mode for interacting with Aider's REPL."
  (define-key aider-comint-mode-map (kbd "C-c C-c") 'aider-comint-chat)
  (define-key aider-comint-mode-map (kbd "C-c C-a") 'aider-comint-architect)
  (define-key aider-comint-mode-map (kbd "C-c C-m") 'aider-comint-model))

(defun aider-comint-start ()
  "Start the aider process in a Comint buffer using  bash as the shell."
  (interactive) 
  (let (
	(buffer-name "*aider*")
	(shell-file-name "/bin/bash")
	(explicit-shell-file-name "/bin/bash")
	(process-environment (cons "SHELL=/bin/bash" process-environment))
	)
    (apply 'make-comint
           buffer-name            ;; Process name
           "/usr/local/bin/aider" ;; Path to bash
           nil
           '("--model" "gemini/gemini-1.5-flash"))
    (if (get-buffer-process (format "*%s*" buffer-name))
        (progn
          (message "Aider process started successfully!")
          (pop-to-buffer (format "*%s*" buffer-name)) ;; Switch to the Aider-Chat buffer
	  (aider-comint-mode))  
      (message "Failed to start aider process."))
    )) 

(defun aider-comint-send-command (command)
  "Send a COMMAND to the aider REPL."
  (interactive "sCommand: ")
  (comint-send-string (get-buffer-process "**aider**") (concat command "\n")))

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
    (kill-buffer)

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



;; * Backlog
;; ** TODO Color code keywords
;; ** TODO Models dropdown list


