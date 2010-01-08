;;
;; Emacs edit-server
;;
;; This provides an edit server to respond to requests from the Chrome
;; Emacs Chrome plugin. This is my first attempt at doing something
;; with sockets in Emacs. I based it on the following examples:
;;
;; http://www.emacswiki.org/emacs/EmacsEchoServer
;; http://nullprogram.com/blog/2009/05/17/
;;
;; (C) 2009 Alex Bennee (alex@bennee.com)
;; (C) 2010 Riccardo Murri (riccardo.murri@gmail.com)
;;
;; Licensed under GPLv3
;;

; still debugging
(setq debug-on-error 't)
(setq edebug-all-defs 't)

;; Customization
(defcustom edit-server-port 9292
  "Local port the edit server listens to."
  :group 'edit-server
  :type 'integer)

(defcustom edit-server-new-frame t
  "If not nil, edit each buffer in a new frame (and raise it)."
  :group 'edit-server
  :type 'boolean)

;; Vars
(defvar edit-server-proc 'nil
  "Network process associated with the current edit, made local when
 the edit buffer is created")

(defvar edit-server-frame 'nil
  "The frame created for a new edit-server process, made local when
 then edit buffer is created")

(defvar edit-server-clients '() 
  "List of all client processes associated with the server process.")

(defconst edit-server-buffer-name " *edit-server*"
  "Template name of the edit-server process buffers")

;; Mode magic
;
; We want to re-map some of the keys to trigger edit-server-done
; instead of the usual emacs like behaviour. However using
; local-set-key will affect all buffers of the same mode, hence we
; define a special (derived) mode for handling editing of text areas.
;

(defvar edit-server-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x #") 'edit-server-done)
    ;; XXX: should rather invoke 'edit-server-abort when the buffer is deleted,
    ;; and send the original content back to the HTTP client
    (define-key map (kbd "C-x k") 'edit-server-abort)
    (define-key map (kbd "C-x C-s") 'edit-server-done)
  map)
"Keymap for `edit-server-text-mode'.")

;; XXX: make it a minor mode? server-mode is minor
(define-derived-mode edit-server-text-mode text-mode "Edit Server Text Mode"
  "A derived version of text-mode with a few common Emacs keystrokes
rebound to more functions that can deal with the response to the
edit-server request")

;; Edit Server socket code
;

(defun edit-server-start nil
  "Start the edit server"
  (interactive)
  (if (process-status "edit-server")
      (message "An edit-server process is already running")
    (make-network-process
     :name "edit-server"
     :buffer edit-server-buffer-name
     :family 'ipv4
     :host 'local ; only listen to local connections
     :service edit-server-port
     :log 'edit-server-accept
     :server 't)
    (setq edit-server-clients '())
    (message "Created a new edit-server process")))

(defun edit-server-stop nil
  "Stop the edit server"
  (interactive)
  (while edit-server-clients
    (edit-server-kill-client (car edit-server-clients))
    (setq edit-server-clients (cdr edit-server-clients)))
  (if (process-status "edit-server")
      (delete-process "edit-server")
    (message "No edit server running"))
  (if (get-buffer edit-server-buffer-name)
      (kill-buffer edit-server-buffer-name)))

; Write log entries
(defun edit-server-log (string &optional client)
  "If a `*edit-server-log*' buffer exists, write STRING to it for logging purposes."
  (if (get-buffer " *edit-server-log*")
      (with-current-buffer " *edit-server-log*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s:" client) " ")
                string)
        (or (bolp) (newline)))))

(defun edit-server-accept (server client msg)
  "Accept a new client connection."
  (let ((buffer (generate-new-buffer edit-server-buffer-name)))
    (buffer-disable-undo buffer)
    (set-process-buffer client buffer)
    (set-process-filter client 'edit-server-filter)
    (set-process-query-on-exit-flag client nil) ; kill-buffer kills the associated process
    (with-current-buffer buffer
      (set (make-local-variable 'edit-server-phase) 'wait)
      (set (make-local-variable 'edit-server-received) 0)
      (set (make-local-variable 'edit-server-request) nil))
      (set (make-local-variable 'edit-server-content-length) nil))
    (add-to-list 'edit-server-clients client))

(defun edit-server-filter (proc string)
  "Process data received from the client."
  ;; there is no guarantee that data belonging to the same client
  ;; request will arrive all in one go; therefore, we must accumulate
  ;; data in the buffer and process it in different phases, which
  ;; requires us to keep track of the processing state.
  (with-current-buffer (process-buffer proc)
    (insert string)
    (setq edit-server-received 
          (+ edit-server-received (string-bytes string)))
    (when (eq edit-server-phase 'wait)
      ;; look for a complete HTTP request string
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\([A-Z]+\\)\\s-+\\(\\S-+\\)\\s-+\\(HTTP/[0-9\.]+\\)\r?\n" nil t)
          (message "edit-server: Got HTTP `%s' request, processing in buffer `%s'..." 
                   (match-string 1) (current-buffer))
          (setq edit-server-request (match-string 1))
          (setq edit-server-content-length nil)
          (setq edit-server-phase 'head))))
    
    (when (eq edit-server-phase 'head)
      ;; look for "Content-length" header
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^Content-Length:\\s-+\\([0-9]+\\)" nil t)
          (setq edit-server-content-length (string-to-number (match-string 1)))))
      ;; look for head/body separator
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\(\r?\n\\)\\{2\\}" nil t)
          ;; HTTP headers are pure ASCII (1 char = 1 byte), so we can subtract
          ;; the buffer position from the count of received bytes
          (setq edit-server-received
                (- edit-server-received (- (match-end 0) (point-min))))
          ;; discard headers - keep only HTTP content in buffer
          (delete-region (point-min) (match-end 0))
          (setq edit-server-phase 'body))))
    
    (when (eq edit-server-phase 'body)
      (if (and edit-server-content-length
               (> edit-server-content-length edit-server-received))
          (message "edit-server: Received %d bytes of %d ..." 
                   edit-server-received edit-server-content-length)
        ;; all content trasnferred - process request now
        (cond
         ((string= edit-server-request "POST")
          ;; create editing buffer, and move content to it
          (edit-server-create-edit-buffer proc))
         (t
          ;; send 200 OK response to any other request
          (edit-server-send-response proc "edit-server is running.\n" t)))
        ;; wait for another connection to arrive
        (setq edit-server-received 0)
        (setq edit-server-phase 'wait)))))

(defun edit-server-create-edit-buffer(proc)
  "Create an edit buffer, place content in it and save the network
  process for the final call back"
  (let ((buffer (generate-new-buffer "TEXTAREA")))
    (copy-to-buffer buffer (point-min) (point-max))
    (with-current-buffer buffer
      (edit-server-text-mode)
      (buffer-enable-undo)
      (set (make-local-variable 'edit-server-proc) proc)
      (set (make-local-variable 'edit-server-frame) 
           (if edit-server-new-frame (make-frame) nil))
      (if edit-server-new-frame
          (raise-frame edit-server-frame)
        (pop-to-buffer buffer)))))

(defun edit-server-send-response (proc &optional string close)
  "Send an HTTP 200 OK response back to process PROC.
Optional second argument STRING specifies the response content.
If optional third argument CLOSE is non-nil, then process PROC
and its buffer are killed with `edit-server-kill-client'."
  (interactive)
  (message "edit-server-send-response")
  (if proc
      (let ((response-header (concat
                          "HTTP/1.0 200 OK\n"
                          "Server: Emacs\n"
                          "Date: "
                          (format-time-string
                           "%a, %d %b %Y %H:%M:%S GMT\n"
                           (current-time)))))
        (process-send-string proc response-header)
        (process-send-string proc "\n")
        (if string
            (process-send-string proc string))
        (process-send-eof proc)
        (if close 
            (edit-server-kill-client proc)))
    (message "edit-server-send-response: null proc (bug?)")))

(defun edit-server-kill-client (proc)
  "Kill client process PROC and remove it from the list."
  (let ((procbuf (process-buffer proc)))
    (delete-process proc)
    (kill-buffer procbuf)
    (setq edit-server-clients (delq procbuf edit-server-clients))))

(defun edit-server-done ()
  "Once someone is done with editing their text edit-server-done is
  called and the response is sent back to the browser"
  (interactive)
  (let ((buffer (current-buffer))
        (proc edit-server-proc)
        (procbuf (process-buffer edit-server-proc)))
    ;; edit-server-* vars are buffer-local, so they must be used before issuing kill-buffer
    (edit-server-send-response edit-server-proc (buffer-string))
    (if edit-server-frame (delete-frame edit-server-frame))
    ;; delete-frame may change the current buffer
    (kill-buffer buffer)
    (edit-server-kill-client proc)))

(defun edit-server-abort ()
  "Send the original text back to the browser."
  (interactive)
  (let ((buffer (current-buffer))
        (proc edit-server-proc)
        (procbuf (process-buffer edit-server-proc)))
    ;; edit-server-* vars are buffer-local, so they must be used before issuing kill-buffer
    (with-current-buffer procbuf
      (edit-server-send-response proc (buffer-string)))
    (if edit-server-frame (delete-frame edit-server-frame))
    ;; delete-frame may change the current buffer
    (kill-buffer buffer)
    (edit-server-kill-client proc)))
