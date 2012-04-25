;;; edit-server.el --- server that responds to edit requests from Chrome

;; Copyright (C) 2009-2011  Alex Bennee
;; Copyright (C) 2010-2011  Riccardo Murri

;; Author: Alex Bennee <alex@bennee.com>
;; Maintainer: Alex Bennee <alex@bennee.com>
;; Version: 1.10
;; Homepage: https://github.com/stsquad/emacs_chrome

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides an edit server to respond to requests from the Chrome
;; Emacs Chrome plugin. This is my first attempt at doing something
;; with sockets in Emacs. I based it on the following examples:
;;
;;   http://www.emacswiki.org/emacs/EmacsEchoServer
;;   http://nullprogram.com/blog/2009/05/17/
;;
;; To use it ensure the file is in your load-path and add something
;; like the following examples to your .emacs:
;;
;; To open pages for editing in new buffers in your existing Emacs
;; instance:
;;
;;   (when (require 'edit-server nil t)
;;     (setq edit-server-new-frame nil)
;;     (edit-server-start))
;;
;; To open pages for editing in new frames using a running emacs
;; started in --daemon mode:
;;
;;   (when (and (require 'edit-server nil t) (daemonp))
;;     (edit-server-start))
;;
;; Buffers are edited in `text-mode' by default; to use a different
;; major mode, change `edit-server-default-major-mode' or customize
;; `edit-server-url-major-mode-alist' to specify major modes based
;; on the remote URL:
;;
;;   (setq edit-server-url-major-mode-alist
;;         '(("github\\.com" . markdown-mode)))
;;
;; Alternatively, set the mode in `edit-server-start-hook'.  For
;; example:
;;
;; (add-hook 'edit-server-start-hook
;;          (lambda ()
;;            (when (string-match "github.com" (buffer-name))
;;              (markdown-mode))))


;;; Code:

;; uncomment to debug
;; (setq debug-on-error t)
;; (setq edebug-all-defs t)

(unless (featurep 'make-network-process)
  (error "Incompatible version of [X]Emacs - lacks make-network-process"))

;;; Customization

(defcustom edit-server-port 9292
  "Local port the edit server listens to."
  :group 'edit-server
  :type 'integer)

(defcustom edit-server-host nil
  "If not nil, accept connections from HOST address rather than just
localhost. This may present a security issue."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-verbose nil
  "If not nil, log connections and progress also to the echo area."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-done-hook nil
  "Hook run when done editing a buffer for the Emacs HTTP edit-server.
Current buffer holds the text that is about to be sent back to the client."
  :group 'edit-server
  :type 'hook)

(defcustom edit-server-start-hook nil
  "Hook run when starting a editing buffer.  Current buffer is
the fully prepared editing buffer.  Use this hook to enable
buffer-specific modes or add key bindings."
  :group 'edit-server
  :type 'hook)

;; frame options

(defcustom edit-server-new-frame t
  "If not nil, edit each buffer in a new frame (and raise it)."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-new-frame-alist
  '((name . "Emacs TEXTAREA")
    (width . 80)
    (height . 25)
    (minibuffer . t)
    (menu-bar-lines . t))
  "Frame parameters for new frames.  See `default-frame-alist' for examples.
If nil, the new frame will use the existing `default-frame-alist' values."
  :group 'edit-server
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

(defcustom edit-server-default-major-mode
  'text-mode
  "The default major mode to use in editing buffers, if no other
mode is selected by `edit-server-url-major-mode-alist'."
  :group 'edit-server
  :type 'function)

(defcustom edit-server-url-major-mode-alist
  nil
  "A-list of patterns and corresponding functions; when the first
pattern is encountered which matches `edit-server-url', the
corresponding function will be called in order to set the desired
major mode. If no pattern matches,
`edit-server-default-major-mode' will be used."
  :group 'edit-server
  :type '(alist :key-type (string :tag "Regexp")
		:value-type (function :tag "Major mode function")))

(defcustom edit-server-new-frame-mode-line t
  "Show the emacs frame's mode-line if set to t; hide if nil."
  :group 'edit-server
  :type 'boolean)

;;; Variables

(defconst edit-server-process-buffer-name " *edit-server*"
  "Template name of the edit-server process buffers.")

(defconst edit-server-log-buffer-name "*edit-server-log*"
  "Template name of the edit-server process buffers.")

(defconst edit-server-edit-buffer-name "TEXTAREA"
  "Template name of the edit-server text editing buffers.")

(defvar edit-server-clients ()
  "List of all client processes associated with the server process.")

;; Buffer local variables
;;
;; These are all required to associate the edit buffer with the
;; correct connection to the client and allow for the buffer to be sent
;; back when ready. They are `permanent-local` to avoid being reset if
;; the buffer changes major modes.

(defvar edit-server-proc nil
  "Network process associated with the current edit.")
(make-variable-buffer-local 'edit-server-proc)
(put 'edit-server-proc 'permanent-local t)

(defvar edit-server-frame nil
  "The frame created for a new edit-server process.")
(make-variable-buffer-local 'edit-server-frame)
(put 'edit-server-frame 'permanent-local t)

(defvar edit-server-phase nil
  "Symbol indicating the state of the HTTP request parsing.")
(make-variable-buffer-local 'edit-server-phase)
(put 'edit-server-phase 'permanent-local t)

(defvar edit-server-received nil
  "Number of bytes received so far in the client buffer.
Depending on the character encoding, may be different from the buffer length.")
(make-variable-buffer-local 'edit-server-received)
(put 'edit-server-received 'permanent-local t)

(defvar edit-server-request nil
  "The HTTP request (GET, HEAD, POST) received.")
(make-variable-buffer-local 'edit-server-request)
(put 'edit-server-request 'permanent-local t)

(defvar edit-server-content-length nil
  "The value gotten from the HTTP `Content-Length' header.")
(make-variable-buffer-local 'edit-server-content-length)
(put 'edit-server-content-length 'permanent-local t)

(defvar edit-server-url nil
  "The value gotten from the HTTP `x-url' header.")
(make-variable-buffer-local 'edit-server-url)
(put 'edit-server-url 'permanent-local t)

;;; Mode magic
;;
;; We want to re-map some of the keys to trigger edit-server-done
;; instead of the usual emacs like behaviour. However using
;; local-set-key will affect all buffers of the same mode, hence we
;; define a special (derived) mode for handling editing of text areas.


(defvar edit-server-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'edit-server-save)
    (define-key map (kbd "C-x #")   'edit-server-done)
    (define-key map (kbd "C-c C-c") 'edit-server-done)
    (define-key map (kbd "C-x C-c") 'edit-server-abort)
    map)
  "Keymap for minor mode `edit-server-edit-mode'.

Redefine a few common Emacs keystrokes to functions that can
deal with the response to the edit-server request.

Any of the following keys will close the buffer and send the text
to the HTTP client: C-x #, C-c C-c.

Pressing C-x C-s will save the current state to the kill-ring.

If any of the above isused with a prefix argument, the
unmodified text is sent back instead.")

(define-minor-mode edit-server-edit-mode
  "Minor mode enabled on buffers opened by `edit-server-accept'.

Its sole purpose is currently to enable
`edit-server-edit-mode-map', which overrides common keystrokes to
send a response back to the client."
  :group 'edit-server
  :lighter " EditSrv"
  :init-value nil
  :keymap edit-server-edit-mode-map)

(defun turn-on-edit-server-edit-mode-if-server ()
  "Turn on `edit-server-edit-mode' if in an edit-server buffer."
  (when edit-server-proc
    (edit-server-edit-mode t)))

(define-globalized-minor-mode global-edit-server-edit-mode
  edit-server-edit-mode turn-on-edit-server-edit-mode-if-server)
(global-edit-server-edit-mode t)


;; Edit Server socket code

(defun edit-server-start (&optional verbose)
  "Start the edit server.

If argument VERBOSE is non-nil, logs all server activity to buffer
`*edit-server-log*'.  When called interactivity, a prefix argument
will cause it to be verbose."
  (interactive "P")
  (if (or (process-status "edit-server")
	  (null (condition-case err
		    (make-network-process
		     :name "edit-server"
		     :buffer edit-server-process-buffer-name
		     :family 'ipv4
		     :host (or edit-server-host 'local)
		     :service edit-server-port
		     :log 'edit-server-accept
		     :server t
		     :noquery t)
		  (file-error nil))))
      (message "An edit-server process is already running")
    (setq edit-server-clients '())
    (if verbose (get-buffer-create edit-server-log-buffer-name))
    (edit-server-log nil "Created a new edit-server process")))

(defun edit-server-stop nil
  "Stop the edit server"
  (interactive)
  (while edit-server-clients
    (edit-server-kill-client (car edit-server-clients))
    (setq edit-server-clients (cdr edit-server-clients)))
  (if (process-status "edit-server")
      (delete-process "edit-server")
    (message "No edit server running"))
  (when (get-buffer edit-server-process-buffer-name)
    (kill-buffer edit-server-process-buffer-name)))

(defun edit-server-log (proc fmt &rest args)
  "If a `*edit-server-log*' buffer exists, write STRING to it.
This is done for logging purposes.  If `edit-server-verbose' is
non-nil, then STRING is also echoed to the message line."
  (let ((string (apply 'format fmt args)))
    (if edit-server-verbose
	(message string))
    (if (get-buffer edit-server-log-buffer-name)
	(with-current-buffer edit-server-log-buffer-name
	  (goto-char (point-max))
	  (insert (current-time-string)
		  " "
		  (if (processp proc)
		      (concat
		       (buffer-name (process-buffer proc))
		       ": ")
		    "") ; nil is not acceptable to 'insert
		  string)
	  (or (bolp) (newline))))))

(defun edit-server-accept (server client msg)
  "Accept a new client connection."
  (let ((buffer (generate-new-buffer edit-server-process-buffer-name)))
    (when (fboundp 'set-buffer-multibyte)
      (set-buffer-multibyte t)) ; djb
    (buffer-disable-undo buffer)
    (set-process-buffer client buffer)
    (set-process-filter client 'edit-server-filter)
    ;; kill-buffer kills the associated process
    (set-process-query-on-exit-flag client nil)
    (with-current-buffer buffer
      (setq edit-server-phase 'wait
	    edit-server-received 0
	    edit-server-request nil))
    (setq edit-server-content-length nil
	  edit-server-url nil))
  (add-to-list 'edit-server-clients client)
  (edit-server-log client msg))

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
	(when (re-search-forward
	       "^\\([A-Z]+\\)\\s-+\\(\\S-+\\)\\s-+\\(HTTP/[0-9\.]+\\)\r?\n"
	       nil t)
	  (edit-server-log
	   proc "Got HTTP `%s' request, processing in buffer `%s'..."
	   (match-string 1) (current-buffer))
	  (setq edit-server-request (match-string 1))
	  (setq edit-server-content-length nil)
	  (setq edit-server-phase 'head))))

    (when (eq edit-server-phase 'head)
      ;; look for "Content-length" header
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^Content-Length:\\s-+\\([0-9]+\\)" nil t)
	  (setq edit-server-content-length
		(string-to-number (match-string 1)))))
      ;; look for "x-url" header
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^x-url: .*//\\(.*\\)/" nil t)
	  (setq edit-server-url (match-string 1))))
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
	  (edit-server-log proc
			   "Received %d bytes of %d ..."
			   edit-server-received edit-server-content-length)
	;; all content transferred - process request now
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

(defun edit-server-create-frame(buffer)
  "Create a frame for the edit server"
  (if edit-server-new-frame
      (let ((new-frame
	     (if (memq window-system '(ns mac))
		 ;; Aquamacs, Emacs NS, Emacs (experimental) Mac port
		 (make-frame edit-server-new-frame-alist)
	       (make-frame-on-display (getenv "DISPLAY")
				      edit-server-new-frame-alist))))
	(unless edit-server-new-frame-mode-line
	  (setq mode-line-format nil))
	(select-frame new-frame)
	(when (and (eq window-system 'x)
		   (fboundp 'x-send-client-message))
	  (x-send-client-message nil 0 nil
				 "_NET_ACTIVE_WINDOW" 32
				 '(1 0 0)))
	(raise-frame new-frame)
	(set-window-buffer (frame-selected-window new-frame) buffer)
	new-frame)
    (pop-to-buffer buffer)
    (raise-frame)
    nil))

(defun edit-server-choose-major-mode ()
  "Use `edit-server-url-major-mode-alist' to choose a major mode
initialization function based on `edit-server-url', or fall back
to `edit-server-default-major-mode'"
  (let ((pairs edit-server-url-major-mode-alist)
	(mode edit-server-default-major-mode))
    (while pairs
      (let ((entry (car pairs)))
	(if (string-match (car entry) edit-server-url)
	    (setq mode (cdr entry)
		  pairs nil)
	  (setq pairs (cdr pairs)))))
    (funcall mode)))

(defun edit-server-create-edit-buffer(proc)
  "Create an edit buffer, place content in it and save the network
	process for the final call back"
  (let ((buffer (generate-new-buffer
		 (or edit-server-url
		     edit-server-edit-buffer-name))))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte t))) ; djb
    (copy-to-buffer buffer (point-min) (point-max))
    (with-current-buffer buffer
      (setq edit-server-url (with-current-buffer (process-buffer proc) edit-server-url))
      (edit-server-choose-major-mode)
      ;; Allow `edit-server-start-hook' to override the major mode.
      ;; (re)setting the minor mode seems to clear the buffer-local
      ;; variables that we depend upon for the response, so call the
      ;; hooks early on
      (run-hooks 'edit-server-start-hook)
      (not-modified)
      (add-hook 'kill-buffer-hook 'edit-server-abort* nil t)
      (buffer-enable-undo)
      (setq edit-server-proc proc
	    edit-server-frame (edit-server-create-frame buffer))
      (edit-server-edit-mode))))

(defun edit-server-send-response (proc &optional body close)
  "Send an HTTP 200 OK response back to process PROC.
Optional second argument BODY specifies the response content:
    - If nil, the HTTP response will have null content.
    - If a string, the string is sent as response content.
    - Any other value will cause the contents of the current
      buffer to be sent.
If optional third argument CLOSE is non-nil, then process PROC
and its buffer are killed with `edit-server-kill-client'."
  (interactive)
  (if (processp proc)
      (let ((response-header (concat
			      "HTTP/1.0 200 OK\n"
			      (format "Server: Emacs/%s\n" emacs-version)
			      "Date: "
			      (format-time-string
			       "%a, %d %b %Y %H:%M:%S GMT\n"
			       (current-time)))))
	(process-send-string proc response-header)
	(process-send-string proc "\n")
	(cond
	 ((stringp body)
	  (process-send-string proc (encode-coding-string body 'utf-8)))
	 ((not body) nil)
	 (t
	  (encode-coding-region (point-min) (point-max) 'utf-8)
	  (process-send-region proc (point-min) (point-max))))
	(process-send-eof proc)
	(when close
	  (edit-server-kill-client proc))
	(edit-server-log proc "Editing done, sent HTTP OK response."))
    (message "edit-server-send-response: invalid proc (bug?)")))

(defun edit-server-kill-client (proc)
  "Kill client process PROC and remove it from the list."
  (let ((procbuf (process-buffer proc)))
    (delete-process proc)
    (when (buffer-live-p procbuf)
      (kill-buffer procbuf))
    (setq edit-server-clients (delq proc edit-server-clients))))

(defun edit-server-done (&optional abort nokill)
  "Finish editing: send HTTP response back, close client and editing buffers.

The current contents of the buffer are sent back to the HTTP
client, unless argument ABORT is non-nil, in which case then the
original text is sent back.
If optional second argument NOKILL is non-nil, then the editing
buffer is not killed.

When called interactively, use prefix arg to abort editing."
  (interactive "P")
  ;; Do nothing if the connection is closed by the browser (tab killed, etc.)
  (unless (eq (process-status edit-server-proc) 'closed)
    (let ((buffer (current-buffer))
	  (proc edit-server-proc)
	  (procbuf (process-buffer edit-server-proc)))
      ;; edit-server-* vars are buffer-local,
      ;; so they must be used before issuing kill-buffer
      (if abort
	  ;; send back original content
	  (with-current-buffer procbuf
	    (run-hooks 'edit-server-done-hook)
	    (edit-server-send-response proc t))
	;; send back edited content
	(save-restriction
	  (widen)
	  (buffer-disable-undo)
	  ;; ensure any format encoding is done (like longlines)
	  (dolist (format buffer-file-format)
	    (format-encode-region (point-min) (point-max) format))
	  ;; send back
	  (run-hooks 'edit-server-done-hook)
	  (edit-server-send-response edit-server-proc t)
	  ;; restore formats (only useful if we keep the buffer)
	  (dolist (format buffer-file-format)
	    (format-decode-region (point-min) (point-max) format))
	  (buffer-enable-undo)))
      (when edit-server-frame
	(delete-frame edit-server-frame))
      ;; delete-frame may change the current buffer
      (unless nokill
	(kill-buffer buffer))
      (edit-server-kill-client proc))))

;;
;; There are a couple of options for handling the save
;; action. The intent is to preserve data but not finish
;; editing. There are a couple of approaches that could
;; be taken:
;;  a) Use the iterative edit-server option (send a buffer
;;     back to the client which then returns new request
;;     to continue the session).
;;  b) Back-up the edit session to a file
;;  c) Save the current buffer to the kill-ring
;;
;; I've attempted to do a) a couple of times but I keep running
;; into problems which I think are emacs bugs. So for now I'll
;; just push the current buffer to the kill-ring.

(defun edit-server-save ()
  "Save the current state of the edit buffer."
  (interactive)
  (save-restriction
    (widen)
    (buffer-disable-undo)
    (copy-region-as-kill (point-min) (point-max))
    (buffer-enable-undo)))

(defun edit-server-abort ()
  "Discard editing and send the original text back to the browser."
  (interactive)
  (edit-server-done t))

(defun edit-server-abort* ()
  "Discard editing and send the original text back to the browser,
but don't kill the editing buffer."
  (interactive)
  (edit-server-done t t))

(provide 'edit-server)

;;; edit-server.el ends here
