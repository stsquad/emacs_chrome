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
;; Licensed under GPLv3
;;
;;

; still debugging
(setq debug-on-error 't)
(setq edebug-all-defs 't)

;; Vars
(defvar edit-server-port 9292
  "Port the edit server listens too")

(defvar edit-server-clients '() 
  "alist where KEY is a client process and VALUE is the string")

(defvar edit-server-current-proc 'nil
  "Network process associated with the current edit, made local when
  the edit buffer is create")

(defun edit-server-start nil
  "Start the edit server"
  (interactive)
  (unless (process-status "edit-server")
    (make-network-process
     :name "edit-server"
     :buffer "*edit-server*"
     :family 'ipv4
     :host 'local ; only listen to local connections
     :service edit-server-port
     :filter 'edit-server-filter
     :server 't) 
    (setq edit-server-clients '())))

(defun edit-server-stop nil
  "Stop the edit server"
  (interactive)
  (while edit-server-clients
    (delete-process (car (car edit-server-clients)))
    (setq edit-server-clients (cdr edit-server-clients)))
  (delete-process "edit-server"))


(defun edit-server-filter (proc string)
  "Called each time something connects to the edit server"
  (message (format "edit-server-filter: got %s" string))
  (let ((pending (assoc proc edit-server-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq edir-server-clients (cons (cons proc "") edit-server-clients))
      (setq pending  (assoc proc edit-server-clients)))

    ;;Get the content from the headers, we don't actually much care
    ;;about the headers for now. I suspect this would break on Windows
;    (let ((content (cdr (split-string string "\n\n"))))
;      (edit-server-create-edit-buffer proc content))))
    (edit-server-send-response proc "You got a response")))

(defun edit-server-create-edit-buffer(proc string)
  "Create an edit buffer, place content in it and setup the call
backs"
  (switch-to-buffer "edit-text-buffer")
  (set (make-local-variable 'edit-server-current-proc) proc)
; Can't do this, affects all buffers of same major mode, will need to
; create a special mode to do this.
;  (local-set-key (kbd "C-x k") 'edit-server-done)
;  (local-set-key (kbd "C-x C-s") 'edit-server-done)
  (insert string))

;
; HTTP/1.0 200 OK
; Server: BaseHTTP/0.3 Python/2.6.4
; Date: Fri, 18 Dec 2009 19:00:28 GMT
; 
; This is a test
; For the text
;

(defun edit-server-send-response (proc string)
  "Send a response back to the calling process with a string"
  (interactive)
  (message "edit-server-send-response")
  (process-send-string proc "HTTP/1.0 200 OK\n")
  (process-send-string proc "Server: Emacs\n")
  (process-send-string proc "\n")
  (process-send-string proc string))

(defun edit-server-done()
  "Once someone is done with editing their text edit-server-done is
  called and the response is sent back to the browser"
  (interactive)
  (edit-server-send-response edit-server-current-proc (buffer-string)))
    

; What do I need this for?
(defun edit-server-sentinel (proc msg)
  (delq proc edit-server-clients)
  (message (format "client %s has quit" proc)))



