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
     :server 't)))

(defun edit-server-stop nil
  "Stop the edit server"
  (interactive)
  (delete-process "edit-server"))


(defun edit-server-filter (proc request)
  "Called each time something connects to the edit server"
  (message (format "edit-server-filter: got %sEOF" request))

  ;; Get the content from the headers, we don't actually much care
  ;; about the headers for now. I suspect this would break on Windows
  ;;
  ;; As we split on \n\n we need to re-assemble to take into account
  ;; any multiple new lines in our content part.
  (let* ((after-headers (cdr (split-string request "\n\n")))
	 (content (car after-headers))
	 (rest (cdr after-headers)))
    (if rest
	(dolist (x rest)
	  (setq content (concat content "\n\n" x))))

    (edit-server-create-edit-buffer proc content)))

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
; Send the response back to the browser as a properly formed
; HTTP/1.0 200 OK message
;

(defun edit-server-send-response (proc string)
  "Send a response back to the calling process with a string"
  (interactive)
  (message "edit-server-send-response")
  (let ((response-header (concat
			  "HTTP/1.0 200 OK\n"
			  "Server: Emacs\n"
			  "Date: "
			  (format-time-string
			   "%a, %d %b %Y %H:%M:%S GMT\n"
			   (current-time)))))
    (process-send-string proc response-header)
    (process-send-string proc "\n")
    (process-send-string proc string)
    (process-send-eof proc)))

(defun edit-server-done()
  "Once someone is done with editing their text edit-server-done is
  called and the response is sent back to the browser"
  (interactive)
  (edit-server-send-response edit-server-current-proc (buffer-string)))

