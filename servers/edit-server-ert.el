;;; edit-server-ert.el --- ERT tests for edit-server
;;
;; Copyright (C) 2013  Alex Bennée
;;
;;; Commentary:
;;
;; Currently all this does is a simple compile test.  More can be done
;; I'm sure.
;;
;;; Code:

(require 'ert)

; This test needs edit-server loaded to find the path to the file
(ert-deftest edit-server-compiles ()
  "Tests that edit-server.el compiles cleanly."
  (let ((byte-compile-error-on-warn 't)
        (edit-server-file (find-lisp-object-file-name 'edit-server-start (symbol-function 'edit-server-start))))
    (should (byte-compile-file edit-server-file))))

;; Helpers
(defvar edit-server-ert-edit-count
  0
  "Count of times ert has triggered an edit.")

(defvar edit-server-ert-edit-frame
  nil
  "Name of the last frame we were editing.")

(defun edit-server-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(defun edit-server-url-http-post (text)
  "Send POST edit request to URL with TEXT."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")
           ("x-url" . "http://edit.server.test/ert-server-test")
           ("x-id"  . "1")
           ("x-file" . nil)))
        (url-request-data text))
        (url-retrieve-synchronously "http://127.0.0.1:9292/edit")))
;                      'edit-server-switch-to-url-buffer)))

(defun edit-server-ert-helper ()
  "A test function for inserting text and finishing an edit session.
It runs using the edit-server mode hook."
  (message "running ert helper hook")
  (insert "have edited some text")
  (edit-server-done))

(ert-deftest edit-server-new-frame ()
  "Check we create a new frame when set."
  (let ((nf (length (frame-list)))
        (old-hooks edit-server-edit-mode-hook)
        (edit-server-new-frame t))
    (unwind-protect
        (progn
          (add-hook 'edit-server-edit-mode-hook
                    'edit-server-ert-helper)
          (message "added hook")
          (with-buffer (edit-server-url-http-post "This is some text")
                       (message (buffer-string)))
          (message "did url call")
          (should (> (length (frame-list)) nf)))
      (setq edit-server-edit-mode-hook old-hooks))))

(provide 'edit-server-ert)
;;; edit-server-ert.el ends here
