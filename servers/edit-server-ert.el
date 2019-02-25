;;; edit-server-ert.el --- ERT tests for edit-server

;; Copyright (C) 2013  Alex Bennée

(package-initialize)

(require 'ert)
(require 'ert-async)

; This test needs edit-server loaded to find the path to the file
(ert-deftest edit-server-compiles ()
  "Tests that edit-server.el compiles cleanly."
  (let ((byte-compile-error-on-warn 't)
        (edit-server-file (find-lisp-object-file-name 'edit-server-start (symbol-function 'edit-server-start))))
    (should (byte-compile-file edit-server-file))))

(ert-deftest-async edit-server-responds (done)
  "Test the edit server starts and responds to a ping test."
  (let ((edit-server-verbose t)
        (edit-server-port 9393))
    (edit-server-start)
    (url-retrieve
     "http://localhost:9393/ping"
     (lambda (x) (funcall done)))
    (edit-server-stop)))
