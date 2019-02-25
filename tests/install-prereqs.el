;;; install-prereqs --- Install any pre-requisites needed for testing
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'ert-async)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'install-prereqs)
;;; install-prereqs.el ends here
