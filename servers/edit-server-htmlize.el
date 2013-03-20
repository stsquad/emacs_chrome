;;; edit-server-htmlize.el --- (de)HTMLization hooks for edit-server.el

;;; Copyright (C) 2013 Roland McGrath

;; Author: Roland McGrath <roland@hack.frob.com>
;; Maintainer: Roland McGrath <roland@hack.frob.com>
;; Version: 0.2

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

;; This is intended for use with edit-server.el and the corresponding
;; "Edit with Emacs" extension for the Chromium/Google Chrome Web browser.
;; See https://github.com/stsquad/emacs_chrome
;;
;; This provides some simple functions for deHTMLizing and reHTMLizing
;; "plain text" as encoded by e.g. GMail composition boxes.

;;; Code:

;; (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
;; (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)


(require 'edit-server)
(require 'html2text)


(defconst edit-server-htmlize-entity-alist
  '(("<" . "&lt;")
    (">" . "&gt;")
    ("&" . "&amp;"))
  "Alist of strings that cannot safely appear inside an HTML <pre> element.
This maps a string to its HTML entity string.")

(defconst edit-server-htmlize-regexp
  (mapconcat (lambda (pair) (concat "\\(" (regexp-quote (car pair)) "\\)"))
             edit-server-htmlize-entity-alist
             "\\|"))
(defconst edit-server-htmlize-replacements
  (apply 'vector (mapcar 'cdr edit-server-htmlize-entity-alist)))

(defun edit-server-htmlize-replace (regexp replacements)
  (save-match-data
    (while (re-search-forward regexp nil t)
      (let ((i 0))
        (while (and (< i (length replacements))
                    (null (match-beginning (1+ i))))
          (setq i (1+ i)))
        (replace-match (aref replacements i) t t)))))

(defun edit-server-htmlize-buffer ()
  "Do a simple HTMLification of the buffer as plain text.
This produces HTML intended to reproduce the original plain text contents
of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "<pre>")
    (edit-server-htmlize-replace edit-server-htmlize-regexp
                                 edit-server-htmlize-replacements)
    (goto-char (point-max))
    (insert "</pre>")))

;; XXX Modified from 23.3.1's gnus/html2text.el html2text,
;; which wants to eat <br> and refill paragraphs.
(defun edit-server-dehtmlize-buffer ()
  "Convert HTML to plain text in the current buffer.
This differs from \\[html2text] in that it doesn't refill paragraphs,
but only turns <br> tags into line breaks."
  (interactive)
  (save-excursion
    (let ((case-fold-search t)
          (buffer-read-only nil)
	  (edit-server-remove-list (append (list "span") html2text-remove-tag-list)))
      (html2text-replace-string "<br>" "\n" (point-min) (point-max))
      (html2text-replace-string "</div>" "</div>\n" (point-min) (point-max))
      (html2text-remove-tags edit-server-remove-list)
      (html2text-format-tags)
      (html2text-remove-tags html2text-remove-tag-list2)
      (edit-server-html2text-substitute)
      (html2text-format-single-elements))))

(defconst edit-server-html2text-substitute-regexp
  (mapconcat (lambda (pair) (concat "\\(" (regexp-quote (car pair)) "\\)"))
             html2text-replace-list
             "\\|"))
(defconst edit-server-html2text-substitute-replacements
  (apply 'vector (mapcar 'cdr html2text-replace-list)))

;; XXX Replacement for 23.3.1's gnus/html2text.el html2text-substitute,
;; which is confounded by "&amp;lt;" and the like.
(defun edit-server-html2text-substitute ()
  "See the variable `html2text-replace-list' for documentation."
  (interactive)
  (goto-char (point-min))
  (edit-server-htmlize-replace edit-server-html2text-substitute-regexp
                               edit-server-html2text-substitute-replacements))

;;;###autoload
(defcustom edit-server-htmlize-url-regexp
  (concat "^" (regexp-quote "mail.google.com/mail/"))
  "*Regexp matching `edit-server-url' in a buffer that should be HTMLized.
See `edit-server-maybe-htmlize-buffer'."
  :type 'regexp
  :group 'edit-server
  :require 'edit-server-htmlize)

;;;###autoload
(defun edit-server-maybe-htmlize-buffer ()
  "Possibly HTMLize the current buffer of plain text.
This calls `edit-server-htmlize-buffer' if `edit-server-url'
matches `edit-server-htmlize-url-regexp'.

This is intended for use on `edit-server-done-hook'."
  (interactive)
  (if (string-match edit-server-htmlize-url-regexp edit-server-url)
      (edit-server-htmlize-buffer)))

;;;###autoload
(defun edit-server-maybe-dehtmlize-buffer ()
  "Possibly deHTMLize the current buffer into plain text.
This calls `edit-server-dehtmlize-buffer' if `edit-server-url'
matches `edit-server-htmlize-url-regexp'.

This is intended for use on `edit-server-start-hook'."
  (interactive)
  (if (string-match edit-server-htmlize-url-regexp edit-server-url)
      (edit-server-dehtmlize-buffer)))

(provide 'edit-server-htmlize)

;;; edit-server-htmlize.el ends here
