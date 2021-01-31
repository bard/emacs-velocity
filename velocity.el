;;; velocity.el --- Search, browse, create text in multiple formats

;; Copyright (C) 2017-2021 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; Version: 1.0
;; Maintainer: Massimiliano Mirra <hyperstruct@gmail.com>
;; Keywords: files, hypermedia, matching, outlines
;; URL: http://github.com/bard/emacs-velocity
;; Package-Requires: ((emacs "25.1") (dash "2.12") (stream "2.2.4"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; Search, browse, create text in multiple formats (org, markdown,
;; plain text) at multiple levels (entire file, level-1 sections,
;; level-2 sections).

;; Inspired by Notational Velocity.

;; See README.md for configuration and usage.

;;; Code:

(defvar velocity-backends '())
(require 'velocity-api)
(require 'velocity-backend-org)
(require 'velocity-backend-markdown)
(require 'velocity-frontend-helm)

;;; USER INTERFACE

(defgroup velocity nil
  "Search across notes in multiple formats."
  :group 'convenience
  :prefix "velocity-"
  :link '(url-link "https://github.com/bard/emacs-velocity"))

(defcustom velocity-searches
  nil
  "Where to look for text."
  :type
  '(repeat
    (plist
     :tag "Spec"
     :options
     ((:files (repeat :tag "Files" string))
      (:backend (choice :tag "Backend"
                        :value org-file
                        (const org-file)
                        (const org-heading-1)
                        (const org-heading-2)
                        (const markdown-file))))
     :validate
     (lambda (widget)
       (let ((files (plist-get (widget-value widget) :files)))
         (unless (and files (> (length files) 0))
           (widget-put
            widget :error
            (format-message "Every search spec must define at least one file"))
           widget))))))

(defcustom velocity-targets nil
  "Where to create new entries."
  :type
  '(repeat
    (plist
     :options
     ((:file (string :tag "File"))
      (:backend (choice :tag "Backend"
                        :value org-file
                        (const org-file)
                        (const org-heading-1)
                        (const org-heading-2)
                        (const markdown-file))))
     :validate
     (lambda (widget)
       (unless (and (plist-member (widget-value widget) :file)
                    (plist-member (widget-value widget) :backend))
         (widget-put
          widget :error
          (format-message "Target spec must define :file and :backend"))
         widget)))))

;;;##autoload
(defun velocity ()
  (interactive)
  (velocity--helm))

;;; META

(provide 'velocity)

;;; velocity.el ends here
