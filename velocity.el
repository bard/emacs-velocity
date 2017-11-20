;;; emacs-velocity.el --- Quickly search notes in multiple formats

;; Copyright (C) 2017 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; Version: 1.0
;; Maintainer: Massimiliano Mirra <hyperstruct@gmail.com>
;; Keywords: notes, search, org, markdown
;; URL: http://github.com/bard/emacs-velocity
;; Package-Requires: ((dash "2.12"))

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

;; Please see README.md.

;;; Code:

(defvar velocity-searches
  '())

(defvar velocity-targets
  '())

(defvar velocity-backends
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER API

;;;##autoload
(defun velocity-define-searches (&rest defs)
  (setq velocity-searches
        (loop for (name . searches) in defs
              collect (cons name searches))))

;;;##autoload
(defun helm-velocity ()
  (interactive)
  (require 'velocity-backends)
  (require 'helm)
  (require 'velocity-frontend-helm)
  (helm-velocity-1))

;;;##autoload
(defun ivy-velocity ()
  (interactive)
  (require 'velocity-backends)
  (require 'ivy)
  (require 'velocity-frontend-ivy)
  (error "Not implemented yet."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META

(provide 'velocity)

;;; velocity.el ends here
