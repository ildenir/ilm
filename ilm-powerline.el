;;; ilm-powerline.el --- config powerline   -*- lexical-binding:t -*-
;; Copyright (C) 2017, 2023 Ildenir Barbosa


;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: sáb 02 dez 2023 16:50:12 -03

;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: script, convenience
;; URL: http://github.com/ildenir/ilm

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'powerline)

(defun ilm-mode-line ()
  "Mode line simplificada para modos comuns como cc-mode.
Usa powerline para outros modos."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face0 (if active 'powerline-active0 'powerline-inactive0))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (projroot (ilm--project-root (buffer-file-name)))
			  (center (list (powerline-raw "%m" face0 'l)))
			  (rhs (list (powerline-raw "(%l" face0 'l)
				 (powerline-raw ":" face0 'l)
				 (powerline-raw "%c)" face0 'l)
				 (powerline-raw (concat "#" (number-to-string (ilm--loc)) " ")
						face0 'l)))
			  (lhs (list
				 (powerline-raw "    " face0)
				 (when (buffer-modified-p) (powerline-raw "%* " face0 'l))
				 (cond
				  ((not (null projroot))
				   (powerline-raw (concat
						   (all-the-icons-icon-for-file (buffer-file-name) :v-adjust 0)
						   " "
						   (file-name-as-directory
						    (file-name-nondirectory (directory-file-name projroot)))
						   (file-relative-name (buffer-file-name) projroot))))
				  (t (powerline-raw "%b" face0 'l)))
				 (powerline-raw " " face0)
				 (when active
				   (powerline-raw (concat
						   (ilm-pomodoro/icon)
						   (ilm-pomodoro/elapse-time)))
				   ))))
		     (concat  (powerline-render lhs)
			      (powerline-fill-center face0 (/ (powerline-width center) 2.0))
			      (powerline-render center)
			      (powerline-fill face0 (powerline-width rhs))
			      (powerline-render rhs)))))))

(provide 'ilm-powerline)

;;; ilm-powerline.el ends here
