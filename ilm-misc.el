;;; ilm-misc.el --- Rotinas comuns   -*- lexical-binding:t -*-
;; Copyright (C) 2017, 2023 Ildenir Barbosa

;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: dom 17 jan 2021 10:47:42 -03

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
;;
(require 'seq)
(require 'memoize)

(defun ilm--loc ()
  "Numero de linhas do buffer."
  (count-lines (point-min) (point-max)))

(defun ilm--proj-p (dir)
  "Verifica se DIR possui caracteristicas de projeto."
  (let* ((files '("Makefile" ".git" "makefile" ".svn" "node_modules"))
	 (pf (mapcar #'(lambda (el) (expand-file-name el dir )) files)))
    (seq-some #'file-exists-p pf)))

(defun ilm--project-root (dir)
  "Raiz do projeto de DIR ou nil."
  (let ((nextdir dir)
	(home (getenv "HOME"))
	(out nil))
    (while (and (not (null nextdir))
		(not (string= nextdir (file-name-as-directory home)))
		(not (string= nextdir "/")))
      (when (ilm--proj-p nextdir) (setq out nextdir))
      (setq nextdir (file-name-directory (directory-file-name nextdir))))
    out))

(memoize 'ilm--project-root)




(provide 'ilm-misc)

;;; ilm-misc.el ends here
