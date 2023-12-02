;;; ilm-front-screen.el --- Tela de inicializacao das configuracoes ilm

;; Copyright (C) 2017, 2021 Ildenir Barbosa

;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: qua 13 jan 2021 21:45:59 -03
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

(require 'ilm-misc)
(require 'recentf)
(require 'all-the-icons)
(require 'dashboard)



(defun ilm-front-screen--recent-projects ()
  "Retorna lista diretorios de projetos recentes.
Entende-se projeto como qualquer diretorio com diretorio .git."
  (remove nil (seq-uniq
	       (seq-map #'(lambda (el) (ilm--project-root el))
			recentf-list))))

(defun ilm-front-screen--recent-files ()
  "Retorna lista de arquivos recentes.")

(defsubst ilm--proj-relative (filename)
  "Retorna path de FILENAME relativo ao diretorio do projeto."
  (let ((proj-root (ilm--project-root filename)))
    (concat
	   (file-name-as-directory
	    (file-name-nondirectory (directory-file-name proj-root)))
	   (file-relative-name filename proj-root))))

(defvar ilm-front-screen-buffer-name "*ilm-front-screen*"
  "Nome do buffer usado para tela inicial.")

(defun ilm-front-screen-show ()
  "Printa tela inicial."
  (interactive)
  (let* ((inhibit-read-only t)
	 (buf (get-buffer-create ilm-front-screen-buffer-name))
	 (halfWidth (/ (window-width) 2))
	 (col (/ (window-width) 12))
	 (col2 (* 2 col))
	 (title " ILM -- dot EMACS")
	 (title-pad (- halfWidth (/ (length title) 2))))
    (with-current-buffer buf
      (org-mode)
      (erase-buffer)

      (insert "\n*")
      (insert title)

      (insert "\n\n**")
      (insert " *Projetos* \n")
      (mapc  #'(lambda (filename)
		 (insert (format "    - [[%s][%s]]\n"
				 filename
				 (file-name-nondirectory
				  (directory-file-name filename)))))
	     (ilm-front-screen--recent-projects))


      (insert "\n**")
      (insert " *Arquivos* \n")
      (mapc #'(lambda (filename)
		(let ((proj-root (ilm--project-root filename)))
		  (insert (format "    - %s [[%s][%s]]\n"
				  (all-the-icons-icon-for-file filename :v-adjust 0)
				  filename
				  (if proj-root
				      (ilm--proj-relative filename)
				    filename)))))
	    (recentf-elements 20)))))
;(seq-subseq recentf-list 0)
(defun ilm-front-screen-switch-buffer ()
  "Altera buffer para tela inicial."
  (let ((buf (get-buffer ilm-front-screen-buffer-name)))
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "ILM -- dot Emacs")
    ;(setq dashboard-icon-type 'all-the-icons)
    (setq dashboard-display-icons-p t)
    (setq dashboard-startup-banner nil)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-items '((projects . 5) (recents  . 5)))))

(provide 'ilm-front-screen)

;;; ilm-front-screen.el ends here
