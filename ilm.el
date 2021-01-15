;;; ilm.el --- Configuracoes pessoais  -*- lexical-binding:t -*-

;; Copyright (C) 2017, 2021 Ildenir Barbosa

;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: Sat Jul 29 21:52:29 2017
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


;;;;;;;;;;;;;;;;;;
;; Config Emacs ;;
;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(load-theme 'wombat)
(windmove-default-keybindings)
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 '(powerline-default-separator (quote wave))
 '(blink-cursor-mode nil))

(custom-set-faces
 '(cursor ((t (:background "dark orange")))))

(require 'fringe)
(set-fringe-style '(nil . 0))


;;;;;;;;;;;;;;;;;;;;;;
;; Package install  ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
	      ("org" . "http://orgmode.org/elpa/")
	      ("marmalade" . "http://marmalade-repo.org/packages/"))))
  (dolist (repo repos)
    (add-to-list 'package-archives repo)))


(defun ilm-install-packages(&rest packs)
  "Instala pacotes se necessario."
  (dolist (pack packs)
    (message (format "Processando %s" (symbol-name pack)))
    (unless (package-installed-p pack)
      (message (format "Installing package %s" (symbol-name pack)))
      (condition-case nil
	  (package-install pack t)
	(error (message "error handling") (package-refresh-contents) (package-install pack))))))

(ilm-install-packages 'projectile 'helm 'helm-projectile 'powerline 'yasnippet
		      'magit 'markdown-mode 'switch-window 'paredit
		      'htmlize 'flycheck 'org-bullets 'column-enforce-mode
		      'all-the-icons)


;;;;;;;;;;;;;;;;;;;;;;
;; Config packages  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Powerline
(require 'powerline)
(require 'ilm-misc)

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
			  (line (list
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
				 (funcall separator-left face0 face1)
				 (powerline-raw "%m" face1 'l)
				 (funcall separator-left face1 face2)
				 (powerline-raw "%l" face2 'l)
				 (powerline-raw ":" face2 'l)
				 (powerline-raw "%c" face2 'l)
				 (powerline-raw "LOC" face2 'l)
				 (powerline-raw (number-to-string (ilm--loc)) face2 'l)
				 (funcall separator-left face2 face0))))
		     (powerline-render line))))))
(ilm-mode-line)
(set-face-attribute 'mode-line nil :box '(:line-width 4 :color "#444444"))
(set-face-attribute 'mode-line-inactive nil
		    :box '(:line-width 4 :color "#444444"))

;; Projectile
(require 'projectile)
(projectile-mode)
(add-hook 'before-save-hook #'projectile-regenerate-tags)
(setq tags-revert-without-query t)
(define-key projectile-mode-map (kbd "s-,") 'projectile-command-map)

;; Helm
;; Referencia: tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; Projectile Helm integration
(require 'helm-projectile)
(helm-projectile-on)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Whitespace
(require 'whitespace)
(setq whitespace-action '(auto-cleanup))
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Org-mode
(require 'org)
(setq org-startup-folded nil)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; Paredit
(require 'paredit)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)

;; Colunm enforce
(setq column-enforce-comments nil)
(add-hook 'lisp-mode-hook #'column-enforce-mode)
(add-hook 'emacs-lisp-mode-hook #'column-enforce-mode)
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
				    (outline-minor-mode t)
				    (setq outline-regexp ";;;\s+")))
(add-hook 'lisp-interaction-mode-hook #'column-enforce-mode)
(add-hook 'c-mode-hook #'column-enforce-mode)
(add-hook 'prog-mode-hook #'column-enforce-mode)


;;;;;;;;;;;;;;;;;;
;; Keybindings  ;;
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x G") 'magit-status)
(global-set-key (kbd "C-c w w") 'whitespace-mode)
(global-set-key (kbd "C-c w c") 'whitespace-cleanup)
(global-set-key (kbd "<f9>") 'other-frame)
(global-set-key (kbd "<f12>") 'recompile)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-<left>") 'scroll-up-line )
(global-set-key (kbd "M-<right>") 'scroll-down-line )
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'cc-mode)
(define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)

(require 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)

(provide 'ilm)

;;; ilm.el ends here
