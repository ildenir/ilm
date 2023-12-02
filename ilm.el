;;; ilm.el --- Configuracoes pessoais  -*- lexical-binding:t -*-

;; Copyright (C) 2017, 2023 Ildenir Barbosa

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

(defgroup ilm nil
  "ilm configs."
  :group 'misc)

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

(require 'ilm-pomodoro)


;;;;;;;;;;;;;;;;;;;;;;
;; Package install  ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
	      ("org" . "http://orgmode.org/elpa/"))))
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

(ilm-install-packages 'use-package)


;;;;;;;;;;;;;;;;;;;;;;
;; Config packages  ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; vue-mode
(use-package vue-mode)

;; Markdown
(use-package markdown-mode)

;; Powerline

(use-package powerline
  :init
  (require 'ilm-powerline)
  (require 'ilm-misc)
  (ilm-mode-line)
  (set-face-attribute 'mode-line nil :box '(:line-width 4 :color "#444444"))
  (set-face-attribute 'mode-line-inactive nil
		      :box '(:line-width 4 :color "#444444")))

;; Neotree
(use-package neotree
	     :bind ("<f8>" . neotree-toggle))

;; Ivy
(use-package ivy
	     :init
	     (require 'ivy)
	     (ivy-mode)
	     (setq ivy-use-virtual-buffers t)
	     (setq enable-recursive-minibuffers t))

;; Yasnippet
(use-package yasnippet
	     :init
	     (require 'yasnippet)
	     (yas-global-mode 1))

;; Whitespace
(require 'whitespace)
(setq whitespace-action '(auto-cleanup))
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Org-mode
(use-package org-bullets
	     :init
	     (require 'org)
	     (setq org-startup-folded nil)
	     (require 'org-bullets)
	     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; Paredit
(use-package paredit
	     :init
	     (require 'paredit)
	     (add-hook 'lisp-mode-hook #'paredit-mode)
	     (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
	     (add-hook 'lisp-interaction-mode-hook #'paredit-mode))

;; Flycheck
(use-package flycheck
	     :init
	     (require 'flycheck)
	     (global-flycheck-mode))

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

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; eglot
(add-hook 'c-mode-hook #'eglot-ensure)

;; ilm-front-screen
(require 'ilm-front-screen)
;(require 'all-the-icons)

;; All-the-icons
(use-package all-the-icons
	     :init (require 'all-the-icons))

;; Magit
(use-package magit
	     :bind ( "C-x G" . magit-status))
;;
(use-package switch-window
	     :bind
	     (("C-x 1" . witch-window-then-maximize)
	      ("C-x 2" . witch-window-then-split-below)
	      ("C-x 3" . witch-window-then-split-right)
	      ("C-x 0" . vwitch-window-then-delete)))


(defun ilm--on-recentf-update ()
  "Atualiza tela inicial."
  (ilm-front-screen-show)
  (ilm-front-screen-switch-buffer))

(add-hook 'find-file-hook 'ilm-front-screen-show)
(add-hook 'recentf-mode-hook 'ilm--on-recentf-update)
(recentf-mode t)


;;;;;;;;;;;;;;;;;;
;; Keybindings  ;;
;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c w w") 'whitespace-mode)
(global-set-key (kbd "C-c w c") 'whitespace-cleanup)
(global-set-key (kbd "<f9>") 'other-frame)
(global-set-key (kbd "<f12>") 'recompile)

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

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)
(define-key lisp-mode-map (kbd "<f5>") 'slime-eval-buffer)


(global-set-key (kbd "<pause>") 'ilm-pomodoro-set-timer)

(provide 'ilm)

;;; ilm.el ends here
