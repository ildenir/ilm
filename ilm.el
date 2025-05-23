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
  "ILM configs."
  :group 'misc)

(setq inhibit-startup-message t)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
(setq warning-minimum-level :emergency)

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
  "Instala pacotes PACKS se necessario."
  (dolist (pack packs)
    (message (format "Processando %s" (symbol-name pack)))
    (unless (package-installed-p pack)
      (message (format "Installing package %s" (symbol-name pack)))
      (condition-case nil
	  (package-install pack t)
	(error (message "error handling") (package-refresh-contents) (package-install pack))))))

(ilm-install-packages 'use-package 'column-enforce-mode)


;;;;;;;;;;;;;;;;;;;;;;
;; Config packages  ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; vue-mode
(use-package vue-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t)

;; Powerline

(use-package powerline
  :ensure t
  :config
  (require 'ilm-powerline)
  (require 'ilm-misc)
  (ilm-mode-line)
  (set-face-attribute 'mode-line nil :box '(:line-width 4 :color "#444444"))
  (set-face-attribute 'mode-line-inactive nil
		      :box '(:line-width 4 :color "#444444")))

;; Neotree
(use-package neotree
  :ensure t
	     :bind ("<f8>" . neotree-toggle))

;; Ivy
(use-package ivy
  :ensure t
	     :config
	     (require 'ivy)
	     (ivy-mode)
	     (setq ivy-use-virtual-buffers t)
	     (setq enable-recursive-minibuffers t))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet)
  (setq yas-snippet-dirs
	`(,(expand-file-name "~/Projetos/yasnippet-snippets/snippets")
	  ,(expand-file-name "~/.ilm.d/snippets")))
  (yas-global-mode 1))

;; Whitespace
(use-package whitespace
  :ensure t
  :config
  (require 'whitespace)
  (setq whitespace-action '(auto-cleanup))
  :hook (before-save . whitespace-cleanup))

;; Org-mode
(use-package org-bullets
  :ensure t
	     :config
	     (require 'org)
	     (setq org-startup-folded nil)
	     (require 'org-bullets)
	     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Paredit
(use-package paredit
  :ensure t
	     :config
	     (require 'paredit)
	     (add-hook 'lisp-mode-hook #'paredit-mode)
	     (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
	     (add-hook 'lisp-interaction-mode-hook #'paredit-mode))

;; Flycheck
(use-package flycheck
  :ensure t
	     :config
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
(use-package company
  :ensure t
  :defer t
  :config
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-yasnippet))

;; eglot
(use-package eglot
  :ensure t
  :defer t
  :hook (c-mode . eglot-ensure))

;; All-the-icons
(use-package all-the-icons
  :ensure t
	     :config (require 'all-the-icons))

;; ilm-front-screen
(require 'ilm-front-screen)

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind ( "C-x G" . magit-status))

;;
(use-package switch-window
  :ensure t
  :defer t
  :bind
  (("C-x 1" . switch-window-then-maximize)
   ("C-x 2" . switch-window-then-split-below)
   ("C-x 3" . switch-window-then-split-right)
   ("C-x 0" . switch-window-then-delete)))


(defun ilm--on-recentf-update ()
  "Atualiza tela inicial."
  (ilm-front-screen-show)
  (ilm-front-screen-switch-buffer))

(add-hook 'find-file-hook 'ilm-front-screen-show)
(add-hook 'recentf-mode-hook 'ilm--on-recentf-update)
(recentf-mode t)

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   ))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


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
