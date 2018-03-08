;;; init.el --- distribuicao pessoal emacs -*- lexical-binding:t -*-

;; Copyright (C) 2017 Ildenir Barbosa

;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: Sat Jul 29 21:52:29 2017
;; Version: 0.0
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


;;;;;;;;;;;;;;;;;;;;;;
;; Package install  ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq user-emacs-directory (expand-file-name "~/.ilm.d"))
(setq package-user-dir user-emacs-directory)

(package-initialize)
(let ((repos '(("melpa" . "http://melpa.org/packages/")
	      ("org" . "http://orgmode.org/elpa/")
	      ("marmalade" . "http://marmalade-repo.org/packages/"))))
  (dolist (repo repos)
    (add-to-list 'package-archives repo)))


(defun ilm-install-packages(&rest packs)
  "Instala pacotes se necessario"
  (dolist (pack packs)
    (message (format "Processando %s" (symbol-name pack)))
    (unless (package-installed-p pack)
      (message (format "Installing package %s" (symbol-name pack)))
      (condition-case condition
	  (package-install pack t)
	(error (message "error handlin") (package-refresh-contents) (package-install pack))))))

(ilm-install-packages 'projectile 'helm 'helm-projectile 'powerline 'yasnippet
		      'magit 'markdown-mode 'switch-window 'paredit
		      'htmlize 'flycheck)

(condition-case condit (package-install 'projectile)
  (error (package-refresh-contents)))


;;;;;;;;;;;;;;;;;;;;;;
;; Config packages  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Projectile
(require 'projectile)
(projectile-mode)
(add-hook 'before-save-hook #'projectile-regenerate-tags)
(setq tags-revert-without-query t)

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
(setq org-startup-folded nil)

;; Paredit
(require 'paredit)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)


;;;;;;;;;;;;;;;;;;
;; Keybindings  ;;
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x G") 'magit-status)
(global-set-key (kbd "C-c w w") 'whitespace-mode)
(global-set-key (kbd "C-c w c") 'whitespace-cleanup)
(global-set-key (kbd "<f9>") 'other-frame)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

;;; init.el ends here
