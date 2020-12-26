;;; ilm.el --- Configuracoes pessoais

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
		      'htmlize 'flycheck 'org-bullets 'column-enforce-mode)


;;;;;;;;;;;;;;;;;;;;;;
;; Config packages  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Powerline
(require 'powerline)
(powerline-default-theme)


(defun ilm-loc ()
  "Numero de linhas do buffer."
  (count-lines (point-min) (point-max)))

(defun ilm-mode-line ()
  "Mode line simplificada para modos comuns como cc-mode.
Usa powerline para outros modos."
  (interactive)
  (setq mode-line-format
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
		  (lhs (list
			(when (buffer-modified-p) (powerline-raw "%*" face0 'l))
			(if (buffer-file-name) (powerline-raw "%f" face0 'l)
			  "%b")
			(powerline-raw " " face0)
			(funcall separator-left face0 face1)
			(powerline-raw "%l" face1 'l)
			(powerline-raw ":" face1 'l)
			(powerline-raw "%c" face1 'l)

			(funcall separator-left face1 face2)
			(powerline-raw "#" face2 'l)
			(powerline-raw (number-to-string (ilm-loc)) face2 'l)
			(funcall separator-left face2 face0))))
	     (concat (powerline-render lhs)))))))

(add-hook 'c-mode-hook #'ilm-mode-line)
(add-hook 'c++-mode-hook #'ilm-mode-line)
(add-hook 'lisp-mode-hook #'ilm-mode-line)
(add-hook 'lisp-interaction-mode-hook #'ilm-mode-line)

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
