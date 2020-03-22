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
  "Instala pacotes se necessario"
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

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-<left>") 'scroll-up-line )
(global-set-key (kbd "M-<right>") 'scroll-down-line )
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(provide 'ilm)

;;; ilm.el ends here
