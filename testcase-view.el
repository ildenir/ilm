;;; testcase-view.el --- Exibe testes do projeto em um buffer -*- lexical-binding: t -*-
;; Author: Ildenir Barbosa <ildenir@gmail.com>
;; Keywords: unittest
;; Package: emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Este eh um major-mode para exibir/navegar/editar testes unitarios
;; do  projeto.
;;

;;; Code:
;;

;;; Variaveis customizaveis

(require 'outline)

(defgroup testcase-view nil
  "Testcase view/browser"
  :group 'misc)

(defcustom testcase-view-test-data nil
  "Nome do arquivo de dados gerado pelo gtest."
  :type 'string
  :group 'testcase-view)

;;; Hook variables

(defcustom testcase-view-load-from-project nil
  "Carrega dados do projeto corrente.
Ao finalizar carga, variavel testcase-view-data deve conter
os novos dados."
  :group 'testcase-view
  :type 'hook)



;;; Commands

(defconst testcase-view--status-prettyprint
  '((ok . "ok")
    (fail . "fail")
    (nil . "none"))
  "Alist tranforma codigo status em prettyprint.")

(defvar testcase-view-buffer "*TestCaseView*"
  "Buffer onde view eh visualizada.")

;; testcase (testname (list testcase))
;; testcase := testname passed (list testcase-children)
;; testcase-children := testcase|nil
;; passed := ok | fail | nil
;; ("BankAccount" ()
(defvar testcase-view-data
  '("Tests" nil (("Addition" nil (("canAddTwoNumber" nil nil)))
		 ("BankAccountTest" nil (("canDepositMoney" nil nil)))
		 ("silver " nil (("add" nil nil)
				 ("sub" nil nil)
				 ("block" nil nil))))))

(defun testcase-view--get-test-children (node)
  (nth 2 node))

(defun testcase-view--get-testname (node)
  (nth 0 node))

(defun testcase-view--get-teststatus (node)
  (nth 1 node))

(defun testcase-view--level (node)
  "Retorna nivel do NODE na arvore."
  (let ((tovisit (list (cons testcase-view-data  0)))
	(found nil)
	curr
	level
	currnode)
    (while (not found)
      (setq curr (pop tovisit)
	    currnode (car curr)
	    level (cdr curr))
      (if (eql currnode node)
	  (setq found t)
	(progn
	  (dolist (e (testcase-view--get-test-children currnode))
	    (push (cons e (1+ level)) tovisit))
	  )))
    level))

(defun testcase-view--print-data (node)
  "Percorre NODE e print de acordo com arvore."
  (let* ((children    (testcase-view--get-test-children node))
	 (testname    (testcase-view--get-testname node))
	 (test-status (testcase-view--get-teststatus node))
	 (status      (cdr (assoc test-status
				  testcase-view--status-prettyprint))))
    (when node
      (insert (make-string (1+ (testcase-view--level node )) ?*))
      (insert (make-string (1+ (testcase-view--level node )) ?\s))
      (insert (format " %s %s \n" testname status)))
    (dolist (e children)
      (testcase-view--print-data e))))

(defun testcase-view--build-buffer ()
  "Escreve arvore de casos de teste."
  (run-hooks testcase-view-load-from-project)
  (testcase-view--print-data testcase-view-data))

(defun testcase-view-do-revert ()
  "Atualiza buffer com dados de teste."
  (interactive)
  (let ((buf (get-buffer-create testcase-view-buffer))
	(inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (save-excursion
	(testcase-view--build-buffer)))))


;;; Keybinding
(defvar testcase-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [?\t] 'outline-toggle-children)
    (define-key map (kbd "g") 'testcase-view-do-revert)
    (define-key map (kbd "+") 'testcase-view-do-add-test)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "a") 'outline-show-all)
    map))


;;; Mode define
(define-derived-mode testcase-view-mode special-mode
  "testcase-view"
  (use-local-map testcase-view-mode-map))

(defun testcase-view ()
  "Exibe buffer com estes do diretorio corrente."
  (interactive)
  (let ((buf (get-buffer-create testcase-view-buffer))
	)
    (switch-to-buffer buf)
    (testcase-view-mode)
    (testcase-view-do-revert)
    (read-only-mode)
    (outline-minor-mode)))

(provide 'testcase-view)

;;; testcase-view.el ends here
