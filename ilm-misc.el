;;; ilm-misc.el --- Rotinas comuns

;;; Commentary:
;;

;;; Code:
;;
(require 'seq)

(defun ilm--loc ()
  "Numero de linhas do buffer."
  (count-lines (point-min) (point-max)))

(defun ilm--proj-p (dir)
  "Verifica se DIR possui caracteristicas de projeto."
  (let* ((files '("Makefile" ".git" "makefile" ".svn"))
	 (pf (mapcar #'(lambda (el) (expand-file-name el dir )) files)))
    (seq-some #'file-exists-p pf)))

(defun ilm--project-root (dir)
  "Raiz do projeto de DIR ou nil."
  (let ((nextdir dir)
	(home (getenv "HOME"))
	(out nil))
    (while (and (not (null dir))
		(not (string= nextdir (file-name-as-directory home))))
      (when (ilm--proj-p nextdir) (setq out nextdir))
      (setq nextdir (file-name-directory (directory-file-name nextdir))))
    out))



(provide 'ilm-misc)

;;; ilm-misc.el ends here
