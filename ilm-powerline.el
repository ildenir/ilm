;;; ilm-powerline.el --- config powerline

;;; Commentary:
;;

;;; Code:

(require 'powerline)

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
			  (center (list (powerline-raw "%m" face0 'l)))
			  (rhs (list (powerline-raw "(%l" face0 'l)
				 (powerline-raw ":" face0 'l)
				 (powerline-raw "%c)" face0 'l)
				 (powerline-raw (concat "#" (number-to-string (ilm--loc)) " ")
						face0 'l)))
			  (lhs (list
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
				 (when active
				   (powerline-raw (concat
						   (ilm-pomodoro/icon)
						   (ilm-pomodoro/elapse-time)))
				   ))))
		     (concat  (powerline-render lhs)
			      (powerline-fill-center face0 (/ (powerline-width center) 2.0))
			      (powerline-render center)
			      (powerline-fill face0 (powerline-width rhs))
			      (powerline-render rhs)))))))

(provide 'ilm-powerline)

;;; ilm-powerline.el ends here
