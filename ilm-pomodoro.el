;;; ilm-pomodoro.el --- Pomodoro timer  -*- lexical-binding:t -*-
;; Copyright (C) 2017, 2021 Ildenir Barbosa

;; Author: Ildenir Barbosa <il.denir@gmail.com>
;; Created: sex 20 ago 2021 19:51:13 -03
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
(require 'cl-lib)
(require 'time-date)

(defgroup ilm-pomodoro nil
  "Manage pomodoro timer."
  :group 'ilm)

(defsubst ilm-pomodoro/-minutes (m)
  "Converte os minutos M em time."
  (seconds-to-time (* m 60)))

(defconst ilm-pomodoro/-zero-minute (ilm-pomodoro/-minutes 0))

(defvar ilm-pomodoro/-current-timer nil
  "Mantem timer corrent.")

(defvar ilm-pomodoro/-initial-time nil
  "Tempo inicial.")

(defcustom ilm-pomodoro/duration 20
  "Duracao de tempo em minutos setado para monitorar."
  :type 'number
  :group 'ilm-pomodoro)

(defvar ilm-pomodoro/-current-timer-type 'work
  "Tipo corrente do timer.")

(defcustom ilm-pomodoro-alarm-hook nil
  "Hook that gets run after duration has been ended."
  :type 'hook
  :group 'ilm-pomodoro)


(defvar ilm-pomodoro/-duration
  (ilm-pomodoro/-minutes ilm-pomodoro/duration)
  "Duracao de tempo setado para monitorar.")

(defun ilm-pomodoro/-show-time ()
  "Exibe tempo final."
  (message (format-time-string "%H:%M:%S" (time-since ilm-pomodoro/-initial-time))))

(defun ilm-pomodoro/elapse-time ()
  "Tempo decorrido depois do start.
Usado na powerline para exibir timer."
  (if ilm-pomodoro/-initial-time
      (format-time-string "%M:%S" (time-since ilm-pomodoro/-initial-time))
    nil))

(defun ilm-pomodoro/-next-timer ()
  "Inicia proximo timer."
  (ilm-pomodoro/-stop-timer))

(defun ilm-pomodoro/-end-time-interval ()
  "Termina intervalo de tempo."
  (let ((last-time (and ilm-pomodoro/-initial-time
			(time-add ilm-pomodoro/-initial-time
				  (ilm-pomodoro/-minutes ilm-pomodoro/duration))))
	(cur-time (current-time)))
    (if (and cur-time last-time (time-less-p cur-time last-time))
	(force-mode-line-update)
      (progn (ilm-pomodoro/-next-timer)
	     (run-hooks 'ilm-pomodoro-alarm-hook)))))

(defvar ilm-pomodoro/-monitor-timer nil)

(defun ilm-pomodoro/-start-monitor-timer ()
  "Inicializa timer auxiliar."
  (and ilm-pomodoro/-monitor-timer (cancel-timer ilm-pomodoro/-monitor-timer))
  (setq ilm-pomodoro/-monitor-timer
	(run-at-time nil 1 #'ilm-pomodoro/-end-time-interval)))

(defun ilm-pomodoro/-stop-monitor-timer ()
  "Para timer auxiliar."
  (and ilm-pomodoro/-monitor-timer (cancel-timer ilm-pomodoro/-monitor-timer))
  (setq ilm-pomodoro/-monitor-timer nil))

;; Start timer
(defun ilm-pomodoro/-start-timer ()
  "Inicia timer com tempo corrente."
  (ilm-pomodoro/-stop-timer)
  (setq ilm-pomodoro/-initial-time (current-time))
  (ilm-pomodoro/-start-monitor-timer))

;; Stop timer
(defun ilm-pomodoro/-stop-timer ()
  "Para timer corrente."
  (ilm-pomodoro/-stop-monitor-timer)
  (setq ilm-pomodoro/-initial-time nil))

;; Reset timer
(defun ilm-pomodoro/reset-timer ()
  "Reseta duracao dos timers."
  (setq ilm-pomodoro/-duration (read-string "Duracao Work Time (hh:mm:ss) :")))

(defface ilm-pomodoro/icon-face
  '((t :family "FontAwesome"
       :height 120
       ))
  "Fonte para icone do pomodoro")

(defconst ilm-pomodoro/-current-timer-type-icon-alist
  '((work . "\uf017")
    (interval . "\uf0f4"))
  )

(defun ilm-pomodoro/icon ()
  "Retorna icone de acordo com timer corrente."
  (if ilm-pomodoro/-initial-time
      (propertize (concat (cdr (assoc ilm-pomodoro/-current-timer-type
				      ilm-pomodoro/-current-timer-type-icon-alist))
			  " ")
		  'face 'ilm-pomodoro/icon-face)
    ""))

(defun ilm-pomodoro-timer-status ()
  "Retorna nil caso timer estiver parado."
  ilm-pomodoro/-initial-time)

;; Set timer
(defun ilm-pomodoro-set-timer (arg)
  "Inicia/para timer.
Caso ARG for nao maior q zero, inicia timer.
Caso contrario para timer."
  (interactive "p")
  (message (format  "Init timer %s" arg))
  (if (> arg 0)
      (ilm-pomodoro/-start-timer)
    (ilm-pomodoro/-stop-timer)))

(provide 'ilm-pomodoro)

;;; ilm-pomodoro.el ends here
