;;; sublimity-map.el --- minimap

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.7

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 fixed minibuffer bug
;; 1.0.2 consider window-margins
;; 1.0.3 configurable font-size for the minimap
;;       option disable minimap when idling
;;       added sublimity-map-on-commands
;; 1.0.4 disable idle-timer while sublimity-mode is off
;; 1.0.5 cancel idle-timer when sublimity-mode is turned off
;; 1.0.6 add automargin.el workaround
;; 1.0.7 add option sublimity-map-keep-commands

;;; Code:

(require 'sublimity)
(defconst sublimity-map-version "1.0.6")

;; * customs

(defcustom sublimity-map-size 20
  "width of minimap"
  :group 'sublimity)

(defcustom sublimity-map-fraction 0.3
  "maximum fraction of minimap width"
  :group 'sublimity)

(defcustom sublimity-map-on-scroll t
  "if minimap should be automatically displayed on scroll"
  :group 'sublimity)

(defcustom sublimity-map-on-commands nil
  "commands after which the minimap should be displayed"
  :group 'sublimity)

(defcustom sublimity-map-keep-commands
  '(previous-line next-line forward-char backward-char
                  forward-sexp backward-sexp
                  forward-word backward-word
                  subword-forward subword-backward)
  "commands which should not kill the minimap"
  :group 'sublimity)

;; * vars

(defvar sublimity-map-setup-hook nil
  "hooks that are called just after minimap is activated
you may assume (selected-window) and (current-buffer) are minimap")

;; * map

(defvar sublimity-map--window nil)
(defvar sublimity-map--buffer nil)
(defvar sublimity-map--overlay nil)

(defun sublimity-map--kill ()
  (when (buffer-live-p sublimity-map--buffer)
    (kill-buffer sublimity-map--buffer))
  (when (window-live-p sublimity-map--window)
    (delete-window sublimity-map--window))
  (when sublimity-map--overlay
    (setq sublimity-map--overlay nil)))

(defun sublimity-map--live-p ()
  (if (and (buffer-live-p sublimity-map--buffer)
           (window-live-p sublimity-map--window)) t
    (sublimity-map--kill)
    nil))

(defun sublimity-map--update ()
  (let* ((margins (window-margins))
         (width (+ (window-width) (or (car margins) 0) (or (cdr margins) 0))))
    (unless (window-minibuffer-p)
      ;; make window
      (unless (sublimity-map--live-p)
        (when (setq sublimity-map--window
                    (when (<= (/ sublimity-map-size width 1.0)
                              sublimity-map-fraction)
                      (split-window (selected-window)
                                    (- width sublimity-map-size) t)))
          (let ((str (buffer-string)))
            (with-selected-window sublimity-map--window
              (switch-to-buffer
               (setq sublimity-map--buffer (generate-new-buffer "*nurumap*")))
              (insert str)
              (text-scale-set -7)
              (run-hooks 'sublimity-map-setup-hook)))))
      ;; update
      (when (sublimity-map--live-p)
        (let ((point (point))
              (beg (window-start))
              (end (window-end nil t)))
          (with-selected-window sublimity-map--window
            (when sublimity-map--overlay
              (delete-overlay sublimity-map--overlay))
            (overlay-put (make-overlay beg end) 'face 'highlight)
            (goto-char point)
            (recenter)))))))

;; * trigger

(defvar sublimity-map--delay nil)
(defvar sublimity-map--timer nil)

(defun sublimity-map-set-delay (secs)
  "set sublimity-map delay to SECs. if secs is 'inf, disable minimap when idling."
  (setq sublimity-map--delay secs)
  (cond ((not sublimity-map--timer)
         (or (eq secs 'inf)
             (setq sublimity-map--timer
                   (run-with-idle-timer secs t 'sublimity-map--idle))))
        ((eq secs 'inf)
         (cancel-timer sublimity-map--timer)
         (setq sublimity-map--timer nil))
        (t
         (timer-set-idle-time sublimity-map--timer secs t))))

(defun sublimity-map--restore-timer ()
  (when sublimity-map--delay
    (sublimity-map-set-delay sublimity-map--delay)))

(defun sublimity-map--pre-command ()
  (unless (member this-command sublimity-map-keep-commands)
   (sublimity-map--kill)))

(defun sublimity-map--post-command ()
  (when (member this-command sublimity-map-on-commands)
    (sublimity-map--update)))

(defun sublimity-map--post-vscroll (_)
  (when sublimity-map-on-scroll
    (sublimity-map--update)))

(defun sublimity-map--idle ()
  (if sublimity-mode
      (sublimity-map--update)
    (cancel-timer sublimity-map--timer)
    (setq sublimity-map--timer nil)))

(add-hook 'sublimity-mode-hook 'sublimity-map--restore-timer)
(add-hook 'sublimity--post-vscroll-functions 'sublimity-map--post-vscroll)
(add-hook 'sublimity--pre-command-functions 'sublimity-map--pre-command)
(add-hook 'sublimity--post-command-functions 'sublimity-map--post-command)

;; * automargin.el workaround

(eval-after-load 'automargin
  '(defadvice sublimity-map--update
     (around sublimity-map-automargin-workaround activate)
     (flet ((automargin-function () nil))
       ad-do-it)))

;; * provide

(provide 'sublimity-map)

;;; sublimity-map.el ends here
