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
;; Version: 1.0.0

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 fixed minibuffer bug

;;; Code:

(require 'sublimity)
(defconst sublimity-map-version "1.0.0")

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

;; * vars

(defvar sublimity-map-setup-hook '((lambda () (text-scale-set -7)))
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
  (unless (sublimity-map--live-p)
    (when (setq sublimity-map--window
                (when (<= (/ sublimity-map-size (window-width) 1.0)
                          sublimity-map-fraction)
                  (split-window (selected-window)
                                (- (window-width) sublimity-map-size) t)))
      (let ((str (buffer-string)))
        (with-selected-window sublimity-map--window
          (switch-to-buffer
           (setq sublimity-map--buffer (generate-new-buffer "*nurumap*")))
          (insert str)
          (run-hooks 'sublimity-map-setup-hook)))))
  (when (sublimity-map--live-p)
    (let ((point (point))
          (beg (window-start))
          (end (window-end nil t)))
      (with-selected-window sublimity-map--window
       (when sublimity-map--overlay
         (delete-overlay sublimity-map--overlay))
       (overlay-put (make-overlay beg end) 'face 'highlight)
       (goto-char point)
       (recenter)))))

;; * trigger

(defvar sublimity-map--timer
  (run-with-idle-timer 2 t 'sublimity-map--idle))

(defun sublimity-map-set-delay (secs)
  (timer-set-idle-time sublimity-map--timer secs t))

(defun sublimity-map--pre-command ()
  (sublimity-map--kill))

(defun sublimity-map--post-vscroll (_)
  (when sublimity-map-on-scroll
    (sublimity-map--update)))

(defun sublimity-map--idle ()
  (unless (window-minibuffer-p)
   (sublimity-map--update)))

(add-hook 'sublimity--post-vscroll-functions 'sublimity-map--post-vscroll)
(add-hook 'sublimity--pre-command-functions 'sublimity-map--pre-command)

;; * provide

(provide 'sublimity-map)

;;; sublimity-map.el ends here
