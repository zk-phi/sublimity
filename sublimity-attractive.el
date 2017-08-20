;;; sublimity-attractive.el --- hide distractive objects

;; Copyright (C) 2013- zk_phi

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
;; URL: https://github.com/zk-phi/sublimity
;; Version: 1.0.0

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(require 'sublimity)
(defconst sublimity-attractive-version "1.0.0")

(defcustom sublimity-attractive-centering-width 110
  "When non-nil, buffer width is truncated to this value and
display centered."
  :type 'integer
  :group 'sublimity)

(defun sublimity-attractive-hide-bars ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun sublimity-attractive-hide-vertical-border ()
  (interactive)
  (set-face-foreground 'vertical-border (face-background 'default)))

(defun sublimity-attractive-hide-fringes ()
  (interactive)
  (set-face-background 'fringe (face-background 'default))
  (set-face-foreground 'fringe (face-background 'default)))

(defun sublimity-attractive-hide-modelines ()
  (interactive)
  (setq-default mode-line-format nil))

(defun sublimity-attractive--window-width (window)
  (let ((margins (window-margins window)))
    (+ (window-width window) (or (car margins) 0) (or (cdr margins) 0))))

(defun sublimity-attractive-window-change ()
  (let ((windows (window-list)))
    ;; process minimap window first
    (when (and (boundp 'sublimity-map--window)
               (window-live-p sublimity-map--window))
      (let* ((left sublimity-map--window)
             (right (window-parameter left 'sublimity-map-partner))
             (target (with-selected-window left
                       sublimity-attractive-centering-width))
             (margin (and target
                          (max (/ (- (+ (sublimity-attractive--window-width left)
                                        (sublimity-attractive--window-width right))
                                     target) 2) 0))))
        (when margin
          (set-window-margins left 0 margin)
          (set-window-margins right margin 0))
        (setq windows (delq right (delq left windows)))))
    ;; process other windows
    (dolist (window windows)
      (unless (window-minibuffer-p window)
        (let* ((target (with-selected-window window
                         sublimity-attractive-centering-width))
               (margin (and target
                            (max (/ (- (sublimity-attractive--window-width window)
                                       target) 2) 0))))
          (when margin
            (set-window-margins window margin margin)))))))

(defun sublimity-attractive--clear-window-margins ()
  (dolist (window (window-list))
    (set-window-margins window 0 0)))

(add-hook 'sublimity-mode-turn-off-hook 'sublimity-attractive--clear-window-margins t)
(add-hook 'sublimity--window-change-functions 'sublimity-attractive-window-change t)

;; + provide

(provide 'sublimity-attractive)

;;; sublimity-attractive.el ends here
