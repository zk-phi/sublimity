;;; sublimity-scroll.el --- smooth-scrolling

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
;; Version: 1.1.0

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 modified the default value of speeds
;; 1.1.0 changed algorithm for smooth-scrolling

;;; Code:

(require 'sublimity)
(defconst sublimity-scroll-version "1.1.0")

;; * customs

(defcustom sublimity-scroll-weight1 10
  "this defines the granularity of scroll speeds.
basically, if this is set larger, scroll ends more smoothly."
  :group 'sublimity)

(defcustom sublimity-scroll-weight2 1.7
  "this defines how the scroll speed goes down.
basically, if this is set larger, long scroll become slower."
  :group 'sublimity)

;; * utils

(defun sublimity-scroll--vscroll (lins)
  "FOR ANIMATION USE ONLY"
  (goto-char (window-start))
  (forward-line lins)
  (set-window-start nil (point)))

(defun sublimity-scroll--hscroll (cols)
  (if (< cols 0)
      (scroll-right (- cols))
    (scroll-left cols)))

;; * animation

(defun sublimity-scroll--gen-speeds (amount)
  (let ((amount2 (/ amount sublimity-scroll-weight2))
        (base sublimity-scroll-weight1))
    (cond ((< amount 0)
           (mapcar '- (sublimity-scroll--gen-speeds (- amount))))
          ((zerop amount)
           '())
          ((< amount2 base)
           (make-list amount '1))
          (t
           (let* ((spd (expt base (floor (log amount2 base))))
                  (times (floor (/ amount2 spd)))
                  (rest (- amount (* times spd))))
             (append (make-list times spd)
                     (sublimity-scroll--gen-speeds rest)))))))

(defun sublimity-scroll--vscroll-effect (lins)
  (save-excursion
    (let ((speeds (sublimity-scroll--gen-speeds lins)))
      (sublimity-scroll--vscroll (- lins))
      (dolist (speed speeds)
        (sublimity-scroll--vscroll speed)
        (redisplay t)))))

(defun sublimity-scroll--hscroll-effect (cols)
  (save-excursion
    (let ((speeds (sublimity-scroll--gen-speeds cols)))
      (sublimity-scroll--hscroll (- cols))
      (dolist (speed speeds)
        (sublimity-scroll--hscroll speed)
        (redisplay t)))))

;; * triggers

(defun sublimity-scroll--post-vscroll (lins)
  (sublimity-scroll--vscroll-effect lins))

(defun sublimity-scroll--post-hscroll (cols)
  (sublimity-scroll--hscroll-effect cols))

(add-hook 'sublimity--post-vscroll-functions
          'sublimity-scroll--post-vscroll)

(add-hook 'sublimity--post-hscroll-functions
          'sublimity-scroll--post-hscroll)

;; * provide

(provide 'sublimity-scroll)

;;; sublimity-scroll.el ends here
