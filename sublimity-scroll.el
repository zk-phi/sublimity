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
;; Version: 1.0.0

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(require 'sublimity)
(defconst sublimity-scroll-version "1.0.0")

;; * customs

(defcustom sublimity-scroll-vdecc 1.7
  "how vertical scroll speed goes down"
  :group 'sublimity)

(defcustom sublimity-scroll-vspeeds '(1000 300 100 50 7 3)
  "speeds of vertical scrolling animation"
  :group 'sublimity)

(defcustom sublimity-scroll-hdecc 1.5
  "how horizontal scroll speed goes down"
  :group 'sublimity)

(defcustom sublimity-scroll-hspeeds '(400 200 10)
  "speeds of horizontal scrolling animation"
  :group 'sublimity)

;; * utils

(defun sublimity-scroll--vscroll (lines)
  "FOR ANIMATION USE ONLY"
  (goto-char (window-start))
  (forward-line lines)
  (set-window-start nil (point)))

(defun sublimity-scroll--hscroll (cols)
  (if (< cols 0)
      (scroll-right (- cols))
    (scroll-left cols)))

;; * animation

(defun sublimity-scroll--animate (amount fun speeds decc)
  (save-window-excursion
    (save-excursion
      (let* ((fun (if (>= amount 0) fun
                    `(lambda (amount) (,fun (- amount)))))
             (abs (abs amount))
             (cursor-type nil))
        (funcall fun (- abs))
        (dolist (spd speeds)
          (while (>= abs (floor (* decc spd)))
            (funcall fun spd)
            (setq abs (- abs spd))
            (redisplay t)))
        (dotimes (tmp abs)
          (funcall fun 1)
          (redisplay t))))))

(defun sublimity-scroll--vscroll-effect (lins)
  (sublimity-scroll--animate lins
                             'sublimity-scroll--vscroll
                             sublimity-scroll-vspeeds
                             sublimity-scroll-vdecc))

(defun sublimity-scroll--hscroll-effect (lins)
  (sublimity-scroll--animate lins
                             'sublimity-scroll--hscroll
                             sublimity-scroll-hspeeds
                             sublimity-scroll-hdecc))

;; * triggers

(defun sublimity-scroll--post-vscroll (lines)
  (sublimity-scroll--vscroll-effect lines))

(defun sublimity-scroll--post-hscroll (lines)
  (sublimity-scroll--hscroll-effect lines))

(add-hook 'sublimity--post-vscroll-functions
          'sublimity-scroll--post-vscroll)

(add-hook 'sublimity--post-hscroll-functions
          'sublimity-scroll--post-hscroll)

;; * provide

(provide 'sublimity-scroll)

;;; sublimity-scroll.el ends here
