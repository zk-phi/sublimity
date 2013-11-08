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
;; Version: 1.2.0

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 modified the default value of speeds
;; 1.1.0 changed algorithm for smooth-scrolling
;; 1.2.0 scroll is now faster in very long buffers
;;       and easier to configure

;;; Code:

(require 'sublimity)
(require 'cl-lib)
(eval-when-compile (require 'cl))

(defconst sublimity-scroll-version "1.2.0")

;; * customs

(defcustom sublimity-scroll-weight 4
  "scroll is maybe divided into N small scrolls"
  :group 'sublimity)

(defcustom sublimity-scroll-drift-length 6
  "scroll last N lines especially slowly"
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
  (let (a lst)
    (flet ((fix-list (lst &optional eax)
                     (if (null lst) nil
                       (let* ((rem (car lst))
                              (val (floor rem))
                              (rem (+ (- rem val) (or eax 0)))
                              (val (if (>= rem 1) (1+ val) val))
                              (rem (if (>= rem 1) (1- rem) rem)))
                         (cons val (fix-list (cdr lst) rem))))))
      (cond ((integerp sublimity-scroll-weight)
             (setq sublimity-scroll-weight (float sublimity-scroll-weight))
             (sublimity-scroll--gen-speeds amount))
            ((< amount 0)
             (mapcar '- (sublimity-scroll--gen-speeds (- amount))))
            ((< amount sublimity-scroll-drift-length)
             (make-list amount 1))
            (t
             (setq amount (- amount sublimity-scroll-drift-length))
             ;; x = a t (t+1) / 2 <=> a = 2 x / (t^2 + t)
             (setq a (/ (* 2 amount)
                        (+ (expt (float sublimity-scroll-weight) 2)
                           sublimity-scroll-weight)))
             (dotimes (n sublimity-scroll-weight)
               (setq lst (cons (* a (1+ n)) lst)))
             (append (cl-remove-if 'zerop (sort (fix-list lst) '>))
                     (make-list sublimity-scroll-drift-length 1)))))))

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
