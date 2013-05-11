;;; sublimity.el --- smooth-scrolling and minimap, like sublime editor

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

;;; Commentary:

;; Require this script.
;;
;; (require 'sublimity)
;;
;; Now "M-x sublimity-scroll" will load smooth-scrolling, and "M-x
;; sublimity-map" will load minimap. If you want emacs always load them,
;; put code like
;;
;; (when (require 'sublimity nil t)
;;   (sublimity-scroll)
;;   (sublimity-map)))
;;
;; into your init file.

;; For more informations, see "Readme".

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(defconst sublimity-version "1.0.0")

;; * customs

(defgroup sublimity nil
  "smooth-scrolling and minimap, like sublime editor"
  :group 'emacs)

;; * children

(defvar sublimity-scroll nil)
(defvar sublimity-map nil)

;;;###autoload
(defun sublimity-scroll ()
  (interactive)
  (setq sublimity-scroll (require 'sublimity-scroll nil t)))

;;;###autoload
(defun sublimity-map ()
  (interactive)
  (setq sublimity-map (require 'sublimity-map nil t)))

;; * sublimity common vars, functions

(defvar sublimity-auto-hscroll-mode auto-hscroll-mode)
(setq auto-hscroll-mode nil)

(defvar sublimity--pre-command-functions nil
  "pre-command functions")

(defvar sublimity--post-command-functions nil
  "like post-command-hook but not called when the buffer or window is switched.")

(defvar sublimity--post-vscroll-functions nil
  "called with number of lines, when vertical scroll is occurred.")

(defvar sublimity--post-hscroll-functions nil
  "called with number of columns, when horizontal scroll is occurred.")

(defvar sublimity--prev-lin (line-number-at-pos (window-start)))
(defvar sublimity--prev-col (window-hscroll))
(defvar sublimity--prev-buf (current-buffer))
(defvar sublimity--prev-wnd (selected-window))

(defun sublimity--should-be-quiet ()
  (or (not (eq sublimity--prev-buf (current-buffer)))
      (not (eq sublimity--prev-wnd (selected-window)))
      (and (boundp 'cua--rectangle) cua--rectangle)
      (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
      (eq major-mode 'shell-mode)))

(defun sublimity--horizontal-recenter ()
  (let ((cols (- (current-column) (window-hscroll) (/ (window-width) 2))))
    (if (< cols 0)
        (scroll-right (- cols))
      (scroll-left cols))))

(defun sublimity--pre-command ()
  (setq sublimity--prev-lin (line-number-at-pos (window-start))
        sublimity--prev-col (window-hscroll)
        sublimity--prev-buf (current-buffer)
        sublimity--prev-wnd (selected-window))
  (run-hooks 'sublimity--pre-command-functions))

(defun sublimity--post-command ()
  (let (deactivate-mark)
    (unless (sublimity--should-be-quiet)
      ;; do vscroll
      (when (or (< (point) (window-start))
                (>= (point) (window-end)))
        (recenter))
      ;; do hscroll
      (when (and sublimity-auto-hscroll-mode
                 (or truncate-lines
                     (truncated-partial-width-window-p))
                 (or (< (current-column) (window-hscroll))
                     (< (+ (window-hscroll) (window-width)) (current-column))))
        (sublimity--horizontal-recenter))
      ;; call post-command functions
      (let ((lins (- (line-number-at-pos (window-start))
                     sublimity--prev-lin))
            (cols (- (window-hscroll) sublimity--prev-col)))
        (run-hooks 'sublimity--post-command-functions)
        (when (not (zerop lins))
          (run-hook-with-args 'sublimity--post-vscroll-functions lins))
        (when (not (zerop cols))
          (run-hook-with-args 'sublimity--post-hscroll-functions cols))))))

(add-to-list 'pre-command-hook 'sublimity--pre-command t)
(add-to-list 'post-command-hook 'sublimity--post-command t)

;; * provide

(provide 'sublimity)

;;; sublimity.el ends here
