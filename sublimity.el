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
;; Version: 1.1.1

;;; Commentary:

;; Require this script and some of "sublimity-scroll" "sublimity-map".
;;
;;   (require 'sublimity)
;;   (require 'sublimity-scroll)

;;   (require 'sublimity-map)
;;
;; then call command "M-x sublimity-mode".

;; If you want to enable sublimity everywhere, call function
;; sublimity-global-mode.
;;
;;   (sublimity-global-mode)

;; For more informations, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.1.0 turned into minor-mode
;; 1.1.1 added sublimity-mode-hook

;;; Code:

(defconst sublimity-version "1.1.1")

;; * customs

(defgroup sublimity nil
  "smooth-scrolling and minimap, like sublime editor"
  :group 'emacs)

(defcustom sublimity-mode-hook nil
  "hook run when entering sublimity-mode"
  :group 'sublimity)

;; * minor mode

(defvar sublimity-auto-hscroll-mode auto-hscroll-mode)

(define-minor-mode sublimity-mode
  "smooth-scrolling and minimap, like sublime editor"
  :init-value nil
  :global nil
  (if sublimity-mode
      (progn
        (setq auto-hscroll-mode nil)
        (run-hooks 'sublimity-mode-hook))
    (setq auto-hscroll-mode sublimity-auto-hscroll-mode)))

(define-globalized-minor-mode sublimity-global-mode
  sublimity-mode
  (lambda () (sublimity-mode 1)))

;; * commands (for backward compatibility)

;;;###autoload
(defun sublimity-scroll ()
  (interactive)
  (require 'sublimity-scroll nil t))

;;;###autoload
(defun sublimity-map ()
  (interactive)
  (require 'sublimity-map nil t))

;; * sublimity common vars, functions

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
  (let ((cols (- (current-column)
                 (window-hscroll)
                 (/ (window-width) 2))))
    (if (< cols 0)
        (scroll-right (- cols))
      (scroll-left cols))))

(defun sublimity--pre-command ()
  (when sublimity-mode
    (setq sublimity--prev-lin (line-number-at-pos (window-start))
          sublimity--prev-col (window-hscroll)
          sublimity--prev-buf (current-buffer)
          sublimity--prev-wnd (selected-window))
    (run-hooks 'sublimity--pre-command-functions)))

(defun sublimity--post-command ()
  (when (and sublimity-mode
             (not (sublimity--should-be-quiet)))
    (let (deactivate-mark)
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

(add-hook 'pre-command-hook 'sublimity--pre-command)
(add-hook 'post-command-hook 'sublimity--post-command t)

;; * workaround (FIXME)

(defadvice sublimity--pre-command (around sublimity--workaround activate)
  (condition-case err
      ad-do-it
    (error (progn (message "an error occurred in sublimity--pre-command.")
                  (sit-for 1)))))

(defadvice sublimity--post-command (around sublimity--workaround activate)
  (condition-case err
      ad-do-it
    (error (progn (message "an error occurred in sublimity--post-command.")
                  (sit-for 1)))))

;; * provide

(provide 'sublimity)

;;; sublimity.el ends here
