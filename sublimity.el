;;; sublimity.el --- smooth-scrolling, minimap and distraction-free mode

;; Copyright (C) 2013-2015 zk_phi

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
;; Version: 1.1.3

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
;; 1.1.0 turn into minor-mode
;; 1.1.1 add sublimity-mode-hook
;; 1.1.2 add sublimity-handle-scroll-criteria
;;       remove add-hook in toplevel
;;       make sublimity-mode global
;; 1.1.3 scroll-bar workaround

;;; Code:

(defconst sublimity-version "1.1.3")

;; + customs

(defgroup sublimity nil
  "smooth-scrolling and minimap, like sublime editor"
  :group 'emacs)

(defcustom sublimity-mode-hook nil
  "hook run when entering sublimity-mode"
  :group 'sublimity)

(defcustom sublimity-handle-scroll-criteria
  '((eq sublimity--prev-buf (current-buffer))
    (eq sublimity--prev-wnd (selected-window))
    (or (not (boundp 'cua--rectangle)) (not cua--rectangle))
    (or (not (boundp 'multiple-cursors-mode)) (not multiple-cursors-mode))
    (not (eq major-mode 'shell-mode))
    (not (memq this-command '(scroll-bar-drag
                              scroll-bar-toolkit-scroll
                              scroll-bar-scroll-up
                              scroll-bar-scroll-down))))
  "if any of the sexps evaluates to nil, sublimity does not
handle scrolling."
  :group 'sublimity)

;; + minor mode

(defvar sublimity-auto-hscroll-mode nil)

;;;###autoload
(define-minor-mode sublimity-mode
  "smooth-scrolling and minimap, like sublime editor"
  :init-value nil
  :global t
  (cond (sublimity-mode
         (setq sublimity-auto-hscroll-mode auto-hscroll-mode)
         (setq auto-hscroll-mode nil)
         (add-hook 'pre-command-hook 'sublimity--pre-command nil)
         (add-hook 'post-command-hook 'sublimity--post-command t)
         (add-hook 'window-configuration-change-hook 'sublimity--window-change t)
         (run-hooks 'sublimity-mode-hook))
        (t
         (remove-hook 'pre-command-hook 'sublimity--pre-command)
         (remove-hook 'post-command-hook 'sublimity--post-command)
         (remove-hook 'window-configuration-change-hook 'sublimity--window-change)
         (setq auto-hscroll-mode sublimity-auto-hscroll-mode))))

;; + sublimity common vars, functions

(defvar sublimity--pre-command-functions nil)
(defvar sublimity--post-command-functions nil)
(defvar sublimity--window-change-functions nil)
(defvar sublimity--post-vscroll-functions nil
  "called with number of lines, when vertical scroll is occurred.")
(defvar sublimity--post-hscroll-functions nil
  "called with number of columns, when horizontal scroll is occurred.")

(defvar sublimity--prepared nil)
(defvar sublimity--prev-lin (line-number-at-pos (window-start)))
(defvar sublimity--prev-col (window-hscroll))
(defvar sublimity--prev-buf (current-buffer))
(defvar sublimity--prev-wnd (selected-window))

(defun sublimity--run-hooks (hook &optional arg)
  (let* ((sublimity--window-change-functions nil))
    (if arg
        (run-hook-with-args 'hook arg)
      (run-hooks 'hook))))

(defun sublimity--horizontal-recenter ()
  ;; NOT accurate for some propertized texts.
  (let ((cols (- (current-column)
                 (window-hscroll)
                 (/ (window-width) 2))))
    (if (< cols 0)
        (scroll-right (- cols))
      (scroll-left cols))))

;; + hook functions

(defun sublimity--pre-command ()
  (setq sublimity--prev-lin (line-number-at-pos (window-start))
        sublimity--prev-col (window-hscroll)
        sublimity--prev-buf (current-buffer)
        sublimity--prev-wnd (selected-window)
        sublimity--prepared t)
  (sublimity--run-hooks sublimity--pre-command-functions))

(defun sublimity--post-command ()
  ;; avoid running post-command multiple times
  (when sublimity--prepared
    (setq sublimity--prepared nil)
    (let ((handle-scroll (cl-every 'eval sublimity-handle-scroll-criteria)))
      (when handle-scroll
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
                         (< (+ (window-hscroll) (window-width))
                            (current-column))))
            (sublimity--horizontal-recenter))))
      ;; call post-command functions
      (sublimity--run-hooks sublimity--post-command-functions)
      ;; animation
      (when handle-scroll
        (let ((lins (- (line-number-at-pos (window-start))
                       sublimity--prev-lin))
              (cols (- (window-hscroll) sublimity--prev-col)))
          (when (not (zerop lins))
            (sublimity--run-hooks sublimity--post-vscroll-functions lins))
          (when (not (zerop cols))
            (sublimity--run-hooks sublimity--post-hscroll-functions cols)))))))

(defun sublimity--window-change ()
  (sublimity--run-hooks sublimity--window-change-functions))

;; * provide

(provide 'sublimity)

;;; sublimity.el ends here
