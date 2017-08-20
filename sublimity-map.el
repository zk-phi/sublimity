;;; sublimity-map.el --- minimap

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
;; Version: 2.0.0

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
;; 1.0.8 add option sublimity-map-text-scale
;; 2.0.0 rewrite almost everything for better performance

;;; Code:

(require 'cl-lib)
(require 'sublimity)
(defconst sublimity-map-version "2.0.0")

;; + customs

(defcustom sublimity-map-size 17
  "width of the minimap"
  :type 'integer
  :group 'sublimity)

(defcustom sublimity-map-max-fraction 0.3
  "maximum width that the minimap can get"
  :type 'number
  :group 'sublimity)

(defcustom sublimity-map-text-scale -8
  "font rescale for the minimap"
  :type 'integer
  :group 'sublimity)

(defcustom sublimity-map-active-region 'highlight
  "face for the active region"
  :type 'face
  :group 'sublimity)

(defcustom sublimity-map-current-line 'cursor
  "face for the current line"
  :type 'face
  :group 'sublimity)

(defcustom sublimity-map-criteria
  '((not (window-minibuffer-p))
    (or (derived-mode-p 'prog-mode)
        (derived-mode-p 'text-mode)
        (derived-mode-p 'css-mode))
    (<= (/ sublimity-map-size (window-total-width) 1.0)
        sublimity-map-max-fraction))
  "sexps that must be evaluated to non-nil when creating minimap"
  :type 'sexp
  :group 'sublimity)

(defcustom sublimity-map-setup-hook nil
  "hook run just after the minimap is activated, with the minimap
selected."
  :type 'hook
  :group 'sublimity)

;; + obsolete variables

(dolist (var '(sublimity-map-on-scroll
               sublimity-map-on-commands
               sublimity-map-keep-commands))
  (eval `(defvar ,var nil))
  (make-obsolete-variable
   var "now minimap is updated in post-command-hook."
   "sublimity-map 2.0.0"))

;; + internal variables

;; global value : the minimap window
(defvar sublimity-map--window nil)

;; normal buffers : the minimap buffer for this buffer
;; minimap buffers : the buffer this minimap buffer is for
(defvar sublimity-map--buffer nil)
(make-variable-buffer-local 'sublimity-map--buffer)

;; variables for minimap bufers
(defvar sublimity-map--minimap-buffer-p nil)
(defvar sublimity-map--active-overlay nil)
(defvar sublimity-map--current-overlay nil)
(make-variable-buffer-local 'sublimity-map--minimap-buffer-p)
(make-variable-buffer-local 'sublimity-map--active-overlay)
(make-variable-buffer-local 'sublimity-map--current-overlay)

;; + create/kill the minimap

(defun sublimity-map--delete-window ()
  "Kill the minimap window."
  (when (window-live-p sublimity-map--window)
    (let* ((partner (window-parameter
                     sublimity-map--window 'sublimity-map-partner))
           (margin1 (window-margins partner))
           (margin2 (window-margins sublimity-map--window))
           (fringe1 (window-fringes partner))
           (fringe2 (window-fringes sublimity-map--window)))
      (delete-window sublimity-map--window)
      (set-window-margins partner (car margin1) (cdr margin2))
      (set-window-fringes partner (car fringe1) (cadr fringe2))
      (setq sublimity-map--window nil))))

(defun sublimity-map--split-window ()
  "Make a minimap window."
  ;; make sure that the old one is killed
  (sublimity-map--delete-window)
  ;; split new one off
  (let* ((basewin (selected-window))
         (margin (window-margins basewin))
         (fringe (window-fringes basewin)))
    (set-window-margins basewin (car margin) 0)
    (set-window-fringes basewin (car fringe) 0)
    (let ((win (split-window
                basewin (- (+ (or (cdr margin) 0) sublimity-map-size)) t)))
      (set-window-margins win 0 (cdr margin))
      (set-window-fringes win 0 (cadr fringe))
      (set-window-parameter win 'sublimity-map-partner basewin)
      (setq sublimity-map--window win))))

(defun sublimity-map--generate-buffer (base)
  "Make minimap buffer for this buffer."
  (let ((ind (make-indirect-buffer
              base (concat " *minimap/" (buffer-name base) "*"))))
    (with-current-buffer ind
      (setq vertical-scroll-bar             nil
            truncate-lines                  t
            buffer-read-only                t
            mode-line-format                (and mode-line-format "")
            sublimity-map--minimap-buffer-p t
            sublimity-map--buffer           base
            sublimity-map--active-overlay   (make-overlay 0 0)
            sublimity-map--current-overlay  (make-overlay 0 0))
      (set (make-local-variable 'auto-hscroll-mode) nil)
      (overlay-put sublimity-map--active-overlay
                   'face sublimity-map-active-region)
      (overlay-put sublimity-map--current-overlay
                   'face sublimity-map-current-line)
      (text-scale-set sublimity-map-text-scale)
      (run-hooks 'sublimity-map-setup-hook))
    (setq sublimity-map--buffer ind)))

(defun sublimity-map--update (basewin miniwin minibuf)
  "Sync window informations to minimap."
  (let ((region-beg (window-start basewin))
        (region-end (window-end basewin t))
        (point (point)))
    (with-selected-window miniwin
      (set-window-buffer nil minibuf t)
      (with-current-buffer minibuf
        (goto-char point)
        (recenter)
        (move-overlay sublimity-map--active-overlay
                      region-beg region-end)
        (move-overlay sublimity-map--current-overlay
                      (point-at-bol) (line-beginning-position 2))))))

(defun sublimity-map-show ()
  "Update or create minimap for the current buffer."
  (interactive)
  (when (and sublimity-mode
             (cl-every 'eval sublimity-map-criteria))
    (unless (and (window-live-p sublimity-map--window)
                 (eq (window-parent) (window-parent sublimity-map--window)))
      (sublimity-map--split-window))
    (unless (buffer-live-p sublimity-map--buffer)
      (sublimity-map--generate-buffer (current-buffer)))
    (when (and (window-live-p sublimity-map--window)
               (buffer-live-p sublimity-map--buffer))
      (sublimity-map--update
       (selected-window) sublimity-map--window sublimity-map--buffer))))

(defun sublimity-map-kill ()
  "Kill the minimap window."
  (interactive)
  (sublimity-map--delete-window))

;; + kill-buffer hook

(defun sublimity-map--kill-buffer-hook ()
  (when (and (not sublimity-map--minimap-buffer-p)
             ;; when the buffer being killed is a buffer generated
             ;; by "with-temp-buffer", its safe to keep minimap active
             (not (string= (buffer-name) " *temp*")))
    (when (and (window-live-p sublimity-map--window)
               (eq (window-parent)
                   (window-parent sublimity-map--window)))
      (sublimity-map--delete-window))
    (when (buffer-live-p sublimity-map--buffer)
      (kill-buffer sublimity-map--buffer))))

(add-hook 'kill-buffer-hook 'sublimity-map--kill-buffer-hook)

;; + add hooks

(add-hook 'sublimity--pre-command-functions 'sublimity-map--delete-window)

;; kill minimap buffer on "kill-all-local-variables"
(add-hook 'change-major-mode-hook 'sublimity-map--kill-buffer-hook)

;; + run with timer

(defvar sublimity-map--timer
  (run-with-idle-timer 1 t 'sublimity-map-show))

(defun sublimity-map-set-delay (sec)
  (cond ((null sec)
         (when sublimity-map--timer
           (cancel-timer sublimity-map--timer))
         (add-hook 'sublimity--post-command-functions
                   'sublimity-map-show t))
        (t
         (remove-hook 'sublimity--post-command-functions
                      'sublimity-map-show)
         (when sublimity-map--timer
           (cancel-timer sublimity-map--timer))
         (setq sublimity-map--timer
               (run-with-idle-timer sec t 'sublimity-map-show)))))

;; + provide

(provide 'sublimity-map)

;;; sublimity-map.el ends here
