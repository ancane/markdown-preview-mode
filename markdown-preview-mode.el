;;; markdown-preview-mode.el --- markdown realtime preview minor mode.

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; URL: https://github.com/ancane/markdown-preview-mode
;; Keywords: markdown, preview
;; Package-Requires: ((websocket "1.3") (markdown-mode "2.0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'websocket)
(require 'markdown-mode)

(defgroup markdown-preview nil
  "Markdown preview mode"
  :group 'text
  :prefix "markdown-preview-"
  :link '(url-link "https://github.com/ancane/markdown-preview-mode"))

(defcustom markdown-preview-port 7379
  "Markdown preview websocket server port"
  :group 'markdown-preview
  :type 'integer)

(defcustom markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-dark.min.css"
  "Markdown preview style URI"
  :group 'markdown-preview
  :type 'string)

(defvar markdown-preview--websocket-server nil)
(defvar markdown-preview--local-client nil)
(defvar markdown-preview--remote-clients nil)
(defvar markdown-preview--preview-url (concat (file-name-directory load-file-name) "preview.html"))
(defvar markdown-preview--idle-timer nil "Preview idle timer")

(defun markdown-preview--stop-idle-timer ()
  (when (timerp markdown-preview--idle-timer)
    (cancel-timer markdown-preview--idle-timer)))

(defun markdown-preview--open-browser-preview ()
  (browse-url markdown-preview--preview-url))

(defun markdown-preview--stop-websocket-server ()
  (when markdown-preview--local-client
    (websocket-close markdown-preview--local-client))
  (when markdown-preview--websocket-server
    (delete-process markdown-preview--websocket-server)
    (setq markdown-preview--websocket-server nil
          markdown-preview--remote-clients nil)))

(defun markdown-preview--drop-closed-clients ()
  (setq markdown-preview--remote-clients
        (cl-remove-if-not #'websocket-openp markdown-preview--remote-clients)))

(defun markdown-preview--start-websocket-server ()
  (when (not markdown-preview--websocket-server)
    (setq markdown-preview--websocket-server
          (websocket-server
           markdown-preview-port
           :on-message (lambda (websocket frame)
                         (mapc (lambda (ws)
                                 (websocket-send-text ws
                                                      (websocket-frame-payload frame)))
                               markdown-preview--remote-clients))
           :on-open (lambda (websocket)
                      (push websocket markdown-preview--remote-clients)
                      (markdown-preview--send-preview-to websocket))
           :on-error (lambda (websocket type err) (message (concat "====> Error:" err)))
           :on-close (lambda (websocket) (markdown-preview--drop-closed-clients))))
    (add-hook 'kill-emacs-hook 'markdown-preview--stop-websocket-server)
    (markdown-preview--open-browser-preview)))

(defun markdown-preview--start-local-client ()
  (when (not markdown-preview--local-client)
    (setq markdown-preview--local-client
          (websocket-open
           (format "ws://localhost:%d" markdown-preview-port)
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket)
                       (setq markdown-preview--local-client nil))))))

(defun markdown-preview--send-preview ()
  (when (bound-and-true-p markdown-preview-mode)
    (markdown-preview--send-preview-to markdown-preview--local-client)))

(defun markdown-preview--send-preview-to (websocket)
  (let ((mark-position-percent
         (number-to-string
          (truncate
           (* 100
              (/
               (float (-  (line-number-at-pos) (/ (count-screen-lines (window-start) (point)) 2)))
               (count-lines (point-min) (point-max))))))))
    (when markdown-preview-mode
      (markdown markdown-output-buffer-name))
    (with-current-buffer (get-buffer markdown-output-buffer-name)
      (websocket-send-text websocket
                           (concat
                            "<div>"
                            "<span id='style'>"
                            markdown-preview-style
                            "</span>"
                            "<span id='position-percentage'>"
                            mark-position-percent
                            "</span>"
                            "<div id='content'>"
                            (buffer-substring-no-properties (point-min) (point-max))
                            "</div>"
                            "</div>")
                           ))))

(defun markdown-preview--start ()
  (markdown-preview--start-websocket-server)
  (markdown-preview--start-local-client)
  (setq markdown-preview--idle-timer
        (run-with-idle-timer 2 t 'markdown-preview--send-preview))
  (add-hook 'after-save-hook 'markdown-preview--send-preview nil t)
  (add-hook 'kill-buffer-hook 'markdown-preview--stop))

(defun markdown-preview--stop ()
  (remove-hook 'after-save-hook 'markdown-preview--send-preview t)
  (markdown-preview--stop-idle-timer))

(defun markdown-preview-open-browser ()
  (interactive)
  (markdown-preview--open-browser-preview))

(defun markdown-preview-cleanup ()
  (interactive)
  (markdown-preview--stop-websocket-server))

;;;###autoload
(define-minor-mode markdown-preview-mode
  "Markdown preview mode"
  :group 'markdown-preview-mode
  :init-value nil
  (when (not (or
              (equal major-mode 'markdown-mode)
              (equal major-mode 'gfm-mode)))
    (markdown-mode))
  (if markdown-preview-mode
      (markdown-preview--start)
    (markdown-preview--stop)))

(provide 'markdown-preview-mode)

;;; markdown-preview-mode.el ends here
