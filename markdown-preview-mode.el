;;; markdown-preview-mode.el --- markdown realtime preview minor mode.

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; URL: https://github.com/ancane/markdown-preview-mode
;; Keywords: markdown, preview
;; Package-Requires: ((websocket "1.3") (markdown-mode "2.0"))

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

(eval-when-compile (require 'cl))

(require 'websocket)
(require 'markdown-mode)

(defgroup markdown-preview nil
  "Markdown preview mode"
  :group 'text
  :prefix "mdpm:"
  :link '(url-link "https://github.com/ancane/markdown-preview-mode"))

(defcustom markdown-preview-port 7379
  "Markdown preview websocket server port"
  :group 'markdown-preview
  :type 'integer)

(defcustom markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-dark.min.css"
  "Markdown preview style URI"
  :group 'markdown-preview
  :type 'string)

(defvar mdpm:websocket-server nil)
(defvar mdpm:local-client nil)
(defvar mdpm:remote-clients nil)
(defvar mdpm:preview-url (concat (file-name-directory load-file-name) "preview.html"))
(defvar mdpm:idle-timer nil "Preview idle timer")

(defun mdpm:stop-idle-timer ()
  (when (timerp mdpm:idle-timer)
    (cancel-timer mdpm:idle-timer)))

(defun mdpm:open-browser-preview ()
  (browse-url mdpm:preview-url))

(defun mdpm:stop-websocket-server ()
  (when mdpm:local-client
    (websocket-close mdpm:local-client))
  (when mdpm:websocket-server
    (delete-process mdpm:websocket-server)
    (setq mdpm:websocket-server nil
          mdpm:remote-clients nil)))

(defun mdpm:drop-closed-clients ()
  (setq mdpm:remote-clients
        (remove-if-not #'websocket-openp mdpm:remote-clients)))

(defun mdpm:start-websocket-server ()
  (when (not mdpm:websocket-server)
    (setq mdpm:websocket-server
          (websocket-server
           markdown-preview-port
           :on-message (lambda (websocket frame)
                         (mapc (lambda (ws)
                                 (websocket-send-text ws
                                                      (websocket-frame-payload frame)))
                               mdpm:remote-clients))
           :on-open (lambda (websocket)
                      (push websocket mdpm:remote-clients)
                      (mdpm:send-preview-to websocket))
           :on-error (lambda (websocket type err) (message (concat "====> Error:" err)))
           :on-close (lambda (websocket) (mdpm:drop-closed-clients))))
    (add-hook 'kill-emacs-hook 'mdpm:stop-websocket-server)
    (mdpm:open-browser-preview)))

(defun mdpm:start-local-client ()
  (when (not mdpm:local-client)
    (setq mdpm:local-client
          (websocket-open
           (format "ws://localhost:%d" markdown-preview-port)
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket)
                       (setq mdpm:local-client nil))))))

(defun mdpm:send-preview ()
  (when (bound-and-true-p markdown-preview-mode)
    (mdpm:send-preview-to mdpm:local-client)))

(defun mdpm:send-preview-to (websocket)
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

(defun mdpm:start ()
  (mdpm:start-websocket-server)
  (mdpm:start-local-client)
  (setq mdpm:idle-timer
        (run-with-idle-timer 2 t 'mdpm:send-preview))
  (add-hook 'after-save-hook 'mdpm:send-preview nil t)
  (add-hook 'kill-buffer-hook 'mdpm:stop))

(defun mdpm:stop ()
  (remove-hook 'after-save-hook 'mdpm:send-preview t)
  (mdpm:stop-idle-timer))

(defun markdown-preview-open-browser ()
  (interactive)
  (mdpm:open-browser-preview))

(defun markdown-preview-cleanup ()
  (interactive)
  (mdpm:stop-websocket-server))

(define-minor-mode markdown-preview-mode
  "Markdown preview mode"
  :group 'markdown-preview-mode
  :init-value nil
  (when (not (or
              (equal major-mode 'markdown-mode)
              (equal major-mode 'gfm-mode)))
    (markdown-mode))
  (if markdown-preview-mode
      (mdpm:start)
    (mdpm:stop)))

(provide 'markdown-preview-mode)

;;; markdown-preview-mode.el ends here
