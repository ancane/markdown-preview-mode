;;; markdown-preview-mode.el --- markdown realtime preview.

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; URL: https://github.com/ancane/markdown-preview-mode
;; Keywords: markdown, preview
;; Package-Requires: ((websocket "1.3"))

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

(defgroup markdown-preview-mode nil
  "Markdown preview mode"
  :group 'text
  :prefix "mdpm:")

(defvar mdpm:websocket-port 7379)
(defvar mdpm:websocket-server nil)
(defvar mdpm:local-client nil)
(defvar mdpm:remote-clients nil)
(defvar mdpm:directory (file-name-directory load-file-name))

(defun mdpm:open-browser-preview ()
  (browse-url (concat mdpm:directory "preview.html")))

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
           mdpm:websocket-port
           :on-message (lambda (websocket frame)
                         (mapc (lambda (ws)
                                 (websocket-send-text ws
                                                      (websocket-frame-payload frame)))
                               mdpm:remote-clients))
           :on-open (lambda (websocket) (push websocket mdpm:remote-clients))
           :on-error (lambda (websocket type err) (message err))
           :on-close (lambda (websocket) (mdpm:drop-closed-clients))
           ))
    (add-hook 'kill-emacs-hook 'mdpm:stop-websocket-server)
    (mdpm:open-browser-preview)))

(defun mdpm:start-local-client ()
  (when (not mdpm:local-client)
    (setq mdpm:local-client
          (websocket-open
           (format "ws://localhost:%d" mdpm:websocket-port)
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket)
                       (setq mdpm:local-client nil))))))

(defun mdpm:send-preview ()
  (markdown markdown-output-buffer-name)
  (with-current-buffer (get-buffer markdown-output-buffer-name)
    (websocket-send-text mdpm:local-client (buffer-substring-no-properties (point-min) (point-max))))
  )

(defun mdpm:start ()
  (mdpm:start-websocket-server)
  (mdpm:start-local-client)
  (add-hook 'after-save-hook 'mdpm:send-preview nil t))

(defun mdpm:stop ()
  (remove-hook 'after-save-hook 'mdpm:send-preview t))

(defun markdown-preview-open-browser ()
  (interactive)
  (mdpm:open-browser-preview))

(defun markdown-preview-kill-websocket-server ()
  (interactive)
  (mdpm:stop-websocket-server))

(define-minor-mode markdown-preview-mode
  "Markdown preview mode"
  :group 'markdown-preview-mode
  :init-value nil
  (when (not (equal major-mode 'markdown-mode))
    (markdown-mode))
  (if markdown-preview-mode
      (mdpm:start)
    (mdpm:stop)))

(provide 'markdown-preview-mode)

;;; markdown-preview-mode.el ends here
