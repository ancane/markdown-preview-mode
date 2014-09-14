;;; markdown-preview-mode.el --- markdown realtime preview.

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
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
(defvar mdpm:websocket-clients nil)
(defvar mdpm:directory (file-name-directory load-file-name))

;; (setq mdpm:websocket-server (get-process "websocket server on port 7379"))

(defun mdpm:start-websocket-server ()
  (when (not mdpm:websocket-server)
    (setq mdpm:websocket-server
          (websocket-server
           mdpm:websocket-port
           :on-message (lambda (websocket frame)
                         (mapc (lambda (ws)
                                 ;; send frame only if ws != websocket
                                 (websocket-send-text
                                  ws
                                  (websocket-frame-payload frame))))
                         mdpm:websocket-clients)
           :on-open (lambda (websocket)
                      (push websocket mdpm:websocket-clients))
           :on-close (lambda (websocket)
                       (delete websocket mdpm:websocket-clients)))))
  (add-hook 'kill-emacs-hook 'mdpm:stop-websocket-server)
  (mdpm:open-browser))

(defun mdpm:open-browser ()
  (browse-url (concat mdpm:directory "preview.html")))

(defun mdpm:stop-websocket-server ()
  (when mdpm:websocket-server
    (delete-process mdpm:websocket-server)
    (setq mdpm:websocket-server nil
          mdpm:websocket-clients nil)))

(defun mdpm:send ())

(defun mdpm:start ()
  (mdpm:start-websocket-server)
  (add-hook 'after-save-hook 'mdpm:send nil t))

(defun mdpm:stop ()
  (remove-hook 'after-save-hook 'mdpm:send))

(defun markdown-preview-open-browser ()
  (interactive)
  (mdpm:open-browser))

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
