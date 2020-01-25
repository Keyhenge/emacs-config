;;; exwm-icecat.el --- Icecat + EXWM     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Version: 0.1
;; URL: https://github.com/ieure/exwm-firefox
;; Package-Requires: ((emacs "25") (exwm "0.22.1") (exwm-icecat-core "20190608.2213"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds enhanced support for Icecat under EXWM.
;; Keybindings intentionally mirror other Emacs navigation controls.

;; To enable it, run M-x exwm-icecat-mode RET

;; - Navigate forwards (=C-c C-f=) and backwards (=C-c C-b=) in
;;   browsing history.
;; - Open a new window in an Emacs split (=C-c C-n=).
;; - Open a new private window in an Emacs split (=C-c C-p=).
;; - Detach the current tab into an Emacs split window (=C-c C-d=).
;;   Requires tabdetach extension.
;; - Merge the detached tab back into its parent window (=C-c C-=).
;;   Requires tabdetach extension.

;;; Code:

(load "~/.emacs.d/custom/exwm-icecat-core.el")
(require 'exwm-icecat-core)
(require 'ert)

(defconst exwm-icecat--title-re
  (rx bol
      ; Page title.  Optional because it's not set on blank pages.
      (optional (group (* anything)) " - ")
      ; Always present.
      (seq "GNU Icecat")
      ; Present in private windows
      (optional (group " (Private Browsing)"))
      eol)
  "Regular expression to ")

(defvar exwm-icecat--intercept nil
  "The function to call when a new Icecat window is created.")

(defvar exwm-icecat-keymap
  (let ((map (copy-keymap exwm-mode-map)))
    (define-key map "\C-c\C-f" 'exwm-icecat-core-history-forward)
    (define-key map "\C-c\C-b" 'exwm-icecat-core-history-back)
    (define-key map "\C-c\C-n" 'exwm-icecat-split-window)
    (define-key map "\C-c\C-p" 'exwm-icecat-split-private)
    (define-key map "\C-c\C-d" 'exwm-icecat-split-detach)
    (define-key map "\C-c\C-g" 'exwm-icecat-merge)
    map)
  "Keymap applied in Icecat windows.")

(defun exwm-icecat? ()
  "Does the current buffer contain a Icecat window?"
  (thread-first (string-match "^Icecat" (or exwm-class-name ""))
    (and t)
    (save-match-data)))

(defun exwm-icecat--setup-keymap-hook ()
  "Configure Icecat keymap for EXWM."
  (when (exwm-icecat?)
    (use-local-map exwm-icecat-keymap)))

(defun exwm-icecat--title->buffer-name (title)
  (concat "*"
          (save-match-data
            (if (string-match exwm-icecat--title-re title)
                (concat
                 "icecat"
                 (if (match-string 2 title) "-private" "")
                 (if-let ((page-title (match-string 1 title)))
                     (concat ": " page-title)
                   ""))
              "icecat"))
          "*"))

(defun exwm-icecat--update-title ()
  (when (exwm-icecat?)
    (let ((name (exwm-icecat--title->buffer-name exwm-title)))
      (unless (s-starts-with? name (buffer-name))
        (rename-buffer (generate-new-buffer-name name))))))

(defun exwm-icecat--split (old-window-config)
  "Move a new Icecat window into a split.

   OLD-WINDOW-CONFIG is the window confguration at the time the
   split window was created.

   Returns nil."
  (let ((new-icecat (current-buffer)))
    (set-window-configuration old-window-config)
    (switch-to-buffer-other-window new-icecat)
    (exwm-icecat-core-focus-search-bar)
    (setq exwm-icecat--intercept nil)))

(defun exwm-icecat--workspace (workspace)
  "Move a new Icecat window into WORKSPACE."
  (let ((new-icecat (current-buffer)))
    (exwm-workspace-move-window workspace)))

(defun exwm-icecat--intercept-hook ()
  "Run an action the next time a Icecat window is created."
  (if-let ((callback (and (exwm-icecat?) exwm-icecat--intercept)))
      (funcall callback)))

(defun exwm-icecat--intercept-next (data callback)
  "Perform an action the next time a Icecat window is created.

   Calls function CALLBACK with DATA as its only argument."

  (unless (exwm-icecat?)
    (error "Not a Icecat window"))
  (when exwm-icecat--intercept
    (warn "Already intercepting"))

  (setq exwm-icecat--intercept (apply-partially callback data))

  ;; If nothing happens in 3 seconds, reset the state
  (thread-last (lambda () (setq exwm-icecat--intercept nil))
    (run-with-timer 3 nil)))

(defun exwm-icecat-split-window ()
  "Create a new Icecat window in a split."
  (interactive)
  (exwm-icecat--intercept-next (current-window-configuration)
                                'exwm-icecat--split)
  (exwm-icecat-core-window-new))

(defun exwm-icecat-split-private (&optional arg)
  "Create a new Icecat private window in a split.

   With no ARG, create the new window in a split in the current workspace.

   With ARG prefix, display the window in that workspace."
  (interactive "P")
  (if arg
      (exwm-icecat--workspace arg)
    (exwm-icecat--intercept-next (current-window-configuration)
                                  'exwm-icecat--split))
  (exwm-icecat-core-window-new-private))

(defun exwm-icecat-split-detach (&optional arg)
  "Detach the current tab into a new split window.

   With no ARG, create the new window in a split in the current workspace.

   With ARG prefix, display the window in that workspace.

   This requires the tabdetach extension to work."
  (interactive "P")
  (if arg
      (exwm-icecat--workspace arg)
    (exwm-icecat--intercept-next (current-window-configuration)
                                  'exwm-icecat--split))
  (exwm-input--fake-key ?\M-\S-d))

(defun exwm-icecat-merge ()
  "Merge the current tab into its parent window.

   This requires the tabdetach extension to work."
  (interactive)
  (exwm-input--fake-key ?\M-\S-m))

(define-minor-mode exwm-icecat-mode
  "Minor mode to enhance Icecat in EXWM."
  nil nil nil
  :global t
  (setq exwm-icecat--intercept nil)
  (if exwm-icecat-mode
      (progn
        (add-hook 'exwm-manage-finish-hook 'exwm-icecat--setup-keymap-hook)
        (add-hook 'exwm-manage-finish-hook 'exwm-icecat--intercept-hook)
        (add-hook 'exwm-update-title-hook 'exwm-icecat--update-title))
    (remove-hook 'exwm-manage-finish-hook 'exwm-icecat--setup-keymap-hook)
    (remove-hook 'exwm-manage-finish-hook 'exwm-icecat--intercept-hook)))

(ert-deftest exwm-icecat--test--title->buffer-name ()
  (should (string= "*icecat*" (exwm-icecat--title->buffer-name "GNU Icecat")))
  (should (string= "*icecat-private*" (exwm-icecat--title->buffer-name "GNU Icecat (Private Browsing)")))
  (should (string= "*icecat: DuckDuckGo — Privacy, simplified.*"
             (exwm-icecat--title->buffer-name "DuckDuckGo — Privacy, simplified. - GNU Icecat")))
  (should (string= "*icecat: ieure/scratch-el: Scratch buffers for Emacs*"
             (exwm-icecat--title->buffer-name "ieure/scratch-el: Scratch buffers for Emacs - GNU Icecat"))))

(provide 'exwm-icecat)
;;; exwm-icecat.el ends here
