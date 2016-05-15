;;; helm-hatena-bookmark.el --- Hatena::Bookmark with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-hatena-bookmark
;; Version: 2.2.0
;; Package-Requires: ((helm "1.9.5"))

;; This program is free software: you can redistribute it and/or modify
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
;; helm-hatena-bookmark.el provides a helm interface to Hatena::Bookmark.

;;; Code:

(require 'helm)

(defgroup helm-hatena-bookmark nil
  "Hatena::Bookmark with helm interface"
  :prefix "helm-hatena-bookmark-"
  :group 'helm)

(defcustom helm-hatena-bookmark-username nil
  "A username of your Hatena account."
  :type '(choice (const nil)
		 string)
  :group 'helm-hatena-bookmark)

(define-obsolete-variable-alias 'helm-hatena-bookmark:username
  'helm-hatena-bookmark-username "2.2.0")

(defcustom helm-hatena-bookmark-file "~/.hatenabookmark"
  "A cache file of your Hatena::Bookmark."
  :type '(choice (const nil)
		 string)
  :group 'helm-hatena-bookmark)

(defcustom helm-hatena-bookmark-candidate-number-limit 10000
  "Candidate number limit."
  :type 'integer
  :group 'helm-hatena-bookmark)

(defcustom helm-hatena-bookmark-interval (* 1 60 60)
  "Number of seconds to call `helm-hatena-bookmark-http-request'."
  :type 'integer
  :group 'helm-hatena-bookmark)

(define-obsolete-variable-alias 'helm-hatena-bookmark:interval
  'helm-hatena-bookmark-interval "2.2.0")

(defvar helm-hatena-bookmark-url nil
  "Cache a result of `helm-hatena-bookmark-get-url'.
DO NOT SET VALUE MANUALLY.")

(defvar helm-hatena-bookmark-curl-program nil
  "Cache a result of `helm-hatena-bookmark-find-curl-program'.
DO NOT SET VALUE MANUALLY.")

(defvar helm-hatena-bookmark-sed-program nil
  "Cache a result of `helm-hatena-bookmark-find-sed-program'.
DO NOT SET VALUE MANUALLY.")

(defvar helm-hatena-bookmark-http-buffer-name " *helm-hatena-bookmark*"
  "Working buffer name of `helm-hatena-bookmark-http-request'.")

(defvar helm-hatena-bookmark-full-frame helm-full-frame)

(defvar helm-hatena-bookmark-timer nil
  "Timer object for Hatena::Bookmark caching will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar helm-hatena-bookmark-debug-mode nil)
(defvar helm-hatena-bookmark-debug-start-time nil)

(defun helm-hatena-bookmark-load ()
  "Load `helm-hatena-bookmark-file'."
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents helm-hatena-bookmark-file))))

(defvar helm-hatena-bookmark-action
  '(("Browse URL" . helm-hatena-bookmark-browse-url)
    ("Show URL" . helm-hatena-bookmark-show-url)
    ("Show Summary" . helm-hatena-bookmark-show-summary)))

(defun helm-hatena-bookmark-browse-url (candidate)
  "Action for Browse URL.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[href:\\(.+\\)\\]$" candidate)
  (browse-url (match-string 1 candidate)))

(defun helm-hatena-bookmark-show-url (candidate)
  "Action for Show URL.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[href:\\(.+\\)\\]$" candidate)
  (message (match-string 1 candidate)))

(defun helm-hatena-bookmark-show-summary (candidate)
  "Action for Show Summary.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[summary:\\(.+\\)\\]\\[" candidate)
  (message (match-string 1 candidate)))

(defvar helm-hatena-bookmark-source
  (helm-build-in-buffer-source "Hatena:Bookmark"
    :init 'helm-hatena-bookmark-load
    :action 'helm-hatena-bookmark-action
    :candidate-number-limit helm-hatena-bookmark-candidate-number-limit
    :multiline t
    :migemo t)
  "Helm source for Hatena::Bookmark.")

(define-obsolete-variable-alias 'helm-hatena-bookmark:source
  'helm-hatena-bookmark-source "2.2.0")

;;;###autoload
(defun helm-hatena-bookmark ()
  "Search Hatena::Bookmark using `helm'."
  (interactive)
  (let ((helm-full-frame helm-hatena-bookmark-full-frame))
    (unless (file-exists-p helm-hatena-bookmark-file)
      (error (format "%s not found" helm-hatena-bookmark-file)))
    (helm :sources helm-hatena-bookmark-source
	  :prompt "Find Bookmark: ")))

(defun helm-hatena-bookmark-find-curl-program ()
  "Return an appropriate `curl' program pathname or error if not found."
  (or
   (executable-find "curl")
   (error "Cannot find `curl' helm-hatena-bookmark.el requires")))

(defun helm-hatena-bookmark-find-sed-program ()
  "Return an appropriate `sed' program pathname or error if not found."
  (executable-find
   (cond
    ((eq system-type 'darwin)
     (unless (executable-find "gsed")
       (error "Cannot find `gsed' helm-hatena-bookmark.el requires.  (example `$ brew install gnu-sed')"))
     "gsed")
    (t
     "sed"))))

(defun helm-hatena-bookmark-get-url ()
  "Return Hatena::Bookmark URL or error if `helm-hatena-bookmark-username' is nil."
  (unless helm-hatena-bookmark-username
    (error "Variable `helm-hatena-bookmark-username' is nil"))
  (format "http://b.hatena.ne.jp/%s/search.data"
	  helm-hatena-bookmark-username))

(defun helm-hatena-bookmark-http-request ()
  "Make a new HTTP request for create `helm-hatena-bookmark-file'."
  (let ((buffer-name helm-hatena-bookmark-http-buffer-name)
	(proc-name "helm-hatena-bookmark")
	(curl-args `("--silent" "--compressed" ,helm-hatena-bookmark-url))
	proc)
    (unless (get-buffer-process buffer-name)
      (if (get-buffer buffer-name)
	  (kill-buffer buffer-name))
      (setq helm-hatena-bookmark-debug-start-time (current-time))
      (setq proc (apply 'start-process
			proc-name
			buffer-name
			helm-hatena-bookmark-curl-program
			curl-args))
      (set-process-sentinel proc 'helm-hatena-bookmark-http-request-sentinel))))

(defun helm-hatena-bookmark-http-request-sentinel (_process _event)
  "Receive a response of `helm-hatena-bookmark-http-request'.
Argument _PROCESS is a http-request process.
Argument _EVENT is a string describing the type of event."
  (let ((buffer-name helm-hatena-bookmark-http-buffer-name)
	result)
    (with-current-buffer (get-buffer buffer-name)
      (let ((sed-args '("-n" "N; N; s/\\(.*\\)\\n\\(\\[.*\\]\\)\\?\\(.*\\)\\n\\(http.*\\)/\\2 \\1 [summary:\\3][href:\\4]/p")))
	(apply 'call-process-region
	       (point-min) (point-max)
	       helm-hatena-bookmark-sed-program t '(t nil) nil
	       sed-args)
	(setq result (> (point-max) (point-min)))
	(when result
	  (if helm-hatena-bookmark-debug-mode
	      (message (format "[B!] write-region at %s, result:%s, point-min:%d, point-max:%d"
			       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) result (point-min) (point-max))))
	  (write-region (point-min) (point-max) helm-hatena-bookmark-file))))
    (if result (kill-buffer buffer-name))
    (if helm-hatena-bookmark-debug-mode
	(message (format "[B!] %s to create %s (%0.1fsec) at %s."
			 (if result "Success" "Failure")
			 helm-hatena-bookmark-file
			 (time-to-seconds
			  (time-subtract (current-time)
					 helm-hatena-bookmark-debug-start-time))
			 (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))))

(defun helm-hatena-bookmark-set-timer ()
  "Set timer."
  (setq helm-hatena-bookmark-timer
	(run-at-time "0 sec"
		     helm-hatena-bookmark-interval
		     #'helm-hatena-bookmark-http-request)))

(defun helm-hatena-bookmark-cancel-timer ()
  "Cancel timer."
  (when helm-hatena-bookmark-timer
    (cancel-timer helm-hatena-bookmark-timer)
    (setq helm-hatena-bookmark-timer nil)))

;;;###autoload
(defun helm-hatena-bookmark-initialize ()
  "Initialize `helm-hatena-bookmark'."
  (setq helm-hatena-bookmark-url
	(helm-hatena-bookmark-get-url))
  (setq helm-hatena-bookmark-curl-program
	(helm-hatena-bookmark-find-curl-program))
  (setq helm-hatena-bookmark-sed-program
	(helm-hatena-bookmark-find-sed-program))
  (helm-hatena-bookmark-set-timer))

(define-obsolete-function-alias 'helm-hatena-bookmark:initialize
  'helm-hatena-bookmark-initialize "2.2.0")

(provide 'helm-hatena-bookmark)

;;; helm-hatena-bookmark.el ends here
