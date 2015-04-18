;;; helm-hatena-bookmark.el --- Hatena::Bookmark with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-hatena-bookmark
;; Version: 1.2.0
;; Package-Requires: ((helm "1.6.9"))

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

(defvar helm-hatena-bookmark-file "~/.hatenabookmark")
(defvar helm-hatena-bookmark-candidate-number-limit 9999)
(defvar helm-hatena-bookmark-full-frame helm-full-frame)

(defun helm-hatena-bookmark--load ()
  "Load `helm-hatena-bookmark-file'."
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents helm-hatena-bookmark-file))))

(defvar helm-hatena-bookmark--action
  '(("Browse URL" . helm-hatena-bookmark--browse-url)
    ("Show URL" . helm-hatena-bookmark--show-url)
    ("Show Summary" . helm-hatena-bookmark--show-summary)))

(defun helm-hatena-bookmark--browse-url (candidate)
  "Action for Browse URL.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[href:\\(.+\\)\\]$" candidate)
  (browse-url (match-string 1 candidate)))

(defun helm-hatena-bookmark--show-url (candidate)
  "Action for Show URL.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[href:\\(.+\\)\\]$" candidate)
  (message (match-string 1 candidate)))

(defun helm-hatena-bookmark--show-summary (candidate)
  "Action for Show Summary.
Argument CANDIDATE a line string of a bookmark."
  (string-match "\\[summary:\\(.+\\)\\]\\[" candidate)
  (message (match-string 1 candidate)))

(defvar helm-hatena-bookmark--source
  `((name . "Hatena::Bookmark")
    (init . helm-hatena-bookmark--load)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-hatena-bookmark-candidate-number-limit)
    (multiline)
    (action . ,helm-hatena-bookmark--action))
  "Helm source for Hatena::Bookmark.")

;;;###autoload
(defun helm-hatena-bookmark ()
  "Search Hatena::Bookmark using `helm'."
  (interactive)
  (let ((helm-full-frame helm-hatena-bookmark-full-frame))
    (unless (file-exists-p helm-hatena-bookmark-file)
      (error (format "%s not found" helm-hatena-bookmark-file)))
    (helm :sources helm-hatena-bookmark--source
	  :prompt "Find Bookmark: ")))

(provide 'helm-hatena-bookmark)

;;; helm-hatena-bookmark.el ends here
