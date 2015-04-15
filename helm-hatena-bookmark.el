;;; helm-hatena-bookmark.el --- Hatena::Bookmark with helm interface

;; Copyright (C) 2015 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-hatena-bookmark
;; Version: 1.0.3
;; Package-Requires: ((helm "1.5.6"))

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

(eval-when-compile (require 'cl))
(require 'helm)
(require 'url)
(require 'xml)

(defvar helm-hatena-bookmark-file "~/.hatenabookmark")
(defvar helm-hatena-bookmark-candidate-number-limit 9999)
(defvar helm-hatena-bookmark-requires-pattern 3)
(defvar helm-hatena-bookmark-full-frame helm-full-frame)

;;;###autoload
(defun helm-hatena-bookmark-get-dump ()
  "Get Hatena::Bookmark dump file."
  (interactive)
  (let
      ((created (format-time-string "%Y-%m-%dT%TZ" (current-time)))
       (nonce (sha1 (format-time-string "%Y-%m-%dT%T%z" (current-time))))
       (url "http://b.hatena.ne.jp/dump")
       (url-request-extra-headers nil)
       (x-wsse "")
       (x-wsse-list nil)
       (entry-list nil)
       (id (read-string "Hatena ID: "))
       (password (read-passwd "Password: ")))
    (setq x-wsse (concat "UsernameToken Username=\"" id "\", PasswordDigest=\"" (base64-encode-string (sha1 (concat nonce created password) nil nil 'binary)) "\", Nonce=\"" (base64-encode-string nonce) "\", Created=\"" created "\""))
    (setq x-wsse-list (cons "X-WSSE" x-wsse))
    (setq url-request-extra-headers (list x-wsse-list))
    (switch-to-buffer (url-retrieve-synchronously url))
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (delete-region (point-min) (1+ (point)))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "> +<" nil t)
      (replace-match "><"))
    (setq entry-list (xml-get-children (car (xml-parse-region (point-min) (point-max))) 'entry))
    (delete-region (point-min) (point-max))
    (loop for elm in entry-list
          do (insert
              (concat
               (apply 'concat (loop for elmelm in (xml-get-children elm 'dc:subject) collect (concat "[" (nth 2 elmelm) "]")))
               " "
               (let ((title (nth 2 (car (xml-get-children elm 'title)))))
                 (while (string-match "[\n\t]" title)
                     (setq title (replace-match "" nil nil title)))
                 title)
               (concat " [summary:" (nth 2 (car (xml-get-children elm 'summary))))
               (concat "][href:" (xml-get-attribute (car (xml-get-children elm 'link)) 'href))
               "]\n")))
    (write-file helm-hatena-bookmark-file)
    (kill-buffer (current-buffer))))

(defvar helm-hatena-bookmark--source
  `((name . "Hatena::Bookmark")
    (init
     . (lambda ()
           (with-current-buffer (helm-candidate-buffer 'global)
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents helm-hatena-bookmark-file)))))
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-hatena-bookmark-candidate-number-limit)
    (requires-pattern . ,helm-hatena-bookmark-requires-pattern)
    (multiline)
    (action
     ("Browse URL" . (lambda (candidate)
                       (string-match "\\[href:\\(.+\\)\\]$" candidate)
                       (browse-url (match-string 1 candidate))))
     ("Show URL" . (lambda (candidate)
                     (string-match "\\[href:\\(.+\\)\\]$" candidate)
                     (message (match-string 1 candidate))))
     ("Show Summary" . (lambda (candidate)
                         (string-match "\\[summary:\\(.+\\)\\]\\[" candidate)
                         (message (match-string 1 candidate))))))
  "Helm source for Hatena::Bookmark")

;;;###autoload
(defun helm-hatena-bookmark ()
  "Search Hatena::Bookmark using `helm'."
  (interactive)
  (let ((helm-full-frame helm-hatena-bookmark-full-frame))
    (unless (file-exists-p helm-hatena-bookmark-file)
      (helm-hatena-bookmark-get-dump))
    (helm :sources helm-hatena-bookmark--source
	  :prompt "Find Bookmark: ")))

(provide 'helm-hatena-bookmark)

;;; helm-hatena-bookmark.el ends here
