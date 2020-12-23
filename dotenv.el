;;; dotenv.el --- Plugin for loading .env project files.

;; Copyright (C) 2020 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.2.0
;; Keywords: dotenv
;; URL: https://www.github.com/pkulev/dotenv.el
;; Package-Requires: ((s "1.12.0") (f "0.20.0"))

;; This file is NOT part of GNU/Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Plugin for loading .env project files.

;;; Code:


(require 'cl-lib)

(require 'f)
(require 's)

;; Customizations:
(defgroup dotenv nil
  "Load .env project files."
  :prefix "dotenv-"
  :group 'tools)

(defcustom dotenv-transform-alist
  '(((lambda (k v) t) . (lambda (k v) (list k v))))
  "List of predicate-transform pairs for custom key or/and value processing."
  :group 'c2
  :type 'alist)

;; Implementation:
(defun dotenv-absolutify-path-var-in-project (path &optional delim)
  "Transform pathes in PATH (delimeted by DELIM) to absolute using project root.

It relies on `projectile', but `dotenv.el' doesn't depend on it. Consider this
like contrib thing.

For example:
 > (dotenv-absolutify-path-var-in-project \"p1:p2:p3\")
\"/path/to/project/p1:/path/to/project/p2:/path-to-project/p3\""
  (when (s-present? path)
    (let ((root (projectile-project-root)))
      (s-join (or delim ":")
              (mapcar (lambda (it) (f-join root it))
                      (s-split (or delim ":") path))))))

(defun dotenv-path (project-root)
  "Construct path for .env file for PROJECT-ROOT."
  (when (s-present? project-root)
    (f-join project-root ".env")))

(defun dotenv-parse-file (path)
  "Parse .env file by absolute PATH."
  (mapcar #'(lambda (it) (s-split-up-to "=" it 1))
          (seq-filter #'s-present? (s-lines (f-read path)))))

(defun dotenv-load% (path)
  "Load .env by absolute PATH."
  (when (file-exists-p path)
    (dotenv-parse-file path)))

(defun dotenv-load (project-root)
  "Load .env by PROJECT-ROOT."
  (dotenv-load% (dotenv-path project-root)))

(defun dotenv-transform-pair (pair)
  "Transform key/value PAIR using custom transformers."
  (cl-destructuring-bind (key value) pair
    (dolist (pair dotenv-transform-alist)
      (cl-destructuring-bind (pred . transform) pair
        (when (funcall pred key value)
          (return (funcall transform key value)))))))

(defun dotenv-update-env (env-pairs)
  "Update env with values from ENV-PAIRS."
  (dolist (pair env-pairs)
    (cl-destructuring-bind (key value) (dotenv-transform-pair pair)
      (setenv key value))))

(defun dotenv-update-project-env (project-root)
  "Update env with .env values from PROJECT-ROOT."
  (dotenv-update-env (dotenv-load project-root)))

(defun dotenv-get (key path)
  "Get value by KEY from env file PATH.

Returns nil if KEY is not exist in .env file."
  (second (assoc key (dotenv-load path))))

(provide 'dotenv)

;;; dotenv.el ends here
