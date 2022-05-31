;;; dotenv.el --- Plugin for loading .env project files. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.5.0
;; Keywords: tools
;; URL: https://www.github.com/pkulev/dotenv.el
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (f "0.20.0"))

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

;; Dotenv parser by following rules:
;; * skip empty lines
;; * trim every string (all spaces before first non-whitespace char and after
;;   last non-whitespace char will be removed)
;; * skip commentary lines (# at the line start)
;; * skip lines which doesn't look like an proper assignment
;; * variable name and its value will be trimmed

;; TODO: respect whitespaces between quotes (VAR="  <- keep those ->  ")

;;; Code:


(require 'cl-lib)
(require 'subr-x)

(require 'f)
(require 's)

;; Customizations:
(defgroup dotenv nil
  "Load .env project files."
  :prefix "dotenv-"
  :group 'tools)

(defcustom dotenv-file-name ".env"
  "Usually it's `.env'."
  :group 'dotenv
  :local t
  :safe t
  :type 'string)

(defcustom dotenv-transform-alist
  '(((lambda (k v) t) . (lambda (k v) (list k v))))
  "List of predicate-transform pairs for custom key or/and value processing."
  :group 'dotenv
  :type 'alist)

;; Implementation:

;; Key/Value transformation -->
;; TODO: do something with inner quotes and throw away quote replacement
(defun dotenv-absolutify-path-var-in-project (path &optional delim)
  "Transform pathes in PATH (delimeted by DELIM) to absolute using project root.

It relies on `projectile', but `dotenv.el' doesn't depend on it. Consider this
like contrib thing.

For example:
 > (dotenv-absolutify-path-var-in-project \"p1:p2:p3\")
\"/path/to/project/p1:/path/to/project/p2:/path-to-project/p3\""
  (let ((root (projectile-project-root))
        (quote-re "\"\\|'"))
    (s-join (or delim ":")
            (mapcar (lambda (it) (f-join root it))
                    (s-split (or delim ":")
                             (string-trim path quote-re quote-re))))))

(defun dotenv-transform-pair (pair)
  "Transform key/value PAIR using custom transformers."
  (cl-destructuring-bind (key value) pair
    (cl-dolist (pair dotenv-transform-alist)
      (cl-destructuring-bind (pred . transform) pair
        (when (funcall pred key value)
          (cl-return (funcall transform key value)))))))
;; <-- Key/Value transformation

;; File loading -->
(defun dotenv-path (dir)
  "Construct path for .env file for DIR.

Use this function for writes, as it doesn't check if .env file exists."
  (if (stringp dir)
      (f-join dir dotenv-file-name)
    (error "DIR must be of string type")))

(defun dotenv-locate (dir)
  "Locate .env file in DIR and return absolute path to it if found.

Use this function for reads, as it returns non-nil only if .env file exists."
  (let ((path (dotenv-path dir)))
    (when (f-exists? path)
      path)))

(defun dotenv-load (abs-path)
  "Parse .env file by (absolute) ABS-PATH."
  (seq-filter #'identity
              (mapcar #'dotenv-parse-line
                      (seq-filter #'s-present? (s-lines (f-read abs-path))))))

(defun dotenv-project-load (project-root)
  "Load .env by PROJECT-ROOT."
  (dotenv-load (dotenv-path project-root)))
;; <-- File loading

;; Parsing -->
(defun dotenv--assignment? (line)
  "Naive assignment checker for LINE.

LINE must be trimmed."
  (and (s-present? line)
       (not (s-starts-with? "#" line))
       (s-contains? "=" line)))

;; TODO: escape inner quotes
;; TODO: expand new lines
(defun dotenv-parse-line (line)
  "Parse LINE string into the list of two elements (VAR VALUE).

If LINE doesn't contain any kind of VAR=VALUE pair then nil will be return.
Empty values are allowed."
  (let ((line (s-trim line)))
    (when (dotenv--assignment? line)
      (cl-destructuring-bind (var value) (s-split-up-to "=" line 1)
        (when (s-present? var)
          (list (s-trim var) (s-trim value)))))))
;; <-- Parsing

;; --> Updating environment
(defun dotenv-update-env (env-pairs &optional override)
  "Update env with values from ENV-PAIRS.

If OVERRIDE is true then override variables if already exists."
  (dolist (pair env-pairs)
    (cl-destructuring-bind (key value) (dotenv-transform-pair pair)
      (when (or override (null (getenv key)))
        (setenv key value)))))

(defun dotenv-update-project-env (project-root)
  "Update env with .env values from PROJECT-ROOT."
  (dotenv-update-env (dotenv-load (dotenv-locate project-root))))
;; <-- Updating environment

(defun dotenv-get (key path)
  "Get value by KEY from env file PATH.

Returns nil if KEY is not exist in .env file."
  (cl-second (assoc key (dotenv-load path))))

(provide 'dotenv)

;;; dotenv.el ends here
