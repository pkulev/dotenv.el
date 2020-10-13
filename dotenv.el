;;; dotenv.el --- Plugin for loading .env project files.

;; Copyright (C) 2019 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.1.0
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


(require 'f)
(require 's)


(defun dotenv-path (project-root)
  "Construct path for .env file for PROJECT-ROOT."
  (when (s-present? project-root)
    (f-join project-root ".env")))

(defun dotenv-parse-file (path)
  "Parse .env file by absolute PATH."
  (mapcar #'(lambda (it) (s-split-up-to "=" it 1))
          (seq-filter #'s-present? (s-lines (f-read path)))))

(defun dotenv-load (path)
  "Load .env by PATH."
  (when (file-exists-p path)
    (dotenv-parse-file path)))

(defun dotenv-update-env (env-pairs)
  "Update env with values from ENV-PAIRS."
  (dolist (pair env-pairs)
    (destructuring-bind (key value) pair
      (setenv key value))))

(defun dotenv-get (key path)
  "Get value by KEY from env file PATH.

Returns nil if KEY is not exist in .env file."
  (second (assoc key (dotenv-load path))))

(provide 'dotenv)

;;; dotenv.el ends here
