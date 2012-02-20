;;; logger.el --- logging library for Emacs

;; Copyright (C) 2012  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, tool
;; Version: 0.1
;; Package-Requires: ((eieio "1.3"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module provides logging facility for Emacs

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defclass logger-object ()
  ((level :initarg :level :initform nil)))

(defmethod logger-insert-log ((log logger-object) format &rest objects)
  "Base implementation, do nothing")

(defmethod logger-should-log ((log logger-object) level)
  (let ((l (oref log :level)))
    (and (integerp l)
         (<= level l))))

(defmethod logger-log ((log logger-object) level tag string &rest objects)
  (when (logger-should-log log level)
    (apply 'logger-insert-log log (format "[%s] %s" tag string) objects)))

(defmethod logger-log (log level tag string &rest objects)
  "Fallback implementation, do nothing. This allows in particular
  to pass nil as the log object.")

(defclass logger-message-object (logger-object)
  ())

(defmethod logger-insert-log ((log logger-message-object) format &rest objects)
  (apply 'message format objects))

(defclass logger-buffer-object (logger-object)
  ((buffer :initarg :buffer :initform nil)))

(defmethod logger-should-log ((log logger-buffer-object) level)
  (and (oref log :buffer)
       (call-next-method)))

(defmethod logger-insert-log ((log logger-buffer-object) format &rest objects)
  (let ((buffer (get-buffer-create (oref log :buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format objects) "\n\n"))))

(defvar logger-def-in-package 'logger
  "Object holding the prefix to give to the generated
  accessors. This allows using custom log levels in different
  packages")

(defmacro logger-def-level (sym val)
  "Define a constant logger-<SYM>-level and a macro logger:<SYM>
associated with this level."
  (let ((const (intern (format "%s:%s-level"
                               logger-def-in-package (symbol-name sym))))
        (mac (intern (format "%s:%s"
                             logger-def-in-package (symbol-name sym)))))
    `(progn
       (defconst ,const ,val)
       (defmacro ,mac (log string &rest objects)
         (append
          (list 'logger-log log ,const '',sym string)
          objects)))))

;; built-in log levels
(let ((logger-def-in-package 'logger))
  (logger-def-level error 0)
  (logger-def-level info 5)
  (logger-def-level verbose 10)
  (logger-def-level debug 15))

(provide 'logger)
;;; logger.el ends here
