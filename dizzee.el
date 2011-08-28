;;; dizzee.el --- Utilities for managing subprocesses in Emacs

;; Copyright (C) 2011 David Miller <david@deadpansincerity.com>

;; Author: David Miller <david@deadpansincerity.com>
;; Version: 0.1
;; Created: 2011-06-23
;; Keywords: Emacs processes

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;
;; Commentary:
;;
;; This package provides a way of managing projects that require running
;; sub-processes/servers etc from within Emacs
;;

;;; Code:

;;
;; Module level requires
;;
(require 'cl)
(require 'assoc)

;;
;; Utilities
;;
;; Commentary:
;;
;; Generic lispish utilities that serve either as abstraction layers or
;; syntax sugar for common idioms.
;;

;;;###autoload
(defun dz-xp (expr)
  "Wrap the results of `expr', evaluating to t or nil when creating predicate-p functions"
  (if expr t nil))

;;;###autoload
(defun dz-symb-concat (symb suffix)
  "Return the symbol created by concatenating `symb' with `suffix'"
  (intern (concat (symbol-name symb) (symbol-name suffix))))

;;;###autoload
(defun dz-split (lst)
    "Split list into a list of lists"
    (if (eql 1 (length lst))
        (list lst)
      (let ((container (list)))
        (add-to-list 'container (list (first lst)))
        (append container (dz-split (rest lst))))))

;;;###autoload
(defun dz-akeys (alist)
  "Return a list of the keys in `alist'"
  (mapcar #'car alist))

;;;###autoload
(defun dz-regexp-filter (list regexp)
      "Filter LIST of strings with `regexp'."
      (let (new)
        (dolist (string list)
          (when (string-match regexp string)
           (setq new (cons string new))))
        (nreverse new)))

;;;###autoload
(defun dz-alist-filter (alist regexp)
  "Return values from `alist' whose KEY matches `regexp'"
  (mapcan #'(lambda (k) (aget alist k)) (dz-regexp-filter (dz-akeys alist) regexp)))

;;;###autoload
(defmacro dz-dir-excursion (dir body)
  "Perform BODY having moved to DIR before returning to the current directory"
  (let ((curdir default-directory))
    `(progn (cd ,dir)
            ,body
            (cd ,curdir))))

;;
;; Emacs utilities
;;
;; Commentary:
;;
;; These utilities outline various re-usable wrappers and convenience
;; abstractions for common patterns when dealing with the Emacs API
;;

;;;###autoload
(defun dz-pop (buffer)
  "Wraps pop-to and get buffer for `buffer'"
  (pop-to-buffer (get-buffer buffer)))

;;
;; Sub-Processes
;;
;; Commentary:
;;
;; Re-usable program components for interacting with sub-processes
;; such as servers etc
;;

(defvar dz-service-hash (make-hash-table)
  "A hash table of all services and their ports")

;;;###autoload
(defun dz-comint-pop (name command &optional args dont-pop)
  "Make a comint buffer for process `name', executing `command' with
`args' and then pop to that buffer."
  (ansi-color-for-comint-mode-on)
  (apply 'make-comint name command args)
  (if (not dont-pop)
      (dz-pop (concat "*" name "*"))))

;;;###autoload
(defun dz-subp-stop (name)
  "Check to see if the process `name' is running stop it if so."
  (let ((proc (get-buffer-process (concat "*" name "*"))))
    (if proc (kill-process proc))))

;;
;; Services
;;
;; Commentary:
;;
;; Functions related to defining and manipulating services
;;

(defmacro* dz-defservice (name command &key port args cd)
  "Expand to be an interactive dz service e.g. sse/backend/whitelabel
Args are expected to be: `name` `command` `args` `dont-pop`
where name and command are strings, args a list, and dont-pop optional.

This macro will provide the following functions:

name-start
name-stop
name-restart
name-running-p

\(dz-defservice backend \"~/scripts/backend_server\") :port 8080)
"
  (let* ((namestr (symbol-name name))
         (start (concat namestr "-start"))
         (stop (concat namestr "-stop")))
    (if port
        (puthash namestr port dz-service-hash))
    `(progn
       (defun ,(intern start) ()
         "Start the service"
         (interactive)
         (message "starting...")
         ,(let ((run `(dz-comint-pop ,namestr ,command ,args)))
            (if cd
                `(dz-dir-excursion ,cd
                                   ,run)
              run))
       (defun ,(intern stop) ()
         "Stop the service"
         (interactive)
         (message "stopping")
         (dz-subp-stop ,namestr))
       (defun ,(intern (concat namestr "-restart")) ()
         "Restart the service..."
         (interactive)
         (message "Restarting...")
         (,(intern stop))
         (run-with-timer 1 nil ',(intern start) ))
       (defun ,(intern (concat namestr "-running-p")) ()
         "Determine whether we're running or not"
         (dz-xp (get-buffer-process ,(concat "*" namestr "*"))))))))


(defmacro dz-defservice-group (name services)
  "Create a group of services that function as a project together.
This allows us to start groups of complimentary services together.

Example:

\(dz-defservice-group warehouse (ornithology-thrift ornithology-frontend))
"
  (let ((namestr (symbol-name name)))
    `(progn
       (defun ,(intern (concat namestr "-start")) ()
         ,(concat "Start the service group " namestr)
         (interactive)
         (message ,(concat "Starting " namestr "..."))
         ,@(loop for call in services
                 collect `(,(intern (concat (symbol-name call) "-start")))))

       (defun ,(intern (concat namestr "-stop")) ()
         ,(concat "Stop the service group " namestr)
         (interactive)
         (message ,(concat "Stopping " namestr))
         ,@(loop for call in services
                 collect `(,(intern (concat (symbol-name call) "-stop")))))

       (defun ,(intern (concat namestr "-restart")) ()
         ,(concat "Restart the service group " namestr)
         (interactive)
         (message ,(concat "Restarting " namestr))
         ,@(loop for call in services
                 collect `(,(intern (concat (symbol-name call) "-restart"))))))))

;;
;; Reloads
;;
;; Commentary:
;;
;; Sometimes we want to restart services when we save files in
;; a particular path. Let's do that.
;;

(defvar dz-reload-services (make-hash-table :test 'equal)
  "Alist of services that we want to reload")

(defun dz-reload ()
  "Executed as a file-save-hook, this function restarts any services that
have been regisered as reloading."
  (let ((saving (expand-file-name (buffer-file-name))))
    (maphash (lambda (service path-re)
               (if (string-match-p path-re saving)
                   (progn
                     (message (concat "Restarting " service))
                     (funcall (intern (concat service "-restart"))))))
             dz-reload-services)))

(defmacro dz-register-reload (service path)
  "Register SERVICE as a project you would like to reload when saving any
files under PATH"
  `(puthash ,(symbol-name service) ,path dz-reload-services ))


(dz-register-reload data-thrift2 "~/src2/onzo/hah")

(add-hook 'after-save-hook (lambda () (dz-reload)))

(provide 'dizzee)
;; Code ends
