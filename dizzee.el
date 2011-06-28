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

;;
;; Utilities
;;
;; Commentary:
;;
;; Generic lispish utilities that serve either as abstraction layers or
;; syntax sugar for common idioms.
;;

(defun dz-xp (expr)
  "Wrap the results of `expr', evaluating to t or nil when creating predicate-p functions"
  (if expr t nil))

(defun dz-symb-concat (symb suffix)
  "Return the symbol created by concatenating `symb' with `suffix'"
  (intern (concat (symbol-name symb) (symbol-name suffix))))

(defun dz-split (lst)
    "Split list into a list of lists"
    (if (eql 1 (length lst))
        (list lst)
      (let ((container (list)))
        (add-to-list 'container (list (first lst)))
        (append container (dz-split (rest lst))))))

(defun dz-akeys (alist)
  "Return a list of the keys in `alist'"
  (mapcar #'car alist))

(defun dz-regexp-filter (list regexp)
      "Filter LIST of strings with `regexp'."
      (let (new)
        (dolist (string list)
          (when (string-match regexp string)
           (setq new (cons string new))))
        (nreverse new)))

(defun dz-alist-filter (alist regexp)
  "Return values from `alist' whose KEY matches `regexp'"
  (mapcan #'(lambda (k) (aget alist k)) (dz-regexp-filter (dz-akeys alist) regexp)))

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

(defun dz-map-call (services call &optional splice)
    "Map calls to `call' onto each of the SEQUENCE `services'
If optional `splice' is non-nil, we return a list of single-item lists,
each one containing a service call, suitable for splicing into a macro."
    (let ((calls (list)))
      (dolist (serv services)
        (add-to-list 'calls (dz-symb-concat serv call)))
      (if (not splice)
          calls
        )))

(defmacro dz-defservice (name command args &optional port)
  "Expand to be an interactive dz service e.g. sse/backend/whitelabel
Args are expected to be: `name` `command` `args` `dont-pop`
where name and command are strings, args a list, and dont-pop optional.
"
  (let* ((namestr (symbol-name namwe))
         (start (concat namestr "-start"))
         (stop (concat namestr "-stop")))
    (if port
        (puthash namestr port dz-service-hash))
    `(progn
       (defun ,(intern start) ()
         "Start the service"
         (interactive)
         (message "starting...")
         (dz-comint-pop ,namestr ,command ,args))

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
         (dz-xp (get-buffer-process ,(concat "*" namestr "*")))))))

;;
(dz-defservice backend (concat (dz-backend-scripts) "/backend_server") nil 8080)


(defmacro dz-defservice-group (name services)
  "Create a group of services that function as a project together"
  (let ((namestr (symbol-name name)))
    `(progn
       (defun ,(intern (concat namestr "-start")) ()
         ,(concat "Start the service group " namestr)
         (interactive)
         (message ,(concat "Starting " namestr "..."))
         ; TODO Make service calls here
         ,(dz-map-call services '-start t)
         )
         )))

(dz-defservice-group warehouse '(onzo-data-thrift onzo-data-frontend))

;;
;; Reloads
;;
;; Commentary:
;;
;; Sometimes wr want to restart services when we save files in
;; a particular path. Let's do that.
;;

(defvar dz-reload-services '()
  "Alist of services that we want to reload")

(defun dz-reload ()
  "Executed as a file-save-hook, this function restarts any services that
have been regisered as reloading.")

(defun dz-register-reload (service path)
  "Register `service' as a project you would like to reload when saving any
files under `path'"
  (aput 'dz-reload-services path service))

; (dz-register-reload 'data-thrift "~/thrifty")
; (dz-register-reload 'data-thrift2 "~/src2/onzo/hah")#

(add-hook 'after-save-hook (lambda () (dz-reload)))

(provide 'dizzee)
;; Code ends