;;;_ project-buffer-keybind-in-buffers.el --- Bind a key sequence prefix in a project's client buffers

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: tools, convenience

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

;;;_ , Commentary:

;; Entry points:

;; M-x project-buffer-kib-setup-project 
;;   Creates the keybinding prefix "\C-cp" in each buffer that is
;;   currently visiting a file in the given project.
;; M-x project-buffer-kib-setup-all-projects
;;   Does the same for all projects

;;;_ , Requires

(require 'project-buffer-mode)
;;;_. Configuration
(defconst project-buffer-kib-key-prefix "\C-cp" 
   "Key sequence prefix that is to be bound in client buffers" )
;;;_. Body
;;;_ , project-buffer-kib-dispatch-command
(defun project-buffer-kib-dispatch-command (buf project)
   (with-current-buffer buf
      ;;Check that project is current
      (unless (string= project (car project-buffer-master-project))
	 (error "Project is not the current master project"))
      ;;$$IMPROVE ME - could prompt to change master project to
      ;;PROJECT and proceed.

      (let
	 ((key-sequence
	     (read-key-sequence "Project-buffer command: " t)))
	 (call-interactively
	    (local-key-binding key-sequence t)))))

;;;_ , project-buffer-kib-build-command
(defun project-buffer-kib-build-command (buf project)
   "Return a lambda form that will dispatch a command in BUF with
master project PROJECT."
   `(lambda ()
       (interactive)
       (project-buffer-kib-dispatch-command ,buf ,project)))

;;;_ , project-buffer-kib-setup-project
;;;###autoload
(defun project-buffer-kib-setup-project (project-name)
   "In all buffers in a project, set up a keybinding prefix that
executes a command as if in this project-buffer."

   (interactive 
      ;;$$IMPROVE ME - could check for being in project-mode buffer
      ;;here too.  Strategy - Encap this interaction and do the check
      ;;there.
      (list 
	 (completing-read 
	    "KIB which project: " 
	    project-buffer-projects-list nil t 
	    nil nil (car project-buffer-master-project))))
   (unless project-buffer-status (error "Not in project-buffer buffer"))

   (let*
      ((lam (project-buffer-kib-build-command 
	       (current-buffer)
	       project-name)))
      ;;$$IMPROVE ME - Could set this up to also be done later when a
      ;;file is loaded.
      (project-buffer-apply-to-project-files
	 project-name
	 #'(lambda (project-file-name file-path project-name lam)
	      ;;If there is already a buffer visiting that file...
	      (let
		 ((buf (find-buffer-visiting file-path)))
		 (when buf
		    ;;...set up the key prefix in that buffer.
		    (with-current-buffer buf
		       (local-set-key 
			  project-buffer-kib-key-prefix 
			  lam)))))
	 lam)))

;;;_ , project-buffer-kib-setup-all-projects
;;;###autoload
(defun project-buffer-kib-setup-all-projects ()
   ""
   
   (interactive)
   (unless project-buffer-status (error "Not in project-buffer buffer"))

   (dolist
      (project-name project-buffer-projects-list)
      (project-buffer-kib-setup-project project-name)))

;;;_. Footers
;;;_ , Provides

(provide 'project-buffer-keybind-in-buffers)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; project-buffer-keybind-in-buffers.el ends here
