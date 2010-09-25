;;;_ eda-project.el --- EDA extension for project-buffer-mode

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: convenience, tools

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

;; An extension for project-buffer-mode to support electronics
;; projects.  For now it only supports gnucap / gEDA since I myself
;; use it only that way.

;; It wants to use eda-project/Makefile for some dependency-related
;; things, such as design checking.  So copy or symlink that into your
;; projects - there's no support yet to do that automatically.

;;;_ , Requires

(require 'project-buffer-mode)
(require 'iproject)

;;;_. Body
;;;_ , Customizations
;;I haven't made them customizable yet 
;;This will change, possibly to choice of makefile(s) selecting
;;gnucap / gEDA / etc.

;;;_  . eda-project-cmd-build-net-list
(defconst eda-project-cmd-build-net-list 
   "-s -g spice-sdb"
   "" )
;;;_  . eda-project-cmd-design-check
(defconst eda-project-cmd-design-check 
   "-g drc2"
   "" )

;;;_ , Dealing with iproject
;;Not used yet
;;;_  . eda-project-filters
(defvar eda-project-filters
   '((geda     ("\\.sch$" "\\.cir$"))) 
   "Alist from (electronic) project type to file filters" )

;;;_ , Insinuation
;;;_  . eda-project-new
(defun eda-project-new (name root-folder)
   "Create a eda-project buffer named NAME with a `default-directory' set to ROOT-FOLDER."
   (interactive "sProject Buffer Name: \nDRoot Folder: ")
   (let ((buffer (generate-new-buffer (concat "eda:" name)))
	   (local-symbols
	      '(iproject-last-project-type-choosen
		  iproject-last-filter-type-choosen
		  iproject-last-file-filter-query-mode-choosen
		  iproject-last-file-filter-regexp-choosen
		  iproject-last-file-extension-list-choosen
		  ;;No
		  iproject-platform-list
		  ;;No
		  iproject-build-configuration-list
		  ;;Newly bound
		  iproject-filters)))
     
      (switch-to-buffer buffer)
      (with-current-buffer buffer
	 (cd root-folder)
	 (project-buffer-mode)
	 (dolist (sym local-symbols)
	    (make-local-variable sym)
	    (add-to-list 'project-buffer-locals-to-save sym))
	 ;;Platform list and build configurations would not be
	 ;;relevant.
	 ;;Everything to here could be encapped with a few
	 ;;parameters.  Then call any special setup code.

	 (eda-project-setup-local-key)
	 ;;Rebind iproject-filters to eda-project-filters.  Possibly
	 ;;as a defalias, if that works.
	 (add-hook 'project-buffer-post-load-hook
	    'eda-project-setup-local-key nil t)
	 ;;Maybe simply borrow this from iproject
	 ' (add-hook 'project-buffer-refresh-hook   
	      'iproject-refresh-handler nil t))))

;;;_ , Entry points
;;;_  . eda-project-setup-local-key
;;Set up our keymap
(defun eda-project-setup-local-key ()
   ""
   (local-set-key [(control ?c) ?e] 'eda-project-edit-schematic)
   (local-set-key [(control ?c) ?a] 'eda-project-autocheck)
   (local-set-key [(control ?c) ?b] 'eda-project-build-netlist)
   (local-set-key [(control ?c) ?v] 'eda-project-verbose-netlist))


;;;_ , Utility
;;;_  . eda-project-act-on-file
(defmacro eda-project-act-on-file (file-sym filter &rest body)
   "If current node is a file and its extension matches FILTER,
evaluate BODY with symbol FILE-SYM bound to filename."
   (let
      ((node (make-symbol "node"))
	 (node-data (make-symbol "node-data")))
      `(progn
	  (unless project-buffer-status (error "Not in project-buffer buffer"))
	  (let* ((,node (ewoc-locate project-buffer-status))
		   (,node-data (ewoc-data ,node)))
	     (when (eq (project-buffer-node->type ,node-data) 'file)
		(let
		   ((,file-sym (project-buffer-node->filename ,node-data)))
		   ,@(if filter
			`((when
			     (string= (file-name-extension filename) ,filter)
			     ,@body))
			body)))))))
;;;_  . eda-project-get-project-path
;;Adapted from project-buffer-perform-action-hook
(defun eda-project-get-project-path ()
   "Get the master project's path to its main file (makefile)"
   (project-buffer-node->filename 
      (ewoc-data (cdr project-buffer-master-project))))

;;;_  . eda-project-shell-call
(defun eda-project-shell-call (name command)
   "Start a process."
   (apply #'start-process-shell-command name nil command))

;;;_  . eda-project-make-target
(defun eda-project-make-target (filename)
   "Shell command to make target FILENAME."
   
   (list "make" 
      "-C" 
      (file-name-directory
	 (eda-project-get-project-path))
      filename))
;;;_  . eda-project-make-target-w/ext
(defun eda-project-make-target-w/ext (filename output-ext)
   "Shell command to make target FILENAME.
If OUTPUT-EXT is given, replace the file extension with it."
   
   (eda-project-make-target
      (concat 
	 (file-name-sans-extension filename)
	 "."
	 output-ext)))

;;;_  . eda-project-sentinel-view-file
(defun eda-project-sentinel-view-file (process event filename on-error)
   "Sentinel that views a certain file when the process is done"
   ;;$$IMPROVE ME: Don't act on "sleep" signals.
   (if (or on-error (string= event "finished\n"))
      (if (file-exists-p filename)
	 (view-file filename)
	 (message "File %s was not created" filename))
      (message "Error creating file %s" filename)))


;;;_  . eda-project-make-and-view
(defun eda-project-make-and-view 
   (filename &optional view-file-name on-error)
   ""
   
   (let
      (  (view-file-name (or view-file-name filename))
	 (process
	    (apply #'start-process "make-and-view" nil
	       (eda-project-make-target filename))))
      ;;Close over view-file-name
      (set-process-sentinel process
	 `(lambda (process event)
	     (eda-project-sentinel-view-file
		process event ,view-file-name ,on-error)))))

;;;_ , Gnetlist support functions

;;(Gone, moved to makefile)

;;;_ , Gschem support functions

;;;_  . eda-project-gschem-edit-schematic
(defun eda-project-gschem-edit-schematic (filename)
   "Shell command to edit FILENAME in gschem"
   (list "gschem" filename))

;;;_ , Gnucap support functions
;;

;;;_ , Commands

;;;_  . eda-project-edit-schematic

(defun eda-project-edit-schematic ()
   "Edit the file as a schematic."
   (interactive)
   (eda-project-act-on-file filename "sch"
      (eda-project-shell-call
	 "edit-schematic"
	 (eda-project-gschem-edit-schematic filename))))

;;;_  . eda-project-autocheck
;;This is specific to gnetlist.
(defun eda-project-autocheck ()
   "View the drc2 file (autocheck) of the schematic file at point.
Make it if neccessary."
   (interactive)
   (eda-project-act-on-file filename "sch"
      (let
	 ((file-name-sans-ext
	     (file-name-sans-extension filename)))
	 (eda-project-make-and-view 
	    (concat 
	       file-name-sans-ext "." "drc2-succeeded")
	    (concat 
	       file-name-sans-ext "." "drc2")
	    t))))

;;;_  . eda-project-build-netlist
(defun eda-project-build-netlist ()
   "View the netlist of the schematic file at point.
Make it if neccessary."
   (interactive)
   (eda-project-act-on-file filename "sch"
      (let
	 ((file-name-sans-ext
	     (file-name-sans-extension filename)))
	 (eda-project-make-and-view 
	    (concat 
	       file-name-sans-ext "." "net")))))

;;;_  . eda-project-analysis-op
;;"gnucap -b Scheme-file"
;;This may use a comint buffer.

;;;_. Footers
;;;_ , Provides

(provide 'eda-project)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; eda-project.el ends here
