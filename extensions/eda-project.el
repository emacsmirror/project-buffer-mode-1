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
;; projects, specifically with gEDA.  

;;;_ , Requires

(require 'project-buffer-mode)
(require 'iproject)

;;;_. Body
;;;_ , Customizations
;;But I haven't made them customizable yet since I myself only do it
;;one way. 
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
		  ;;New
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

   ;;Have not chosen good keys yet, nor rebound anything.
   (local-set-key [(control ?c) ?n] 'eda-project-edit-schematic))


;;;_ , Support
;;;_  . eda-project-act-on-file
(defmacro eda-project-act-on-file (file-sym &rest body)
   ""
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
		   ,@body))))))
;;;_  . eda-project-start-process
(defun eda-project-start-process (name args)
   ""
   
   (apply #'start-process-shell-command "gschem" nil args))

;;;_ , Gnetlist support functions
;;;_  . eda-project-gnetlist+args
(defun eda-project-gnetlist+args (filename extra-args)
   ""
   
   (list
      "gnetlist" 
      extra-args
      "-o" (concat 
	      (file-name-sans-extension filename)
	      ".drc2")
      filename))

;;;_  . eda-project-gnetlist-check-drc
(defun eda-project-gnetlist-check-drc (filename)
   ""
   (eda-project-gnetlist+args filename eda-project-cmd-design-check))

;;;_  . eda-project-gnetlist-build-netlist
(defun eda-project-gnetlist-build-netlist (filename)
   ""
   (eda-project-gnetlist+args filename eda-project-cmd-build-net-list))
;;;_ , Gschem support functions
;;;_  . eda-project-gschem-edit-schematic
(defun eda-project-gschem-edit-schematic (filename)
   "Shell command to edit FILENAME in gchsem"
   
   (list "gschem" filename))


;;;_ , Action functions

;;;_  . eda-project-edit-schematic

(defun eda-project-edit-schematic ()
   "Edit the current file as a schematic."
   (interactive)
   (eda-project-act-on-file filename
       (start-process-shell-command "gschem" nil 
	  "gschem" filename)))
;;;_  . eda-project-check-drc
(defun eda-project-check-drc ()
   "Make a netlist from the current schematic file."
   (interactive)
   (eda-project-act-on-file filename
      (when
	 (string= (file-name-extension filename) "sch")
	 (eda-project-start-process
	    (eda-project-gnetlist-check-drc filename)))))
;;And detect errors - not clear how.
;;Maybe search for "^JFound" and if found, there are errors.  Or does
;;its exit status tell us?  Or search for "DRC errors found. See output file."
;;in its output (its error output, I think)

;;;_  . eda-project-build-netlist
(defun eda-project-build-netlist ()
   "Make a netlist from the current schematic file."
   (interactive)
   (eda-project-act-on-file filename
      (when
	 (string= (file-name-extension filename) "sch")
	 (start-process-shell-command "build-netlist" nil 
	    "gnetlist" 
	    eda-project-cmd-build-net-list
	    "-o" (concat 
		    (file-name-sans-extension filename)
		    ".net")
	    filename))))

;;;_  . eda-project-analysis-op
;;"gnucap {file}"
;;And present it - how?

;;;_. Footers
;;;_ , Provides

(provide 'eda-project)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; eda-project.el ends here
