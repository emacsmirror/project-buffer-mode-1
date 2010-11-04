;;;_ project-buffer-loadglobal.el --- Load and customize global entry points

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

;; Setup:

;; If you use my-site-start.el, just 
;; M-x project-buffer-loadglobal-setup
;; All done!

;; If you use my-site-start.el but your site-start file is somewhere
;; other than ~/emacs.d/site-start.d/, symlink it manually.  For
;; example:

;; (Into dired mode)
;; C-x C-f RET
;; (Put point this file)
;; C-s project-buffer-loadglobal
;; (Symlink it)
;; S ~/.emacs.d/site-start.d/50project-buffer-loadglobal.el

;; If you don't use my-site-start.el, then include in your .emacs
;; (load PATH-TO-THIS-FILE)
;;


;;;_ , Requires



;;;_. Body
;;;_ , This file's identity
(defconst project-buffer-loadglobal-filename 
   (if load-file-name
      (file-truename load-file-name)
      (file-truename buffer-file-name))
   "The file's location" )
;;;_ , Customizations

;;;_ , Co-ordination
;;;_  . Find file
(defun project-buffer-loadglobal-find-file ()
   ""
   
   (interactive)
   (cond
      ((require 'project-buffer-fastload nil t)
	 (project-buffer-fastload-switch))
      ((require 'project-buffer-mode nil t)
	 (call-interactively #'project-buffer-find-file))
      (t
	 (message "Can't find any function to visit project files"))))

;;;_ , Keys setup
(global-set-key [menu-bar tools project-buffer] 
   '(menu-item "Open projects file" project-buffer-loadglobal-find-file
       (nil)
       :help "Visit a project-buffer-mode projects file"))

      

;;;_ , Set this up
(defun project-buffer-loadglobal-setup ()
   ""
   
   (interactive)
   (let
      ((target 
	  "~/.emacs.d/site-start.d/50project-buffer-loadglobal.el"))
      (cond
	 ((not (featurep 'my-site-start))
	    (message "my-site-start not found")
	    (message "Add (load THIS-FILE) to your .emacs" ))
	 ;;Check that path exists.
	 ((not
	     (file-exists-p
		(file-name-directory target)))
	    (message "I don't know where to symlink to"))
	 (t
	    (make-symbolic-link 
	       project-buffer-loadglobal-filename
	       target
	       nil)))))



;;;_. Footers
;;;_ , Provides

(provide 'project-buffer-loadglobal)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; project-buffer-loadglobal.el ends here
