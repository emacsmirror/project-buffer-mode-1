;;;_ project-buffer-fastload.el --- Load a default projects file without interaction

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint

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

;; This plugin is to make project-buffer-mode know a project file
;; without requiring user interaction.

;; If you like it, I would suggest rebinding iproject's keybinding
;; C-x p f to #'project-buffer-fastload-switch

;;;_ , Requires

(require 'project-buffer-mode)

;;;_. Body

;;;_ , Customizations

(defcustom project-buffer-fastload-projects-file "~/.emacs.d/projects"
  "Default projects file."
  :type '(file :must-match t)
  :group 'project-buffer)

(defcustom project-buffer-fastload-ask nil
  "When set, always ask which projects file to load."
  :type 'boolean
  :group 'project-buffer)

;;;_ , Variables

(defvar project-buffer-fastload-buffer nil
   "The known project-buffer-mode buffer." )

;;;_ , project-buffer-fastload-switch
(defun project-buffer-fastload-switch ()
   "Switch to a project-buffer-mode buffer."
   
   (interactive)
   (switch-to-buffer (project-buffer-fastload-get-buffer)))

;;;_ , project-buffer-fastload-get-buf-names
(defun project-buffer-fastload-get-buf-names ()
   ""
   (delq nil
      (mapcar
	 #'(lambda (buf)
	      (with-current-buffer buf
		 (when
		    (eq
		       major-mode 'project-buffer-mode)
		    (buffer-name buf))))
	 (buffer-list))))

;;;_ , project-buffer-fastload-get-buffer
(defun project-buffer-fastload-get-buffer ()
   "Return a project-buffer-mode buffer.
Tries hard to do so without user interaction"
   (unless project-buffer-fastload-buffer
      (setq project-buffer-fastload-buffer
	 (let
	    (
	       (buf-name-list
		  (project-buffer-fastload-get-buf-names)))
	    (cond
	       ;;No project-buffer-mode buffer is open
	       ((not buf-name-list)
		  (let
		     ((filename
			 (if 
			    (or
			       project-buffer-fastload-ask
			       (null project-buffer-fastload-projects-file))
			    (read-file-name
			       "Which file is the projects savefile? "
			       nil project-buffer-fastload-projects-file t)
			    project-buffer-fastload-projects-file)))
		  (save-window-excursion
		     (project-buffer-find-file filename))))

	       ;;Just one project-buffer-mode buffer is open
	       ((not (cdr buf-name-list))
		  (car buf-name-list))

	       ;;Many project-buffer-mode buffers are open
	       (t
		  (completing-read
		     "Use which of the projects buffers? "
		     buf-name-list nil t))))))
   project-buffer-fastload-buffer)
;;;_ , project-buffer-fastload-set-buffer
;;;###autoload
(defun project-buffer-fastload-set-buffer (buf)
   "Set the buffer used for fastloading.
Usually not needed, but it's useful in some circumstances."
   
   (interactive
      (list 
	 (let*
	    ((buf-name-list
		(project-buffer-fastload-get-buf-names))
	       (name
		  (completing-read
		     "Use which projects buffer for fastload? "
		     buf-name-list nil t)))
	    (get-buffer name))))
   
   (setq project-buffer-fastload-buffer buf))


;;;_. Footers
;;;_ , Provides

(provide 'project-buffer-fastload)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; project-buffer-fastload.el ends here
