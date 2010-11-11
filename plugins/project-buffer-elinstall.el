;;;_ project-buffer-elinstall.el --- Invoke elinstall for project-buffer-mode

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

;; This is a script to install project-buffer-mode using elinstall


;;;_ , Requires

(require 'elinstall)

;;;_. Body

(elinstall
   "project-buffer-mode"
   (elinstall-directory-true-name)
   '(def-file "project-buffer-loaddefs.el"
       (all
	  (dir "./")
	  (dir "plugins/")
	  (dir "extensions/"))))

;;;_. Footers
;;;_ , Provides

;;Nothing, it's a script.

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; project-buffer-elinstall.el ends here
