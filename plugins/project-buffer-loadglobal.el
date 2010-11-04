;;;_ loadglobal.el --- Load and customize global entry points

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

;; Setup if you use my-site-start.el: Just symlink this file into
;; ~/emacs.d/site-start.d with a suitable number prefix.  For example:

;; (Into dired mode)
;; C-x C-f RET
;; (Put point this file)
;; C-s project-buffer-loadglobal
;; (Symlink it)
;; S ~/.emacs.d/site-start.d/50project-buffer-loadglobal.el

;;;_ , Requires



;;;_. Body
;;;_ , Customizations

;;;_ , Keys setup
;;Or use fastload instead?
(global-set-key [menu-bar tools project-buffer] 
   '(menu-item "Open projects file" project-buffer-find-file
       (nil)
       :help "Visit a project-buffer-mode projects file"))


;;;_. Footers
;;;_ , Provides

(provide 'loadglobal)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; loadglobal.el ends here
