;;; fsproject.el --- File System Project Viewer
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.0
;; Keywords:    project buffer makefile filesystem management
;; Description: File System Project Viewer Extension
;; Tested with: GNU Emacs 22.x and GNU Emacs 23.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;; 

;; This is an extension to fsproject, an add-on library for
;; project-buffer-mode.
;;
;; 
;; - Added key: C-c +   to add new project
;;              C-c -   to delete a project
;;              C-c R   to rename the current project 
;;              C-c C-r to revert the project
;;              C-x C-w to write the project
;;              C-x C-s to save the project

(defcustom fsprojectp-filters
  '((c++       ("\\.[cChH][pPxX+][pPxX+]$" "\\.[cChH]$" "\\.[iI][nN][lL]$" "\\.[cC][cC]$"))
    (c         ("\\.[cChH]$" "\\.[iI][nN][lL]$" "\\.[cC][cC]$"))
    (elisp     ("\\.el$"))
    (perl      ("\\.pl$"))
    (ruby      ("\\.rb$"))
    (sharp     ("\\.[cjf]s$"))
    (python    ("\\.py$"))
    (smalltalk ("\\.st$"))
    (haskell   ("\\.hs$"))
    (ocaml     ("\\.ml$"))
    (lisp      ("\\.cl$"))
    (java      ("\\.java$" "\\.js$"))
    (cg        ("\\.cg\\(?:fx\\)?$"))
    (web       ("\\.htm\\(?:l\\)?$" "\\.xml" "\\.php$" "\\.js$" "\\.css$"))
))

(defcustom fsprojectp-project-type
  '((makefile ("\\.mak$" "Makefile$"))
    (cmake    ("CMakeLists.txt"))
    (jam      ("Jamfile\\(?:s\\)?$" "Jambase$" "Jamroot$"))
    (scons    ("SConstruct$" "Sconscript$"))
    (dmconfig ("build.dmc"))
))

(defcustom fsprojectp-ignore-folder
  '(".git" ".svn" "bzr" ".hg" "CVS" ".CVS" "build" "Debug" "Release"))

;;
;;  Functions:
;;


(defun fsprojectp-read-filter()
  "Read the file filter."
  (interactive)
  (
  )


(defun fsprojectp-add-project(folder main-file filter)
  "Select a FOLDER, a MAIN-FILE and a FILE-FILTER, then add all
files under the current folder and sub-folder matching the
FILE-FILTER will be added to the project."
  (interactive (list (read-directory-name "Project Root Folder: " nil nil t)
		     (read-file-name "Project Main File: " nil nil t)
		     (fsprojectp-read-filter)))
  )
	       


(defun fsprojectp-setup-local-key()
  "Define a local key-bindings."
  ((local-set-key [(control ?+)] 'fsprojectp-add-project)))


;;
;;  User commands:
;;

(defun fsproject-new(name root-folder)
  "Entry function of thie project-mode."
  (interactive "sProject Buffer Name: \nDRoot Folder: ")
  (let ((buffer (generate-new-buffer (concat "fsx:" name))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (project-buffer-mode)
      (fsprojectp-setup-local-key)
      )))
