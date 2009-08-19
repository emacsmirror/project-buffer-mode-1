;;; fsproject+.el --- File System Project Viewer
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
    (custom    (nil)))
  "List of the different file filters."
)

(defcustom fsprojectp-project-type
  '((makefile ("\\.mak$" "Makefile$")     
	      ("make CONFIG={build}" "make clean CONFIG={build}" "" ""))
    (cmake    ("CMakeLists.txt")                         
	      ("make CONFIG={build}" "make clean CONFIG={build}" "" ""))
    (jam      ("Jamfile\\(?:s\\)?$" "Jamrules$" "Jambase$" "Jamroot$")
	      ("jam -a {project}" "jam clean -a {project}" "" ""))
    (scons    ("SConstruct$" "Sconscript$")
	      ("scons" "scons --clean" "" ""))
    (dmconfig ("build.dmc$")
	      ("make {platform}.{project}-{config}.build"
	       "make {platform}.{project}-{config}.clean"
	       "make {platform}.{project}-{config}.run"
	       "make {platform}.{project}-{config}.debug"))
    (cabal    ("\\.cabal$")
	      ("cabal build" "cabal clean" "" ""))
    (any      (".*$")
	      ("" "" "" ""))
    (blank    nil
	      ("" "" "" "")))
  "List of the different project type.

Each project type is a list of the following format:
  (symbol matching-regexp (action-string-list)) where
  action-string-list is a set of 4 strings representing the default
  command to 'build' 'clean' 'run' and 'debug'.
  the following wild cards can be use in each action string:
   {config}   the current selected build version
   {platform} the current selected platform 
   {project}  name of the project
   {path}     path of the project"
)

(defcustom fsprojectp-ignore-folder
  '(".git" ".svn" "bzr" ".hg" "CVS" ".CVS" "build" "lib" "Debug" "Release")
  "List of folder to ignore during the recursive search.")

;;
;;  Local variables:
;;

(defvar fsprojectp-last-project-type-choosen "makefile")
(defvar fsprojectp-last-filter-type-choosen "c++")
(defvar fprojectp-last-file-filter-query-mode-choosen "regexp")
(defvar fprojectp-last-file-filter-regexp-choosen nil)
(defvar fprojectp-last-file-extension-list-choosen nil)


;;
;;  Functions:
;;

(defun fsprojectp-choose-project-type()
  "Request and return the selected project type"
  (let* ((project-type-string (completing-read (format "Project Type [default %s]: " fsprojectp-last-project-type-choosen)
					       fsprojectp-project-type nil t nil nil fsprojectp-last-project-type-choosen))
	 (project-type (intern project-type-string)))
    (setq fsprojectp-last-project-type-choosen project-type-string)
    (assoc project-type fsprojectp-project-type)))

(defun fprojectp-shorten-string(str max-lgt)
  "If the length of STR is greater than MAX-LGT; shorten the string adding '...' at the end."
  (if (> (length str) max-lgt)
      (concat (substring str 0 (- max-lgt 3)) "...")
      str))

(defun fsprojectp-choose-file-filter()
  "Read the file filter."
  (let* ((filter-type-string (completing-read (format "Filter Type [default %s]: " fsprojectp-last-filter-type-choosen)
					       fsprojectp-filters nil t nil nil fsprojectp-last-filter-type-choosen))
	 (filter-type (intern filter-type-string)))
    (setq fsprojectp-last-filter-type-choosen filter-type-string)
    (if (not (eq filter-type 'custom))
	;; If not custom: return the selected file-filter:
	(assoc filter-type fsprojectp-filters)
	;; In case of custom file filter:
	;; Let's first ask how to specify the filter:
	(let* ((query-mode-string (completing-read (format "Enter the file system query mode (regexp, file-extension) [default %s]: " fprojectp-last-file-filter-query-mode-choosen)
						   '("regexp" "file-extension") nil t nil nil fprojectp-last-file-filter-query-mode-choosen))
	       (query-mode (intern query-mode-string)))
	  (setq fprojectp-last-file-filter-query-mode-choosen query-mode-string)
	  (cond ((eq query-mode 'regexp)
		 ;; A regexp: 
		 (let* ((def-string (if fprojectp-last-file-filter-regexp-choosen
					(concat " [default " (fprojectp-shorten-string fprojectp-last-file-filter-regexp-choosen 9) "]")
					""))
			(file-filter-regexp (read-from-minibuffer (format "Enter the file filter regexp%s: " def-string))))
		   (if (= (length file-filter-regexp) 0)
		       (setq file-filter-regexp fprojectp-last-file-filter-regexp-choosen)
		       (setq fprojectp-last-file-filter-regexp-choosen file-filter-regexp))
		   (list 'custom (list file-filter-regexp))))
		((eq query-mode 'file-extension)
		 ;; A list of file extension:
		 (let* ((def-string (if fprojectp-last-file-extension-list-choosen
					(concat " [default " (fprojectp-shorten-string fprojectp-last-file-extension-list-choosen 9) "]")
					""))
			(file-extension-list (read-from-minibuffer (format "Enter the list of extension separated by spaces%s: " def-string))))
		   (if (= (length file-extension-list) 0)
		       (setq file-extension-list fprojectp-last-file-extension-list-choosen)
		       (setq fprojectp-last-file-extension-list-choosen file-extension-list))
		   (list 'custom (list (concat "\\." (regexp-opt (split-string file-extension-list)) "$")))))
		(t (error "Unknown Query Mode")))))))


;;
;;  User command:
;;


(defun fsprojectp-add-project(&optional project-type project-main-file project-root-folder file-filter)
  "Select a FOLDER, a MAIN-FILE and a FILE-FILTER, then add all
files under the current folder and sub-folder matching the
FILE-FILTER will be added to the project."
  (interactive)
  (when (interactive-p)
    ;; Read the project-type
    (unless project-type
      (setq project-type (fsprojectp-choose-project-type)))
    ;; Read the project-main-file (if the project's type is 'blank' there is no root filename)
    (unless project-main-file
      (when (nth 1 project-type)
	(let* ((project-filter (nth 1 project-type))
	       (project-predicate (lambda (filename)
				    (and (not (string-equal filename "./"))
					 (not (string-equal filename "../"))
					 (or (file-directory-p filename)
					     (some '(lambda (item) (string-match item filename)) project-filter))))))
	  (while (or (not project-main-file)
		     (file-directory-p project-main-file)
		     (not (funcall project-predicate project-main-file)))
	    (let ((def-dir (and project-main-file (file-directory-p project-main-file) project-main-file)))
	      (setq project-main-file (read-file-name "Project Main File: " def-dir nil t nil project-predicate))
	      (message "pmf: %s" project-main-file)
	      )))))
    ;; Read the project-root-folder:
    (unless project-root-folder
      (let ((def-dir (if project-main-file
			 (file-name-directory project-main-file)
			 default-directory)))
	(while (or (not project-root-folder)
		   (= (length project-root-folder) 0))
	  (setq project-root-folder (read-directory-name "Project Root Folder: " def-dir def-dir t)))))
    ;; Read the file-filter:
    (unless file-filter
      (setq file-filter (fsprojectp-choose-file-filter)))
    )
  ;;
  ;;
  ;;
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
