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
;; Key mapped:
;; C-c n   to add new project
;; C-c C-r to revert the project
;; C-x C-w to write the project
;; C-x C-s to save the project

;;
;; suggestion:
;;              C-c -   to delete a project
;;              C-c R   to rename the current project

(require 'project-buffer-mode)


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
   {projfile} path of the project's main file
   {root}     root folder of the project"
)

(defcustom fsprojectp-ignore-folder
  '(".git" ".svn" "bzr" ".hg" "CVS" ".CVS" "build" "lib" "Debug" "Release")
  "List of folder to ignore during the recursive search.")


;;
;; History:
;;


(defvar fsprojectp-project-type-history nil)
(defvar fsprojectp-file-filter-history nil)
(defvar fsprojectp-file-filter-query-history nil)
(defvar fsprojectp-file-filter-regexp-history nil)
(defvar fsprojectp-file-filter-extension-list-history nil)
(defvar fsprojectp-project-name-history nil)
(defvar fsprojectp-platforms-history nil)
(defvar fsprojectp-build-configurations-history nil)


;;
;;  Local variables:
;;

(defvar fsprojectp-last-project-type-choosen "makefile")
(defvar fsprojectp-last-filter-type-choosen "c++")
(defvar fsprojectp-last-file-filter-query-mode-choosen "regexp")
(defvar fsprojectp-last-file-filter-regexp-choosen nil)
(defvar fsprojectp-last-file-extension-list-choosen nil)
(defvar fsprojectp-platform-list nil)
(defvar fsprojectp-build-configuration-list nil)


;;
;;  Functions:
;;

(defun fsprojectp-choose-project-type()
  "Request and return the selected project type"
  (let* ((project-type-string (completing-read (format "Project Type [default %s]: " fsprojectp-last-project-type-choosen)
					       fsprojectp-project-type nil t nil 'fsprojectp-project-type-history fsprojectp-last-project-type-choosen))
	 (project-type (intern project-type-string)))
    (setq fsprojectp-last-project-type-choosen project-type-string)
    (assoc project-type fsprojectp-project-type)))

(defun fsprojectp-shorten-string(str max-lgt)
  "If the length of STR is greater than MAX-LGT; shorten the string adding '...' at the end."
  (if (> (length str) max-lgt)
      (concat (substring str 0 (- max-lgt 3)) "...")
      str))

(defun fsprojectp-choose-file-filter()
  "Read the file filter."
  (let* ((filter-type-string (completing-read (format "Filter Type [default %s]: " fsprojectp-last-filter-type-choosen)
					       fsprojectp-filters nil t nil 'fsprojectp-file-filter-history fsprojectp-last-filter-type-choosen))
	 (filter-type (intern filter-type-string)))
    (setq fsprojectp-last-filter-type-choosen filter-type-string)
    (if (not (eq filter-type 'custom))
	;; If not custom: return the selected file-filter:
	(assoc filter-type fsprojectp-filters)
	;; In case of custom file filter:
	;; Let's first ask how to specify the filter:
	(let* ((query-mode-string (completing-read (format "Enter the file system query mode (regexp, file-extension) [default %s]: " fsprojectp-last-file-filter-query-mode-choosen)
						   '("regexp" "file-extension") nil t nil 'fsprojectp-file-filter-query-history fsprojectp-last-file-filter-query-mode-choosen))
	       (query-mode (intern query-mode-string)))
	  (setq fsprojectp-last-file-filter-query-mode-choosen query-mode-string)
	  (cond ((eq query-mode 'regexp)
		 ;; A regexp:
		 (let* ((def-string (if fsprojectp-last-file-filter-regexp-choosen
					(concat " [default " (fsprojectp-shorten-string fsprojectp-last-file-filter-regexp-choosen 9) "]")
					""))
			(file-filter-regexp (read-from-minibuffer (format "Enter the file filter regexp%s: " def-string)
								  nil nil nil 'fsprojectp-file-filter-regexp-history)))
		   (if (= (length file-filter-regexp) 0)
		       (setq file-filter-regexp fsprojectp-last-file-filter-regexp-choosen)
		       (setq fsprojectp-last-file-filter-regexp-choosen file-filter-regexp))
		   (list 'custom (list file-filter-regexp))))
		((eq query-mode 'file-extension)
		 ;; A list of file extension:
		 (let* ((def-string (if fsprojectp-last-file-extension-list-choosen
					(concat " [default " (fsprojectp-shorten-string fsprojectp-last-file-extension-list-choosen 9) "]")
					""))
			(file-extension-list (read-from-minibuffer (format "Enter the list of extension separated by spaces%s: " def-string)
								   nil nil nil 'fsprojectp-file-filter-extension-list-history)))
		   (if (= (length file-extension-list) 0)
		       (setq file-extension-list fsprojectp-last-file-extension-list-choosen)
		       (setq fsprojectp-last-file-extension-list-choosen file-extension-list))
		   (list 'custom (list (concat "\\." (regexp-opt (split-string file-extension-list)) "$")))))
		(t (error "Unknown Query Mode")))))))


(defun fsprojectp-collect-files(root-folder file-filter-list &optional ignore-folders)
  "Parse ROOT-FOLDER and its sub-folder and create a list of full path filename matching one of the regexp of FILE-FILTER-LIST.
The folder defined inside in IGNORE-FOLDERS will be skipped."
  (let ((dir-list (directory-files-and-attributes root-folder t))
	(ign-reg  (regexp-opt ignore-folders))
	file-list)
    (while dir-list
      (let* ((cur-node (pop dir-list))
	     (fullpath (car cur-node))
	     (is-dir   (eq (car (cdr cur-node)) t))
	     (is-file  (not (car (cdr cur-node))))
	     (basename (file-name-nondirectory fullpath)))
	(cond
	 ;; if the current node is a directory different from "." or "..", all it's file gets added to the list
	 ((and is-dir
	       (not (string-equal basename "."))
	       (not (string-equal basename ".."))
	       (or (not ignore-folders)
		   (not (string-match ign-reg basename))))
	       (setq dir-list (append dir-list (directory-files-and-attributes fullpath t))))
	 ;; if the current node is a file
	 (is-file
	  ;; check against the file filter, if it succeed: add the file to the file-list
	  (when (some '(lambda (item) (string-match item basename)) file-filter-list)
	    (setq file-list (cons fullpath file-list)))
	  ))))
    file-list))


(defun fsprojectp-generate-user-data(action-string-list
				     project-name
				     project-main-file
				     project-root-folder)
  "Generate the project's user data based from ACTION-STRING-LIST.
ACTION-STRING-LIST is a list of string; each of them corresponding to the project actions.
This function returns a assoc-list of assoc-list such as:
  (cdr (assoc buildconfig (cdr (assoc platform data)))) should returns a list of user actions.

In each action string list may contain the following wildcard
which will be replaced by their respective value:
   {config}   the current selected build version
   {platform} the current selected platform
   {project}  name of the project
   {projfile} path of the project's main file
   {root}     root folder of the project"

  (let ((platform-list fsprojectp-platform-list)
	user-data)
    (while platform-list
      (let ((current-platform (pop platform-list))
	    (build-config-list fsprojectp-build-configuration-list)
	    bc-list)
	(setq user-data (cons (cons current-platform
				    (progn (while build-config-list
					     (let ((current-build-config (pop build-config-list)))
					       (setq bc-list (cons (cons current-build-config
									 (mapcar (lambda (action-string)
										   (let* ((repl1 (replace-regexp-in-string "{config}"   current-build-config  action-string))
											  (repl2 (replace-regexp-in-string "{platform}" current-platform      repl1))
											  (repl3 (replace-regexp-in-string "{project}"  project-name          repl2))
											  (repl4 (replace-regexp-in-string "{projfile}" project-main-file     repl3))
											  (repl5 (replace-regexp-in-string "{root}"     project-root-folder   repl4)))
										     repl5))
										 action-string-list))
								   bc-list))
					       ))
					   bc-list)
				      )
			      user-data))))
    user-data))

;(cdr (assoc "win32" (cdr (assoc "debug" '(("release" ("win32" . a) ("ppc" . b)) ("debug" ("win32" . c) ("ppc" . d))) ))))


;;
;;  User command:
;;


(defun fsprojectp-add-project(&optional project-type project-main-file project-root-folder project-name file-filter)
  "Select a FOLDER, a MAIN-FILE and a FILE-FILTER, then add all
files under the current folder and sub-folder matching the
FILE-FILTER will be added to the project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
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
	      )))))
    ;; Read the project-root-folder:
    (unless project-root-folder
      (let ((def-dir (if project-main-file
			 (file-name-directory project-main-file)
			 default-directory)))
	(while (or (not project-root-folder)
		   (= (length project-root-folder) 0))
	  (setq project-root-folder (read-directory-name "File Search - Root Folder: " def-dir def-dir t)))
	(unless (string-equal (substring project-root-folder -1) "/")
	  (setq project-root-folder (concat project-root-folder "/")))
	))
    ;; Read the project name:
    (unless project-name
      (while (not project-name)
	(setq project-name (read-from-minibuffer "Project Name: "
						 (file-name-nondirectory (substring project-root-folder 0 -1))
						 nil nil 'fsprojectp-project-name-history))
	(when (project-buffer-project-exists-p project-name)
	  (message "Project %s already exists!" project-name)
	  (sit-for 2)
	  (setq project-name nil))
	))
    ;; Read the file-filter:
    (unless file-filter
      (setq file-filter (fsprojectp-choose-file-filter)))
    )

  (let (file-list user-data)
    ;;
    ;; Collect the project's file
    ;;
    (setq file-list (fsprojectp-collect-files project-root-folder (nth 1 file-filter) fsprojectp-ignore-folder))

    ;;
    ;; Populate the project-buffer-mode:
    ;;

    ;; Generate the project node's user-data:
    (setq user-data (fsprojectp-generate-user-data (nth 2 project-type)
						   project-name
						   project-main-file
						   project-root-folder))					   
    ;; Add the project node
    (project-buffer-insert project-name 'project project-main-file project-name)
    (project-buffer-set-project-build-configurations project-name fsprojectp-build-configuration-list)
    (project-buffer-set-project-platforms            project-name fsprojectp-platform-list)
    (project-buffer-set-project-user-data            project-name user-data)

    ;; Add each individual files to the project:
    (mapcar (lambda (name)
	      (let* ((relative-path (file-relative-name name))
		     (full-path     (abbreviate-file-name name))
		     (file-name     (if (> (length relative-path) (length full-path)) full-path relative-path))
		     (proj-name     (substring name (length (expand-file-name project-root-folder)) (length name))))
		(project-buffer-insert proj-name 'file  file-name project-name)))
	    file-list)
    ;; Add the project's main file to the project:
    (when project-main-file
      (project-buffer-insert (file-name-nondirectory project-main-file) 'file  project-main-file project-name))
  ))

(defun fsprojectp-add-files-to-current-project(&optional root-folder file-filter)
  "Add extra files to the current project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((current-project (project-buffer-get-current-project-name)))
    (unless current-project (error "No current project found."))
    (when (interactive-p)
      ;; Read the root-folder:
      (unless root-folder
	(while (or (not root-folder)
		   (= (length root-folder) 0))
	    (setq root-folder (read-directory-name "File Search - Root Folder: " nil nil t)))
	(unless (string-equal (substring root-folder -1) "/")
	  (setq root-folder (concat root-folder "/"))))
      ;; Read the file-filter:
      (unless file-filter
	(setq file-filter (fsprojectp-choose-file-filter)))
      )

    (let (file-list user-data)
      ;; Collect the project's file
      (setq file-list (fsprojectp-collect-files root-folder (nth 1 file-filter) fsprojectp-ignore-folder))
      
      ;; Add each individual files to the project:
      (mapcar (lambda (name)
		(let* ((relative-path (file-relative-name name))
		       (full-path     (abbreviate-file-name name))
		       (file-name     (if (> (length relative-path) (length full-path)) full-path relative-path))
		       (proj-name     (substring name (length (expand-file-name root-folder)) (length name))))
		  (let ((exist     (project-buffer-exists-p proj-name current-project))
			(file-path (project-buffer-get-file-path proj-name current-project))
			(count 2))
		    (when exist
		      (if (and file-path (string-equal file-name file-path))
			  (setq proj-name nil) ; if the file is already present, skip it (note: the search is very basic; it is possible to trick the system and add a file twice...)
			  (setq proj-name (concat proj-name " (1)"))))
		    (while (and exist proj-name)
		      (setq exist (project-buffer-exists-p proj-name current-project))
      		      (setq file-path (project-buffer-get-file-path proj-name current-project))
		      (when exist
			(if (and file-path (string-equal file-name file-path))
			    (setq proj-name nil) ; if the file is already present, skip it
			    (setq proj-name (concat (substring proj-name 0 -2) (format "%i)" count))
				  count (1+ count)))))
		      (when proj-name ;; skip it?
			(project-buffer-insert proj-name 'file  file-name current-project)))))
	      file-list)
      )))

(defun fsprojectp-delete-current-node()
  "Delete the current node from the current project."
  (interactive)
  (let ((name (project-buffer-get-current-node-name))
	(type (project-buffer-get-current-node-type))
	(proj (project-buffer-get-current-project-name)))
    (when (and proj (yes-or-no-p (concat (format "Delete %s%s " name (if (eq type 'file) "" " and its content")))))
      (message "Deleting '%s' ..." name)
      (cond ((eq type 'file)
	     (project-buffer-delete-file name proj))
	    ((eq type 'folder)
	     (project-buffer-delete-folder name proj))
	    ((eq type 'project)
	     (project-buffer-delete-project name))
	    (t (error "Unknown data type"))))))



(defun fsprojectp-setup-local-key()
  "Define a local key-bindings."
  (local-set-key [(control ?c) ?n] 'fsprojectp-add-project)
  (local-set-key [(control ?c) ?+] 'fsprojectp-add-files-to-current-project)
  (local-set-key [(control ?c) ?d] 'fsprojectp-delete-current-node)

  (local-set-key [(control ?c) (control ?r)] 'project-buffer-revert)
  (local-set-key [(control ?x) (control ?s)] 'project-buffer-save-file)
  (local-set-key [(control ?x) (control ?w)] 'project-buffer-write-file))


;;
;;  User commands:
;;

(defun fsproject-new(name root-folder)
  "Entry function of thie project-mode."
  (interactive "sProject Buffer Name: \nDRoot Folder: ")
  (let ((buffer (generate-new-buffer (concat "fsx:" name))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (cd root-folder)
      (project-buffer-mode)
      ;; local variables:
      (make-local-variable 'fsprojectp-last-project-type-choosen)
      (make-local-variable 'fsprojectp-last-filter-type-choosen)
      (make-local-variable 'fsprojectp-last-file-filter-query-mode-choosen)
      (make-local-variable 'fsprojectp-last-file-filter-regexp-choosen)
      (make-local-variable 'fsprojectp-last-file-extension-list-choosen)
      (make-local-variable 'fsprojectp-platform-list)
      (make-local-variable 'fsprojectp-build-configuration-list)
      ;; register the local variable to be saved:
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-last-project-type-choosen)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-last-filter-type-choosen)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-last-file-filter-query-mode-choosen)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-last-file-filter-regexp-choosen)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-last-file-extension-list-choosen)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-platform-list)
      (add-to-list 'project-buffer-locals-to-save 'fsprojectp-build-configuration-list)
      ;; ask for the platform list:
      (setq fsprojectp-platform-list            (split-string (read-from-minibuffer "Enter the list of platforms separated by spaces: " nil nil nil 'fsprojectp-platforms-history)))
      (setq fsprojectp-build-configuration-list (split-string (read-from-minibuffer "Enter the list of build configurations separated by spaces: " nil nil nil 'fsprojectp-build-configurations-history)))
      ;;
      (fsprojectp-setup-local-key)
      )))
