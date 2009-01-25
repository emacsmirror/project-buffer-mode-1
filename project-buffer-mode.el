;; ewoc
;;
;; proj:folder/
;;
;;
;; Let's play with ewoc...
;;  sample code: git.el


;; Header displaying:
;;  . current project to build
;;  . platform to build (win32/...)
;;  . version to build (debug/release/...)
;;

;; Footer displaying:
;;  . currently building?
;;  . current project dependencies???


;; Flat view:

;;
;;     [+] ProjName1           <deps: ProjName3, ProjName2>
;;     [ ] ProjName2
;;   *     `- FolderA/File1
;;   *     `- FolderA/File2
;;         `- FolderB/File3
;;         `- FolderB/File4
;;         `- FolderB/File5
;;     [+] ProjName3
;;  

;; Folder view:

;;     [+] ProjName1           <deps: ProjName3, ProjName2>
;;     [ ] ProjName2
;;          `- FolderA
;;   *          `- File1
;;   *          `- File2
;;          ++ FolderB
;;     [+] ProjName3
;;  

;; Sample:
;;   [-] other
;;        `- foo.cpp
;;   [-] test1
;;        `- sources
;;        |   `- abc.cpp
;;        |   `- gfr.cpp
;;        +- extras
;;        `- headers
;;        |   `- xtra.h
;;   [-] test1
;;        `- sources/abc.cpp
;;        `- sources/gfr.cpp
;;        `- headers/xtra.h
;;   [-] test2
;;        `- apl.c
;;        `- roo.c
;;        `- zzz.h



;;
;;   vcproj search patterns:
;;    <files>
;;      <File RelarivePath="...">
;;        <FileConfiguration Name="config|platform"
;;      </File>
;;      <filter Name="Folder Name In VC"
;;         ...
;;         <File RelativePath="real\path\to\file.ext"
;;         
;; <FileConfiguration
;;   Name="DebugFast|Win32"
;;   ExcludedFromBuild="true"
;; >
;;


;; Shortcut:
;;    m    -> mark file
;;    u    -> unmark file
;;    +    -> collapse/expand folder/project
;;    v    -> Toggle view mode (flat / flat with the foldershidden / folder)
;;
;; Shortcut todo:
;;    /    -> find file name
;;    n    -> next file matching regexp
;;    p    -> prev file matching regexp
;;   <RET> -> open file at cursor pos
;;    c    -> compile current file / marked files? [?]
;;    t    -> toggle marked files
;;    f    -> open marked files
;;    h    -> find corresponding header/source
;;   <SPC> -> collapse/expand folder/project + move to the next line
;;    Fe   -> set filter on extension
;;    Fn   -> set filter on filename
;;    Fd   -> set filter on internal directory
;;    g    -> reload/reparse sln/vcprojs files
;;    ?    -> show brief help!!
;;    >    -> go to the next object of the same type: next file / folder / project
;;    <    -> go to the previous object of the same type: next file / folder / project
;;   <BCK> -> 
;;   <TAB> -> cycle through: collapse current folder / expand folder / expand all folders inside
;;   <LFT> -> expand if collapsed move to the first folder; move inside if expanded
;;   <RGT> -> move up if folded collapsed; collapse if in front of folder ; move to the folded if in front of a file
;; C-<DWN> -> move to the next project
;; C-<UP>  -> move to the previous project

;;
;;    B    -> launch build
;;    C    -> launch clean
;;    D    -> launch run/with debugger
;;    R    -> launch runexit !
;;    T    -> touch marked files
;;
;; Future improvement:
;;    d    -> show/hide project dependencies
;;    b    -> buils marked files
;;    S    -> seach in all marked files
;;  C-M    -> marked files based on regexp???

;; To find how to do:
;; - Grayed out exclude from build files??
;; - Different coler for files referenced in the proj but don't exist?
;; - Auto reload if file modified on disk?
;;

(require 'cl)
(require 'ewoc)
(require 'enum-type)

;;

(defvar project-buffer-status nil)
(defvar project-buffer-view-mode nil)

;;

(defenum project-buffer-item-type      'file 'project 'folder)
(defenum project-buffer-view-mode-type 'flat-view 'folder-hidden-view 'folder-view) 

;; Structure to store data attached to each ewoc-node.
;; Each node represents either a file or a project or a folder indide the project"
(defstruct (project-buffer-node
	    (:copier nil)
	    (:constructor project-buffer-create-node (name type filename project &optional hidden))
	    (:conc-name project-buffer-node->))
  name              ;; string displayed to represent the file (usually the file.ext)
  type              ;; project? file? folder?

  marked            ;; is the file/project marked?
  hidden            ;; hidden files (currently: = project/folder close)
  collapsed         ;; is the folder/project collapsed or not?
  project-collapsed ;; t if the project the file belong to is collapsed

  filename          ;; full path to the filename
  project           ;; name of the project the file belongs to
)

;;(require 'record-type)
;;(defrecord project-buffer-data
;;  "Structure to store the project buffer data"
;;  :name  'stringp
;;  :sln   'stringp
;;  :items 'listp
;;)

;;

(defun project-buffer-convert-name-for-display(node-data)
  (let ((node-name (project-buffer-node->name node-data)))
    (cond ((eq project-buffer-view-mode 'flat-view) 
	   (concat " `- " node-name))
	  ((eq project-buffer-view-mode 'folder-hidden-view)
	   (concat " `- " (file-name-nondirectory node-name)))
	  ((eq project-buffer-view-mode 'folder-view)
	   (let ((dir-list (split-string node-name "/"))
		 (str (if (project-buffer-node->collapsed node-data) " `+ " " `- "))
		 (cur 1))
	     (while (< cur (length dir-list)) 
	       (setq str (concat " |  " str)
		     cur (1+ cur)))
	     (concat str (file-name-nondirectory node-name) )))
	  (t (format "Unknown view mode: %S" project-buffer-view-mode) ))))

(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-collapsed (project-buffer-node->collapsed node))
	(node-name   (project-buffer-node->name  node))
	(node-marked (project-buffer-node->marked node))
	(node-type   (project-buffer-node->type node))
	(node-hidden (project-buffer-node->hidden node))
	(node-prjcol (project-buffer-node->project-collapsed node))
	)
    (if (or (and (eq project-buffer-view-mode 'folder-view)
		 (message "Folder view")
		 (not node-hidden))
	    (and (not (eq project-buffer-view-mode 'folder-view))
		 (message "View %S " project-buffer-view-mode)
		 (not (eq node-type 'folder))
		 (not node-prjcol))
	    (eq node-type 'project))
	(insert (concat " " 
			(if node-marked "*" " ")
			" "
			(cond ((not (eq node-type 'project)) "   ")
			      (node-collapsed                "[+]")
			      (t                             "[-]"))
			" "
			(or (and (eq node-type 'project)  node-name)
			    (project-buffer-convert-name-for-display node))
			"\n")))))


(defun project-buffer-mark-file()
  "Mark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (setf (project-buffer-node->marked node-data) t)
    (ewoc-invalidate project-buffer-status node)
    (ewoc-goto-next project-buffer-status 1)))

(defun project-buffer-unmark-file()
  "Unmark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (setf (project-buffer-node->marked node-data) nil)
    (ewoc-invalidate project-buffer-status node)
    (ewoc-goto-next project-buffer-status 1)))

(defun project-buffer-toggle-expand-collapse()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - nothing will be done."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status)
	 prj-sel
	 hidden-flag
	 project 
	 skip-under
	 folder)
    (unless (eq (project-buffer-node->type node-data) 'file)
      (when (eq (project-buffer-node->type node-data) 'folder)
	(setq folder (project-buffer-node->name node-data)))
      (setf (project-buffer-node->collapsed node-data) (not (project-buffer-node->collapsed node-data)))
      (setq hidden-flag (project-buffer-node->collapsed node-data))
      (setq prj-sel (eq (project-buffer-node->type node-data) 'project))
      (when prj-sel
	(setf (project-buffer-node->project-collapsed node-data) hidden-flag))
      (ewoc-invalidate status node)
      (setq project (project-buffer-node->project node-data)
	    node (ewoc-next status node))
      (while node
	(setq node-data (ewoc-data node))
	(when skip-under
	  (unless (project-buffer-parent-of-p (project-buffer-node->name  node-data) skip-under)
	    (setq skip-under nil)))
	(if (and (string-equal (project-buffer-node->project node-data) project)
		 (or (not folder)
		     (project-buffer-parent-of-p (project-buffer-node->name  node-data) folder)))
	    (progn
	      (when prj-sel
		(setf (project-buffer-node->project-collapsed node-data) hidden-flag)
		(ewoc-invalidate status node))
	      (unless skip-under
		(setf (project-buffer-node->hidden node-data) hidden-flag)
		(ewoc-invalidate status node)
		(if (and (eq (project-buffer-node->type node-data) 'folder)
			 (project-buffer-node->collapsed node-data)
			 (not hidden-flag))
		    (setq skip-under (project-buffer-node->name node-data))))
	      (setq node (ewoc-next status node)))
	    (setq node nil))))))

(defun project-buffer-toggle-view-mode()
  "Toggle between the different view mode (folder-view / flag-view / folder-hidden-view)"
  (interactive)
  (save-excursion
    (unless project-buffer-status (error "Not in project-buffer buffer."))
    (setq project-buffer-view-mode
	  (cond ((eq project-buffer-view-mode 'folder-view)        'flat-view)
		((eq project-buffer-view-mode 'flat-view)          'folder-hidden-view)
		((eq project-buffer-view-mode 'folder-hidden-view) 'folder-view)))
    (ewoc-refresh project-buffer-status)))
  
;; unused
(defun project-buffer-get-current-item()
  (ewoc-data (ewoc-locate project-buffer-status)))

;; 
;; split / files...
;; 

;; Define the key mapping for the spu mode:
(defvar project-buffer-mode-map
  (let ((project-buffer-mode-map (make-keymap)))
    (define-key project-buffer-mode-map [?+] 'project-buffer-toggle-expand-collapse)
    (define-key project-buffer-mode-map [?\t] 'project-buffer-toggle-expand-collapse)
    (define-key project-buffer-mode-map [?m] 'project-buffer-mark-file)
    (define-key project-buffer-mode-map [?u] 'project-buffer-unmark-file)
    (define-key project-buffer-mode-map [?v] 'project-buffer-toggle-view-mode)
    project-buffer-mode-map))


(defun project-buffer-mode()
  "Entry point to the project-buffer-mode."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (use-local-map project-buffer-mode-map)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer-prettyprint "" "" t)))
      (make-local-variable 'project-buffer-status)
      (make-local-variable 'project-buffer-view-mode)
      (setq project-buffer-status status)
      (setq project-buffer-view-mode 'folder-view)
      (project-buffer-refresh-ewoc-hf status))))

(defun project-buffer-refresh-ewoc-hf(status)
  "Refresh ewoc header/footer"
  (ewoc-set-hf status 
	       (concat "Booh Header\n"
		       "\n\n") ""))

(defun project-buffer-refresh-nodes(status)
  "Refresh displayed buffer"
  (ewoc-map (lambda (data) t)
	    status
	    ))
  
;;(ewoc-data (ewoc-locate project-buffer-status))
;;(ewoc-invalidate project-buffer-status pos)
;;(ewoc-goto-prev project-buffer-status 1)))
;;(ewoc-goto-next project-buffer-status 1)))
;;      (file-name-nondirectory "test/tess")
;;      (file-name-directory "test/tess")


(defun project-buffer-extract-folder (name type)
  (cond ((eq type 'folder) name)
	((eq type 'project) nil)
	(t (let ((dirname (file-name-directory name)))
	     (and dirname (substring dirname 0 -1))))))

(defun project-buffer-directory-lessp (dir1 dir2 type2)
  (let* ((list1  (and dir1 (split-string dir1 "/")))
	 (list2  (and dir2 (split-string dir2 "/")))
	 (cnt 0))
    (if (and list1 list2)
	(progn (while (and (< cnt (length list1))
			   (< cnt (length list2))
			   (string= (nth cnt list1) (nth cnt list2)))
		 (setq cnt (1+ cnt)))
	       (if (and (< cnt (length list1))
			(< cnt (length list2)))
		   (string-lessp (nth cnt list1) (nth cnt list2))
		   (and (eq type2 'file) 
			(< cnt (length list1)))
		   ))
	(null list2))))

(defun project-buffer-parent-of-p (child parent)
  "Check if PARENT is a parent directory of CHILD"
  (let* ((clist (and child  (split-string child "/")))
	 (plist (and parent (split-string parent "/")))
	 (cont t)
	 res)
    (while (and clist plist cont)
      (let ((cname (pop clist))
	    (pname (pop plist)))
	(setq cont (string-equal cname pname))))
    (and cont (null plist))))
	    
  

(defun project-buffer-find-node-up(status node &optional any-parent-ok)
  "Return the directory or project in which the node belong
This may change depending on the view mode
If ANY-PARENT-OK is set, any parent found will be valid"
  (let* ((cur  (ewoc-prev status node))
	 (fold (file-name-directory (project-buffer-node->name (ewoc-data node))))
	 (proj (project-buffer-node->project (ewoc-data node)))
	 found)
    (setq fold (and fold (substring fold 0 -1))) 
    (while (and cur (not found))
      (let* ((data    (ewoc-data cur))
	     (db-name (project-buffer-node->name data))
	     (db-type (project-buffer-node->type data))
	     (db-proj (project-buffer-node->project data)))
	(cond ((and fold
		    (eq db-type 'folder)
		    (eq project-buffer-view-mode 'folder-view)
		    (or (string-equal fold db-name)
			(and any-parent-ok
			     (project-buffer-parent-of-p fold db-name))))
	       (setq found cur))
	      ((and (eq db-type 'project)
		    (string-equal proj db-name))
	       (setq found cur))
	      ((not (string-equal proj db-proj))
	       (setq cur nil)))
	(setq cur (and cur (ewoc-prev status cur)))))
    found))


;;
;; TODO:
;; - need to consider if folder is collapsed or expanded
;; - need to consider if project is collapsed or expanded
;; - also need to consider the view mode!
;;   in general the view mode affects only folders!
;;



(defun project-buffer-insert (status data) ; same as pretty print, assume data is cons (project . filename)
  "Insert a file at the right place in it's project."
  (let ((node        (ewoc-nth status 0))
	(folder-data (project-buffer-extract-folder (project-buffer-node->name data)      (project-buffer-node->type data)))
	(name-data   (file-name-nondirectory (project-buffer-node->name data)))
	(type-data   (project-buffer-node->type data))
	(node-data   nil)
	(here        nil) 
	(proj-found  nil)
	(folder      nil)
	(hidden-flag nil)
	(skip        nil))
    (when (eq type-data 'folder)
      (error "Not supported -- in particular project-buffer-directory-lessp may returns a incorrect value"))
    (while (and node (not here) (not skip))
      (setq node-data (ewoc-data node))
      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp (project-buffer-node->project data) (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq here node)
	    (setq here (and proj-found node)
		  skip (not proj-found))))

       ;; node.project == data.project -> check folder/file name
       ((string-equal (project-buffer-node->project node-data) (project-buffer-node->project data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq skip t)
	    (let* ((folder-db   (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
		   (name-db     (file-name-nondirectory (project-buffer-node->name node-data)))
		   (type-db     (project-buffer-node->type node-data)))
	      ;; we're still on the project line???
	      (unless (eq type-db 'project)
		(if (and folder-db folder-data)
		    (cond ((project-buffer-directory-lessp folder-data folder-db type-db)
			   (setq here node))

			  ((string-equal folder-data folder-db)
			   (setq folder folder-data)
			   (if (eq type-data 'folder)
			       (setq skip t)
			       (unless (eq type-db 'folder)
				 (when (string-lessp name-data name-db)
				   (setq here node)))))

			  (t (setq folder folder-db)))

		    (unless folder-db 
		      (if folder-data
			  (setq here node)
			  (when (string-lessp name-data name-db)
			    (setq here node)))))))
	      (setq proj-found t))
	))

       ;; Carry on...
       (setq node (ewoc-next status node)))

    ;; Insert before here...
    (when (not skip)
      ;; Once the node added we will need to check if it should be hidden or not. 
      ;; At first, if it's a file, it will be hidden to not have any glitch in the displayed buffer
      (unless (eq type-data 'project)
	(setf (project-buffer-node->hidden data) t))
      (if here 
	  (setq node (ewoc-enter-before status here data))
	  (setq node (ewoc-enter-last status data)))
      ;;
      (unless (eq type-data 'project)
	(let* ((shown t)
	       (parent (project-buffer-find-node-up status node t)))
	  (setf (project-buffer-node->project-collapsed data) (project-buffer-node->project-collapsed (ewoc-data parent)))
	  (setq shown (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	  (while (and parent 
		      shown
		      (not (eq (project-buffer-node->type (ewoc-data parent)) 'project)))
	    (setq parent (project-buffer-find-node-up status parent))
	    (setq shown  (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	    )
	  (setq hidden-flag (not shown)))
	(unless hidden-flag 
	  (setf (project-buffer-node->hidden data) nil)
	  (ewoc-invalidate status node)))

      (when folder-data
	(let* ((db-list     (and folder (split-string folder "/")))
	       (curr-list   (split-string folder-data "/"))
	       (cnt 0))
	  (while (and (< cnt (length curr-list))
		      (< cnt (length db-list))
		      (string= (nth cnt db-list) (nth cnt curr-list)))
	    (setq cnt (1+ cnt)))
	  ;; Add the extra folder:
	  (if (< cnt (length curr-list))
	      (let ((ndx 0)
		    (str nil))
		(while (< ndx cnt)
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))
		  (setq ndx (1+ ndx)))
		(while (< ndx (length curr-list))
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))
		  (ewoc-enter-before status 
				     node
				     (project-buffer-create-node str 'folder folder (project-buffer-node->project data) hidden-flag))
		  (setq ndx (1+ ndx)))))
	  ))
)))

;;(split-string "/test/blah/" "/")

;      (when proj-found
;	(let ((folder-data (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data))))
;	  (split-string
;	  ))

;;(compare-strings "abcdef" nil nil "abc" nil nil)


;;
;; Interactive commands:
;;


(defun test-projbuff()
  (interactive)
  (let ((buffer (generate-new-buffer "test-project-buffer")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (project-buffer-mode)

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project "test1.sln" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/gfr.cpp" 'file  "~/temp/gfr.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/abc.cpp" 'file  "~/temp/abc.cpp" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project "test2.sln" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "header/zzz.h" 'file  "~/temp/zzz.h" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/roo.c" 'file  "~/temp/roo.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "script.awk" 'file "~/temp/script.awk" "test2"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "header/xtra.h" 'file "~/temp/xtra.h" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "blah.h" 'file "~/temp/blah.h" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/zzz.cpp" 'file  "~/temp/zzz.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "aha.h" 'file "~/temp/aha.h" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "other" 'project  "other.sln" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "test.h" 'file "~/temp/test.h" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/apl.c" 'file  "~/temp/apl.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "src/foo.cpp" 'file  "~/temp/foo.c" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2.h" 'file "~/temp/test2.h" "other"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "fold" 'project  "fold.sln" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/dee/test.c" 'file  "~/test.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/dee/grr.c" 'file  "~/grr.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/testw.c" 'file  "~/testw.c" "fold")) 
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/rdf.c" 'file  "~/rdf.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/def/gla.c" 'file  "~/gla.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "blue/green/red.c" 'file  "~/red.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/gth.c" 'file  "~/gth.c" "fold"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc/rrr/zgth.c" 'file  "~/zgth.c" "fold"))
)))

(defun test-projbuff-old()
  (interactive)
  (let ((buffer (generate-new-buffer "test-project-buffer")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (project-buffer-mode)

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project "test1.sln" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "gfr.cpp" 'file  "~/temp/gfr.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc.cpp" 'file  "~/temp/abc.cpp" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project "test2.sln" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "zzz.h" 'file  "~/temp/zzz.h" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "roo.c" 'file  "~/temp/roo.c" "test2"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "xtra.h" 'file "~/temp/xtra.h" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "other" 'project  "other.sln" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "apl.c" 'file  "~/temp/apl.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "foo.cpp" 'file  "~/temp/foo.c" "other"))
)))





(defun sln-extract-project(sln-file)
  "Extract projects from the SLN file"
  (save-excursion
    (with-temp-buffer
      (insert-file sln-file)
      (goto-char (point-min))
      (let ((result nil))
	(while (re-search-forward "Project(\"{[-A-Z0-9]+}\")[ 	]+=[ 	]+\"\\([A-Za-z0-9_]+\\)\"[ 	]*,[ 	]+\"\\([\\A-Za-z0-9_.]+\\)\""
				  (point-max)  t) 
	  (add-to-list 'result (cons (match-string-no-properties 1) (match-string-no-properties 2))))
	result))))

(defun create-project-buffer(sln-file)
  "Create a project buffer"
  (let ((buffer (create-file-buffer sln-file))
	(sln-projects (sln-extract-projects sln-file))
	current) ;; list of proj-nane / project file
    (while sln-projects
      (setq current (pop sln-projects))
      (vcproj-extract 
    ;;(switch-to-buffer buffer)
    ;;(project-buffer-mode)
    buffer))))