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

;; TODO:
;;  - show project dependencies
;;  - test color in dark background 
;;  - adding button to collapse/expand folders/projects
;;  - make sure no interactive function are complicated!!! (cf: toggle-expand-collapsed)
;;     it's better to create a function and call it in the command
;;  - flag to be have new created folded collaped or not // same with project 
;;  - flag to for the cursor to always to back up during the research
;;  - flag to for the cursor to always to the top of the file for researchs
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
;; BUG:
;;  - if the research fail searching forward shouldn't it go to the last matching one?



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
;;    t    -> toggle marked files
;;    M    -> mark all
;;    U    -> unmark all
;;    +    -> collapse/expand folder/project
;;   <TAB> -> collapse/expand folder/project
;;    f    -> open marked files
;;    v    -> Toggle view mode (flat / flat with the foldershidden / folder)
;;   <RET> -> open file at cursor pos
;; C-<DWN> -> move to the next project
;; C-<UP>  -> move to the previous project
;;    q    -> quit project-buffer
;;    ?    -> show brief help!!
;;    /    -> find file name
;;    n    -> next file matching regexp
;;    p    -> prev file matching regexp
;;   <BCK> -> go to parent
;; <C-LFT> -> expand if collapsed move to the first folder; move inside if expanded
;; <C-RGT> -> move up if folded collapsed; collapse if in front of folder ; move to the folded if in front of a file
;;
;; Future improvement:
;;    g    -> reload/reparse project files
;;    c    -> compile current file / marked files? [?]
;;    B    -> launch build
;;    C    -> launch clean
;;    D    -> launch run/with debugger
;;    R    -> launch runexit !
;;    T    -> touch marked files
;;    h    -> find corresponding header/source
;;    d    -> show/hide project dependencies
;;    b    -> buils marked files
;;    S    -> seach in all marked files
;;    s    -> mark files containing regexp...

;; To find how to do:
;; - Grayed out exclude from build files??
;; - Different coler for files referenced in the proj but don't exist?
;; - Auto reload if file modified on disk?
;;



;; Sample:
;; -------
;;
;; (defun test-projbuff()
;;   (interactive)
;;   (let ((buffer (generate-new-buffer "test-project-buffer")))
;;     (display-buffer buffer)
;;     (with-current-buffer buffer
;;       (cd "~/temp")
;;       (project-buffer-mode)
;; 
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project "test1.sln" "test1"))
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "src/gfr.cpp" 'file  "~/temp/gfr.cpp" "test1"))
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "src/abc.cpp" 'file  "~/temp/abc.cpp" "test1"))
;; 
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project "test2.sln" "test2"))
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "header/zzz.h" 'file  "~/temp/zzz.h" "test2"))
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "src/roo.c" 'file  "~/temp/roo.c" "test2"))
;;       (project-buffer-insert project-buffer-status (project-buffer-create-node "script.awk" 'file "~/temp/script.awk" "test2"))
;; )))




(require 'cl)
(require 'ewoc)

;;
;;  Global Variables:
;;

(defvar project-buffer-status nil)
(defvar project-buffer-view-mode nil)
(defvar project-buffer-cache-project nil)
(defvar project-buffer-cache-subdirectory nil)


;;
;;  Data type:
;;

;; Structure to store data attached to each ewoc-node.
;; Each node represents either a file or a project or a folder indide the project"
(defstruct (project-buffer-node
	    (:copier nil)
	    (:constructor project-buffer-create-node (name type filename project &optional hidden))
	    (:conc-name project-buffer-node->))
  name              ;; string displayed to represent the file (usually the file.ext)
  type              ;; project? file? folder?

  marked            ;; is the file marked?
  hidden            ;; hidden files (currently: = project/folder close)
  collapsed         ;; is the folder/project collapsed or not?
  project-collapsed ;; t if the project the file belong to is collapsed

  matched           ;; the file matches the regexp search

  filename          ;; full path to the filename
  project           ;; name of the project the file belongs to
)


;;
;;  Font
;;

(defgroup project-buffer nil
  "A special mode to manager project files."
)

(defface project-buffer-project-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Project buffer mode face used to highlight project nodes."
  :group 'project-buffer)

(defface project-buffer-folder-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Project buffer mode face used to highlight folder nodes."
  :group 'project-buffer)

(defface project-buffer-file-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Project buffer mode face used to highlight file nodes."
  :group 'project-buffer)

(defface project-buffer-project-button-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh [ and ] in front of the project name."
  :group 'project-buffer)
  
(defface project-buffer-indent-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used to highlight indent characters."
  :group 'project-buffer)

(defface project-buffer-mark-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "tomato")))
  "Project buffer mode face used highligh marks."
  :group 'project-buffer)

(defface project-buffer-filename-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh file names."
  :group 'project-buffer)

(defface project-buffer-matching-file-face
  '((default (:inherit project-buffer-file-face :bold t)))
  "Project buffer mode face used matching file."
  :group 'project-buffer)




;;
;;  Key Bindings:
;;

;; Define the key mapping for the spu mode:
(defvar project-buffer-mode-map
  (let ((project-buffer-mode-map (make-keymap)))
    (define-key project-buffer-mode-map [?+] 'project-buffer-toggle-expand-collapse)
    (define-key project-buffer-mode-map [?\t] 'project-buffer-toggle-expand-collapse-even-on-file)
    (define-key project-buffer-mode-map [?m] 'project-buffer-mark-file)
    (define-key project-buffer-mode-map [?u] 'project-buffer-unmark-file)
    (define-key project-buffer-mode-map [?M] 'project-buffer-mark-all)
    (define-key project-buffer-mode-map [?U] 'project-buffer-unmark-all)
    (define-key project-buffer-mode-map [?t] 'project-buffer-toggle-all-marks)
    (define-key project-buffer-mode-map [?v] 'project-buffer-toggle-view-mode)
    (define-key project-buffer-mode-map [?f] 'project-buffer-find-marked-files)
    (define-key project-buffer-mode-map [?/] 'project-buffer-search-forward-regexp)
    (define-key project-buffer-mode-map [?n] 'project-buffer-goto-next-match)
    (define-key project-buffer-mode-map [?p] 'project-buffer-goto-prev-match)
    (define-key project-buffer-mode-map [backspace] 'project-buffer-goto-dir-up)
    (define-key project-buffer-mode-map [?\ ] 'project-buffer-next-file)
    (define-key project-buffer-mode-map [(shift ?\ )] 'project-buffer-prev-file)
    (define-key project-buffer-mode-map [return] 'project-buffer-find-file)
    (define-key project-buffer-mode-map [?o] 'project-buffer-find-file-other-window)
    (define-key project-buffer-mode-map [(control left)] 'project-buffer-goto-dir-up-or-collapsed)
    (define-key project-buffer-mode-map [(control right)] 'project-buffer-next-file-or-expand)
    (define-key project-buffer-mode-map [(control up)] 'project-buffer-go-to-previous-folder-or-project)
    (define-key project-buffer-mode-map [(control down)] 'project-buffer-go-to-next-folder-or-project)
    (define-key project-buffer-mode-map [??] 'project-buffer-help)
    (define-key project-buffer-mode-map [?q] 'project-buffer-quit)
    project-buffer-mode-map))


;;
;;  Internal Utility Functions:
;;


(defun project-buffer-mark-matching-file(status regexp)
  "Check each file name and mark the files matching the regular expression REGEXP"
  (let ((node (ewoc-nth status 0)))
    (while node
      (let* ((node-data (ewoc-data node))
	     (node-type (project-buffer-node->type node-data))
	     (node-name (project-buffer-node->name node-data))
	     (file      (file-name-nondirectory node-name)))
	(when (string-match regexp file)
	  (let ((parent (project-buffer-find-node-up status node)))
	    (while (and parent
			(not (eq (project-buffer-node->type (ewoc-data parent)) 'project))
			(not (project-buffer-node->matched (ewoc-data parent))))
	      (setf (project-buffer-node->matched (ewoc-data parent)) t)
	      (ewoc-invalidate status parent)
	      (setq parent (project-buffer-find-node-up status parent))
	      ))
	  (setf (project-buffer-node->matched node-data) t)
	  (ewoc-invalidate status node)
	  ))
      (setq node (ewoc-next status node)))))

(defun project-buffer-clear-matched-mark(status)
  "Clear 'matched' flag"
  (let (result)
    (ewoc-map (lambda (node)
		(when (project-buffer-node->matched node)
		  (setf (project-buffer-node->matched node) nil)
		  (setq result t)))
	      status)
    result))


(defun project-buffer-get-marked-nodes(status)
  "Return the list of marked node or the current node if none are marked"
  (or (ewoc-collect status (lambda (node) (project-buffer-node->marked node)))
      (list (ewoc-data (ewoc-locate status)))))


(defun project-buffer-convert-name-for-display(node-data)
  "Convert the node name into the displayed string depending on the project-buffer-view-mode."
  (let* ((node-name  (project-buffer-node->name node-data))
	 (file-color (if (project-buffer-node->matched node-data) 'project-buffer-matching-file-face 'project-buffer-file-face))
	 (node-color (if (eq (project-buffer-node->type node-data) 'file) file-color 'project-buffer-folder-face)))
    (cond ((eq project-buffer-view-mode 'flat-view) 
	   (concat (propertize " `- " 'face 'project-buffer-indent-face) 
		   (and (file-name-directory node-name) 
			(propertize (file-name-directory node-name) 'face 'project-buffer-folder-face))
		   (propertize (file-name-nondirectory node-name) 'face file-color)))
	  ((eq project-buffer-view-mode 'folder-hidden-view)
	   (concat (propertize " `- " 'face 'project-buffer-indent-face) 
		   (propertize (file-name-nondirectory node-name) 'face file-color)))
	  ((eq project-buffer-view-mode 'folder-view)
	   (let ((dir-list (split-string node-name "/"))
		 (str (if (project-buffer-node->collapsed node-data) " `+ " " `- "))
		 (cur 1))
	     (while (< cur (length dir-list)) 
	       (setq str (concat " |  " str)
		     cur (1+ cur)))
	     (concat (propertize str 'face 'project-buffer-indent-face) 
		     (propertize (file-name-nondirectory node-name) 'face node-color) )))
	  (t (format "Unknown view mode: %S" project-buffer-view-mode) ))))


(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-collapsed (project-buffer-node->collapsed node))
	(node-name     (project-buffer-node->name  node))
	(node-marked   (project-buffer-node->marked node))
	(node-type     (project-buffer-node->type node))
	(node-hidden   (project-buffer-node->hidden node))
	(node-matching (project-buffer-node->matched node))
	(node-prjcol   (project-buffer-node->project-collapsed node)))
    (when (or (and (eq project-buffer-view-mode 'folder-view)
		   (or (not node-hidden)
		       node-matching))
	      (and (not (eq project-buffer-view-mode 'folder-view))
		   (not (eq node-type 'folder))
		   (or (not node-prjcol)
		       node-matching))
	      (eq node-type 'project))
      (insert (concat " " 
		      (if node-marked (propertize "*" 'face 'project-buffer-mark-face)" ")
		      " "
		      (cond ((not (eq node-type 'project)) "   ")
			    (node-collapsed                (propertize "[+]" 'face 'project-buffer-project-button-face) )
			    (t                             (propertize "[-]" 'face 'project-buffer-project-button-face) ))
		      " "
		      (or (and (eq node-type 'project)  (propertize node-name 'face 'project-buffer-project-face))
			  (project-buffer-convert-name-for-display node))))
	(when (and (eq project-buffer-view-mode 'folder-hidden-view)
		   (project-buffer-node->filename node)
		   (eq (project-buffer-node->type node) 'file))
	  (indent-to-column 40)
	  (insert (concat " " (propertize (project-buffer-node->filename node) 
					 'face 'project-buffer-filename-face))))
	(insert "\n")
)))


(defun project-buffer-mode()
  "Major mode to view project.

Commands:
\\{project-buffer-mode-map}"
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
      (make-local-variable 'project-buffer-cache-project)
      (make-local-variable 'project-buffer-cache-subdirectory)

      (setq project-buffer-status status)
      (setq project-buffer-view-mode 'folder-view)
      (setq project-buffer-cache-project nil)
      (setq project-buffer-cache-subdirectory nil)

      (project-buffer-refresh-ewoc-hf status))))


(defun project-buffer-refresh-ewoc-hf(status)
  "Refresh ewoc header/footer"
  (ewoc-set-hf status 
	       (concat "Project Buffer Mode:\n"
		       "\n\n") ""))


(defun project-buffer-extract-folder (name type)
  "Return the folder associated to the node's name"
  (cond ((eq type 'folder) name)
	((eq type 'project) nil)
	(t (let ((dirname (file-name-directory name)))
	     (and dirname (substring dirname 0 -1))))))


(defun project-buffer-directory-lessp (dir1 dir2 type2)
  "Return t if dir1 is less than (dir2,type2)"
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


(defun project-buffer-insert (status data) ; same as pretty print, assume data is cons (project . filename)
  "Insert a file at the right place in it's project."
  (let ((node           (ewoc-nth status 0))
	(folder-data    (project-buffer-extract-folder (project-buffer-node->name data)      (project-buffer-node->type data)))
	(name-data      (file-name-nondirectory (project-buffer-node->name data)))
	(type-data      (project-buffer-node->type data))
	(proj-data      (project-buffer-node->project data))
	(node-data      nil)
	(here           nil) 
	(proj-found     nil)
	(folder         nil)
	(hidden-flag    nil)
	(skip           nil)
	(proj-root-node nil)
	(folder-node    nil)
	)
    (when (eq type-data 'folder)
      (error "Not supported -- in particular project-buffer-directory-lessp may returns a incorrect value"))
    

    ;; Cache check:
    (when project-buffer-cache-project
      (cond 
       ;; cache-project < current-project -> we can start the search from here (at least).
       ((string-lessp (car project-buffer-cache-project) proj-data)
	(setq node (cdr project-buffer-cache-project)
	      project-buffer-cache-subdirectory nil))

       ;; cache-project == current-project -> check the folders...
       ((string-equal (car project-buffer-cache-project) proj-data)
	;; cache-subdir < current-subdir -> we can start from here.
	;; cache-subdir = current-subdir -> good starting point
	(if (and project-buffer-cache-subdirectory
		 folder-data
		 (or (string-equal (car project-buffer-cache-subdirectory) folder-data)
		     (project-buffer-directory-lessp (car project-buffer-cache-subdirectory) folder-data 'folder)))
	    (setq node (cdr project-buffer-cache-subdirectory)
		  proj-root-node (cdr project-buffer-cache-project)
		  proj-found t)
	    (setq node (cdr project-buffer-cache-project)
		  project-buffer-cache-subdirectory nil)))
       ;; other wise: cache miss...
       (t 
	(setq project-buffer-cache-project nil
	      project-buffer-cache-subdirectory nil))))
	 
    ;; Search where to insert the node:
    (while (and node (not here) (not skip))
      (setq node-data (ewoc-data node))
      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq here node)
	    (setq here (and proj-found node)
		  skip (not proj-found))))

       ;; node.project == data.project -> check folder/file name
       ((string-equal proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    ;; If we're trying to add the project when the project already exist... we'll skip it.
	    (setq skip t)
	    ;; Otherwise:
	    (let* ((folder-db   (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
		   (name-db     (file-name-nondirectory (project-buffer-node->name node-data)))
		   (type-db     (project-buffer-node->type node-data)))
	      ;; Are we're on the project line???
	      (if (eq type-db 'project)
		  (setq proj-root-node node)
		  (if (and folder-db folder-data)
		      ;; Both the current node and the new one have a directory
		      (cond ((project-buffer-directory-lessp folder-data folder-db type-db)
			     (setq here node))
			    
			    ((string-equal folder-data folder-db)
			     (when (eq type-db 'folder)
			       (setq folder-node node))
			     (setq folder folder-data)
			     (if (eq type-data 'folder)
				 (setq skip t)
				 (unless (eq type-db 'folder)
				   (when (string-lessp name-data name-db)
				     (setq here node)))))
			    
			    (t (setq folder folder-db)))
		      ;; Either:
		      ;; - the current node has no folder, meaning:
		      ;;   -> either the new node has a directory in which case we'll add it here.
		      ;;   -> or we'll search for the right place to add it.
		      ;; - the current node has a folder, meaning:
		      ;;   -> the new one has no folder, therefore, we need to carry on until we reach the no-folder area.
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
	(setf (project-buffer-node->hidden data) t)
	(unless proj-root-node 
	  (error "Project '%s' not found" proj-data)))
      (if here 
	  (setq node (ewoc-enter-before status here data))
	  (setq node (ewoc-enter-last status data)))
      (when (eq type-data 'project)
	(setq proj-root-node node))

      ;;

      ;; If it's not a project type, search up in all possible parent to see if the node is supposed to be visible or not
      ;; TODO: SLOW! Possible improvement: make the code doubled linked list! :) *or at least add a parent node to each nodes*
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
		  (setq folder-node (ewoc-enter-before status 
						       node 
						       (project-buffer-create-node str 'folder folder proj-data hidden-flag)))
		  (setq ndx (1+ ndx)))))
	  ))
      )

    ;; Save the project root node:
    ;; - to speed up the next insert (we stop looking for the project if it's the same one)
    (setq project-buffer-cache-project (cons proj-data proj-root-node))
    (setq project-buffer-cache-subdirectory (and folder-node
						 (cons folder-data folder-node)))
))




;;
;;  Interactive commands:
;;


(defun project-buffer-goto-dir-up()
  "Go to the project/folder containing the current file/folder"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (setq node (and node (project-buffer-find-node-up status node)))
    (when node
      (ewoc-goto-node status node))))
	
(defun project-buffer-goto-dir-up-or-collapsed()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (project-buffer-node->collapsed node-data))
	  (progn (setq node (and node (project-buffer-find-node-up status node)))
		 (when node (ewoc-goto-node status node)))
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-search-forward-regexp(regexp)
  "Search file matching REGEXP"
  (interactive "sSearch forward (regexp): ")
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (project-buffer-clear-matched-mark project-buffer-status)
  (when (and regexp 
	     (> (length regexp) 0)) 
    (let* ((status project-buffer-status)
	   (node (ewoc-locate status)))
      (project-buffer-mark-matching-file project-buffer-status regexp)
      ;; goto first match
      (while (and node
		  (or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		      (not (project-buffer-node->matched (ewoc-data node)))))
	(setq node (ewoc-next status node)))
      (if node
	(ewoc-goto-node status node)
	(message "Failing forward search."))
)))


(defun project-buffer-goto-next-match()
  "Go to the next matching"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-next status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-next status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing forward search."))))

(defun project-buffer-goto-prev-match()
  "Go to the previous matching"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-prev status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-prev status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing backward search."))))


(defun project-buffer-quit ()
  "Burry project-buffer mode or cancel the research."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (unless (project-buffer-clear-matched-mark project-buffer-status)
    (bury-buffer)))

(defun project-buffer-help ()
  "Display help for project-buffer mode."
  (interactive)
  (describe-function 'project-buffer-mode))


(defun project-buffer-next-file (&optional n)
  "Move the cursor down N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (ewoc-goto-next project-buffer-status n))


(defun project-buffer-next-file-or-expand()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (not (project-buffer-node->collapsed node-data)))
	  (ewoc-goto-next status 1)
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-prev-file (&optional n)
  "Move the cursor up N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (ewoc-goto-prev project-buffer-status n))


(defun project-buffer-find-marked-files()
  "Run find-files on the marked files"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((file-list (project-buffer-get-marked-nodes project-buffer-status))
	 (cnt 0)
	 buffer)
    (project-buffer-clear-matched-mark project-buffer-status)
    (while file-list
      (let ((node (pop file-list)))
	(when (eq (project-buffer-node->type node) 'file)
	  (setq buffer (find-file-noselect (project-buffer-node->filename node))
		cnt (1+ cnt)))))
    (cond ((> cnt 1) (message "Find %i files." cnt))
	  ((= cnt 1) (display-buffer buffer))
	  (t (message "No files selected")))))


(defun project-buffer-go-to-previous-project()
  "Go to previous project line"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-prev status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-prev status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-previous-folder-or-project()
  "If the cursor is on a file, go up to the previous project/folder.
If the cursor is on a folder, search up for the previous project/folder.
If the cursor is on a project, go to previous project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node))))
    (cond ((eq (project-buffer-node->type node-data) 'file) 
	   (project-buffer-goto-dir-up))
	  ((eq (project-buffer-node->type node-data) 'folder)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (eq (project-buffer-node->type (ewoc-data search)) 'file))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  ((eq (project-buffer-node->type node-data) 'project)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  (t (error "Unknown node type! (%S)" (project-buffer-node->type node-data))))))


(defun project-buffer-go-to-next-project()
  "Go to next project line"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-next status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-next-folder-or-project()
  "If the cursor is on a file, go down to the next project/folder.
If the cursor is on a folder, search down for the next project/folder.
If the cursor is on a project, go to next project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node)))
	 (fold-ok   (and node 
			 (not (eq (project-buffer-node->type node-data) 'project))
			 (eq project-buffer-view-mode 'folder-view)))
	 (search    (and node (ewoc-next status node))))
  
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project))
		(not (and fold-ok
			  (eq (project-buffer-node->type (ewoc-data search)) 'folder))))
	    (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))




(defun project-buffer-find-file()
  "Open the file that the cursor is on."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(find-file (project-buffer-node->filename node-data))
	(project-buffer-toggle-expand-collapse))))

(defun project-buffer-find-file-other-window()
  "Open the file that the cursor is on in another window."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(find-file-other-window (project-buffer-node->filename node-data))
	(project-buffer-toggle-expand-collapse))))

(defun project-buffer-mark-file()
  "Mark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (when (eq (project-buffer-node->type node-data) 'file) 
      (setf (project-buffer-node->marked node-data) t)
      (ewoc-invalidate project-buffer-status node))
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


(defun project-buffer-mark-all()
  "Mark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (not (project-buffer-node->marked node)))
                             (setf (project-buffer-node->marked node) t))) 
	    project-buffer-status))

(defun project-buffer-unmark-all()
  "Unmark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (project-buffer-node->marked node))
                             (setf (project-buffer-node->marked node) nil) t))
	    project-buffer-status))

(defun project-buffer-toggle-all-marks()
  "Toggle all file marks."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (ewoc-map (lambda (node) (when (eq (project-buffer-node->type node) 'file)
			     (setf (project-buffer-node->marked node) (not (project-buffer-node->marked node))) t))
	    project-buffer-status))


(defun project-buffer-toggle-expand-collapse-even-on-file()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - search up for the nearest folder and collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer."))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status))
    (project-buffer-clear-matched-mark status)
    (when (eq (project-buffer-node->type node-data) 'file)
      (setq node (and node (project-buffer-find-node-up status node)))
      (when node (ewoc-goto-node status node)))
    (when node
      (project-buffer-toggle-expand-collapse))))

      

  

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
    (project-buffer-clear-matched-mark status)
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
  (let ((node (ewoc-locate project-buffer-status)))
    (unless project-buffer-status (error "Not in project-buffer buffer."))
    (setq project-buffer-view-mode
	  (cond ((eq project-buffer-view-mode 'folder-view)        'flat-view)
		((eq project-buffer-view-mode 'flat-view)          'folder-hidden-view)
		((eq project-buffer-view-mode 'folder-hidden-view) 'folder-view)))
    (ewoc-refresh project-buffer-status)
    (ewoc-goto-node project-buffer-status node)))

;;
(provide 'project-buffer-mode)