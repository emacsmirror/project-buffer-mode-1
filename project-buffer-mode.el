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
;;    /    -> find file name
;;    n    -> next file matching regexp
;;    p    -> prev file matching regexp
;;   <RET> -> open file
;;    c    -> compile current file / marked files? [?]
;;    m    -> mark file
;;    u    -> unmark file
;;    t    -> toggle marked files
;;    o    -> open marked files
;;    h    -> find corresponding header/source
;;    +    -> collapse/uncollapse folder/project
;;    fe   -> set filter on extension
;;    fn   -> set filter on filename
;;    fd   -> set filter on internal directory
;;    g    -> reload/reparse sln/vcprojs files
;;    v    -> Toggle view mode (flat / flat with the foldershidden / folder)
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

;; To find how to do:
;; - Grayed out exclude from build files??
;; - Different coler for files referenced in the proj but don't exist?
;; - Auto reload if file modified on disk?
;;

(require 'ewoc)
(require 'enum-type)

;;

(defvar project-buffer-status nil)

;;

(defenum project-buffer-item-type     'file 'project 'folder)
(defenum project-buffer-node-state    'none 'opened 'closed 'disabled)

;; Structure to store data attached to each ewoc-node.
;; Each node represents either a file or a project or a folder indide the project"
(defstruct (project-buffer-node
	    (:copier nil)
	    (:constructor project-buffer-create-node (name type state filename project))
	    (:conc-name project-buffer-node->))
  name      ;; string displayed to represent the file (usually the file.ext)
  type      ;; project? file? folder?
  state     ;; project/folder state: 'open 'close 'disabled  // (a disabled project doesn't show any files... / exempt during filter search)

  marked    ;; is the file/project marked?
  hidden    ;; hidden files (currently: = project/folder close)

  filename  ;; full path to the filename
  project   ;; name of the project the file belongs to
)

;;(require 'record-type)
;;(defrecord project-buffer-data
;;  "Structure to store the project buffer data"
;;  :name  'stringp
;;  :sln   'stringp
;;  :items 'listp
;;)

;;

(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-state  (project-buffer-node->state node))
	(node-name   (project-buffer-node->name  node))
	(node-marked (project-buffer-node->marked node))
	(node-type   (project-buffer-node->type node))
	)
    (insert (concat " " 
		    (if node-marked "*" " ")
		    " "
		    (cond ((not (eq node-type 'project)) "   ")
			  ((eq node-state 'open)        "[-]")
			  ((eq node-state 'close)        "[+]")
			  ((eq node-state 'disabled)     "[x]")
			  (t "[?]"))
		    " "
		    (or (and (eq node-type 'project)  node-name)
			(concat " `- " node-name))
		    "\n"))))

;; 
;; split / files...
;; 

(defun project-buffer-mode()
  "Entry point to the project-buffer-mode"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer-prettyprint "" "" t)))
      (make-local-variable 'project-buffer-status)
      (make-local-variable 'project-buffer-show-hidden) ;; tmp hack
      (setq project-buffer-show-hidden nil)
      (setq project-buffer-status status)
      (project-buffer-refresh-ewoc-hf status))))

(defun project-buffer-show-hidden()
  (interactive)
  (setq project-buffer-show-hidden (not project-buffer-show-hidden))
  (project-buffer-refresh-ewoc-hf project-buffer-status)
  (project-buffer-refresh-nodes project-buffer-status)
)

(defun project-buffer-refresh-ewoc-hf(status)
  "Refresh ewoc header/footer"
  (ewoc-set-hf status 
	       (concat "Booh Header\n"
		       "Showing Hidden File: " (or (and project-buffer-show-hidden "shown") "hidden")
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


(defun project-buffer-insert (status data) ; same as pretty print, assume data is cons (project . filename)
  "Insert a file at the right place in it's project."
  (let ((node          (ewoc-nth status 0))
	(node-data     nil)
	(here          nil) 
	(proj-found    nil)
	(skip          nil))
    (while (and node (not here) (not skip))
      (setq node-data (ewoc-data node))
      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp (project-buffer-node->project data) (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq here node)
	    (setq here (and proj-found node)
		  skip (not proj-found))))

       ;; node.project == data.project -> check file name
       ((string-equal (project-buffer-node->project node-data) (project-buffer-node->project data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq skip t)
	    (when (and (not (eq (project-buffer-node->type node-data) 'project))
		       (string-lessp (project-buffer-node->name data) (project-buffer-node->name node-data))
	      (setq here node))))
	(setq proj-found t)
	))

      ;; Carry on...
      (setq node (ewoc-next status node)))

    ;; Insert before here...
    (when (not skip)
      (if here
	  (ewoc-enter-before status here data)
	  (ewoc-enter-last status data)))))



(defun test-projbuff()
  (interactive)
  (let ((buffer (generate-new-buffer "test-project-buffer")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (project-buffer-mode)

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test1" 'project 'open "test1.sln" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "gfr.cpp" 'file 'none "~/temp/gfr.cpp" "test1"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "abc.cpp" 'file 'none "~/temp/abc.cpp" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "test2" 'project 'open "test2.sln" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "zzz.h" 'file 'none "~/temp/zzz.h" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "roo.c" 'file 'none "~/temp/roo.c" "test2"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "xtra.h" 'file 'none "~/temp/xtra.h" "test1"))

      (project-buffer-insert project-buffer-status (project-buffer-create-node "other" 'project 'open "other.sln" "other"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "apl.c" 'file 'none "~/temp/apl.c" "test2"))
      (project-buffer-insert project-buffer-status (project-buffer-create-node "foo.cpp" 'file 'none "~/temp/foo.c" "other"))
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
    buffer))