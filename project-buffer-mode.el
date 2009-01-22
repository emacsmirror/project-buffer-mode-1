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

(defvar project-buffer-status nil)

;;

;;(defstruct (project-buffer-fileinto
;;	    (:copier nil)
;;	    (:constructor project-buffer-create-fileinfo (name project type filename))
;;	    (:conc-name project-buffer-fileinfo->))
;;  type      ;; project? file? folder?
;;  filename  ;; full path to the filename
;;  name      ;; string displayed to represent the file (usually the file.ext)
;;  project   ;; name of the project the file belongs to
;;  marked    ;; is the file marked?
;;  state     ;; project/folder state: 'open 'close 'disabled  // file: 'hidden (a disabled project doesn't show any files...)
;;)

(require 'record-type)
(require 'enum-type)


(defenum project-buffer-item-type     'file 'project 'folder)
(defenum project-buffer-node-state    'none 'opened 'closed 'disabled)

(defrecord project-buffer-data
  "Structure to store the project buffer data"
  :name  'stringp
  :sln   'stringp
  :items 'listp
)

(defrecord project-buffer-node
  "Structure to store data attached to each ewoc-node.
Each node represents either a file or a project or a folder indide the project"
  :name     'stringp

  :state    'project-buffer-item-state-p
  :type     'project-buffer-item-type-p
  :hidden   'booleanp
  :marked   'booleanp
  :filtered 'booleanp

  :file     'stringp 
)

;;

(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-state  (get-project-buffer-node-state node))
	(node-name   (get-project-buffer-node-name  node))
	(node-marked (get-project-buffer-node-marked node))
	(node-type   (get-project-buffer-node-type node))
	)
    (if (or (not (eq node-state 'hidden))
	    (eq node-state 'filtered
    (insert (concat " " 
		    (if node-marked "*" " ")
		    " "
		      (cond ((not (eq node-type 'project)) "   ")
			    ((eq node-state 'open')        "[-]")
			    ((eq node-state 'close)        "[+]")
			    ((eq node-state 'disabled)     "[x]")
			    (t "[?]"))
		      (or (and (eq node-type 'project)  node-name)
			  (concat 
	    


  "Pretty-printer function"		; assume it's currently just a cons (project . filename)
  (if (or (not (cdr data))
	  project-buffer-show-hidden
	  (/= (car (string-to-list (cdr data))) ??))
      (insert (concat "   "
		      (if (cdr data) "     `- " "[-] ")
		      (or (cdr data) (car data))
		      "\n"
		      ))))

;; 
;; split / files...
;; 

(defun project-buffer-mode()
  "Entry point to the project-buffer-mode"
  (interactive)
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
  (ewoc-map (lambda (data) (if (cdr data) t))
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
	(project-found nil)
	(prev          nil))
    (while (and node (not here))
      (setq node-data (ewoc-data node))
      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp (car data) (car node-data))
	(setq here node))

       ;; node.project == data.project -> check file name
       ((string-equal (car node-data) (car data))
	(when (and (cdr node-data)
		   (string-lessp (cdr data) (cdr node-data)))
	  (setq here node))
	(setq project-found t)))
      ;; Carry on...
      (setq prev node
	    node (ewoc-next status node)))
    ;; Insert before here...
    (if (not project-found)
	(if here 
	    (ewoc-enter-before status here (cons (car data) nil))
	    (ewoc-enter-last status (cons (car data) nil))))
    (when (cdr data)
      (if here 
	  (ewoc-enter-before status here data)
	  (ewoc-enter-last status data)))))

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