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
;;    f    -> show/hide folders
;;    fe   -> set filter on extension
;;    fn   -> set filter on filename
;;    fd   -> set filter on internal directory
;;    g    -> reload/reparse sln/vcprojs files
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



(defvar project-buffer-status nil)

;;

(defstruct (project-buffer-fileinto
	    (:copier nil)
	    (:constructor project-buffer-create-fileinfo (name project type filename))
	    (:conc-name project-buffer-fileinfo->))
  type      ;; project? file? folder?
  filename  ;; full path to the filename
  name      ;; string displayed to represent the file (usually the file.ext)
  project   ;; name of the project the file belongs to
  marked    ;; is the file marked?
  state     ;; project/folder state: 'open 'close 'disabled  // file: 'hidden (a disabled project doesn't show any files...)
)
			  

;;

(defun project-buffer--prettyprint(data)
  "Pretty-printer function"
  
 )

(defun project-buffer-mode()
  "Entry point to the project-buffer-mode"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer--prettyprint "" "")))
      (make-local-variable 'project-buffer-status)
      (setq project-buffer-status status))))



(defun create-project-buffer(bufname)
  "Create a project buffer"
  (let ((buffer (create-file-buffer bufname)))
    (switch-to-buffer buffer)
    (project-buffer-mode)
    buffer))