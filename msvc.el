;;
;; Function to open a sln project in emacs:
;;  

;; TODO:
;;  - Auto reload if file modified on disk?
;;  - Add refresh command.
;;
;; Need to update the keys:
;;    g    -> reload/reparse project files (mmm should probably be done in the upper file or handler should be provided)

(require 'project-buffer-mode)
(require 'cl)


;;
;; Helper function:
;;

(defvar solution-name nil
  "Local variable to store the solution name")


(defun vcproj-extract-platforms (current-block)
  "Extract a list of platform from CURRENT-BLOCK"
  (unless (eq (car current-block) 'Platforms) (error "Expected a list like '(Platforms ...)"))
  (let ((data (cdddr current-block))
	cur ret)
    (while data
      (setq cur (pop data))
      (when (listp cur)
	(unless (eq (car cur) 'Platform) (error "Unknown id: '%S' expected 'Platform" (car cur)))
	(unless (eq (caaadr cur) 'Name)   (error "Unknown id: '%S' expected 'Name" (car cur)))
	(setq ret (cons (cdaadr cur) ret))))
    (reverse ret)))


(defun vcproj-extract-configurations (current-block)
  "Extract a list of configuration from CURRENT-BLOCK"
  (unless (eq (car current-block) 'Configurations) (error "Expected a list like '(Configurations ...)"))
  (let ((data (cdddr current-block))
	cur ret)
    (while data
      (setq cur (pop data))
      (when (listp cur)
	(unless (eq (car cur) 'Configuration) (error "Unknown id: '%S' expected 'Configuration" (car cur)))
	(let ((search-list (cadr cur))
	      name)
	  (while (and search-list (not name))
	    (let ((item (pop search-list)))
	      (setq name (and (eq (car item) 'Name) (cdr item)))))
	  (unless name (error "Unknown configuration name!"))
	  (setq ret (cons (car (split-string name "|")) ret)))))
    (reverse ret)))


(defun vcproj-extract-file(current-item)
  "Extract the relative path of the current file contain in CURRENT-ITEM"
  (unless (eq (car current-item) 'File) (error "Expected a list like '(File ...)"))
  (let ((data (cadr current-item))
	file)
    (while (and data (not file))
      (let ((cur (pop data)))
	(setq file (and (eq (car cur) 'RelativePath) (cdr cur)))))
    file))

	      
(defun vcproj-extract-filter-name(current-item)
  "Extract the filter name of the CURRENT-ITEM"
  (unless (eq (car current-item) 'Filter) (error "Expected a list like '(Filter ...)"))
  (let ((data (cadr current-item))
	filter)
    (while (and data (not filter))
      (let ((cur (pop data)))
	(setq filter (and (eq (car cur) 'Name) (cdr cur)))))
    filter))


(defun vcproj-extract-filter-list(current-item)
  "Extract the files/filter list attach to the current filter in CURRENT-ITEM"
    (unless (eq (car current-item) 'Filter) (error "Expected a list like '(Filter ...)"))
  (cddr current-item))


(defun vcproj-convert-file-list(file-list)
  "Convert FILE-LIST from a list '((\"virt-subfolder\" \"virt-subfolder\"...) \"full-path\") to a list '(\"virtual-folder\" \"full-path\")" 
  (let (ret)
    (while file-list
      (let* ((node (pop file-list))
	     (vnode (car node))
	     (fullpath (replace-regexp-in-string "\\\\" "/" (cdr node)))
	     (file (file-name-nondirectory fullpath))
	     (virt-folder (if vnode "/" "")))
	(while vnode
	  (let ((item (pop vnode)))
	    (setq virt-folder (concat item virt-folder))))
	(push (cons (concat virt-folder file) fullpath) ret)))
    ret))


(defun vcproj-extract-files(current-block)
  "Extract a list of files from CURRENT-BLOCK"
  (unless (eq (car current-block) 'Files) (error "Expected a list like '(Files ...)"))
  (let ((data (cdddr current-block))
	cur ret stack folder)
    (push data stack)
    (while stack
      (let ((node (pop stack)))
        (pop folder)
	(while node
	  (let ((item (pop node)))
	    (when (listp item)
	      (cond ((eq (car item) 'Filter)
		     (push node stack)
		     (push (vcproj-extract-filter-name item) folder)
		     (setq node (vcproj-extract-filter-list item)))
		    ((eq (car item) 'File)
		     (push (cons folder (vcproj-extract-file item)) ret))
		    (t (error "Unknown data - id: %S" (car item)))))))))
    (vcproj-convert-file-list ret)))
 
  

(defun vcproj-extract-data(vcproj-file)
  "Extract files and directory from the vcproj file"
  (save-excursion
    (let* ((xml-tags (with-temp-buffer
		       (insert-file vcproj-file)
		       (xml-parse-region (point-min) (point-max))))
	   (vs-data (car xml-tags))
	   (vs-tags  (and (eq (car vs-data) 'VisualStudioProject)
			  (cdddr vs-data)))
	   ;;
	   vc-platforms
	   vc-configurations
	   vc-files
	   )
      ;; 
      (while vs-tags
	(let ((cur-block (pop vs-tags)))
	  (when (listp cur-block)
	    (let ((block-tag (car cur-block)))
	      (cond ((eq block-tag 'Platforms)
		     (setq vc-platforms (append (vcproj-extract-platforms cur-block) vc-platforms)))
		    ((eq block-tag 'ToolFiles))     ; Currently ignored
		    ((eq block-tag 'Configurations)
		     (setq vc-configurations (append (vcproj-extract-configurations cur-block) vc-configurations)))
		    ((eq block-tag 'References))    ; Currently ignored
		    ((eq block-tag 'Files)
		     (setq vc-files (append (vcproj-extract-files cur-block) vc-files)))
		    ((eq block-tag 'Globals))       ; Currently ignored
		    (t (error (format "Unknown block tag: %S" block-tag)))) 
	    ))))
      (list vc-platforms vc-configurations vc-files))))


(defun vcproj-update-file-folders(vc-files folder)
  "Update the folder of each files in VC-FILES adding FOLDER in front of them"
  (mapcar '(lambda (item)
	     (cons (car item) 
		   (if (file-name-absolute-p (cdr item))
		       (cdr item)
		       (let ((rela-path (file-relative-name (expand-file-name (concat folder (cdr item)))))
			     (full-path (abbreviate-file-name (expand-file-name (concat folder (cdr item))))))
			 (if (> (length rela-path) (length full-path))
			     full-path
			     rela-path)))))
	  vc-files))


(defun sln-extract-projects(sln-file)
  "Extract projects from the SLN file"
  (save-excursion
    (with-temp-buffer
      (insert-file sln-file)
      (goto-char (point-min))
      (let ((result nil))
	(while (re-search-forward "Project(\"{[-A-Z0-9]+}\")[ 	]+=[ 	]+\"\\([A-Za-z0-9_]+\\)\"[ 	]*,[ 	]+\"\\([\\A-Za-z0-9_.]+\\)\""
				  (point-max)  t) 
	  (add-to-list 'result (cons (match-string-no-properties 1) (replace-regexp-in-string "\\\\" "/" (match-string-no-properties 2)))))
	result))))

(defun is-sln-file (filename)
  (or 
   (null (file-name-extension filename))
   (string= (file-name-extension filename) "sln")))

(defun sln-action-handler-2005(action project-name project-path platform configuration)
  "Project-Buffer action handler."
  (let ((sln-cmd (cond ((eq action 'build) "Build")
		       ((eq action 'clean) "Clean")
		       ((eq action 'run)   "RunExit")
		       ((eq action 'debug) "Debug"))))
    (compile 
     (concat "Devenv \"" solution-name "\" /" sln-cmd " \""  (concat configuration "|" platform) "\" /project \"" project-path "\""))))

(defun sln-action-handler-2008(action project-name project-path platform configuration)
  "Project-Buffer action handler."
  (let* ((prj-str (concat "/Project \"" project-path "\" "))
	 (cfg-str (concat "\"" configuration "|" platform "\" "))
	 (sln-cmd (cond ((eq action 'build) (concat "/Build " cfg-str))
			((eq action 'clean) (concat "/Clean " cfg-str))
			((eq action 'run)   (concat "/ProjectConfig " cfg-str "/RunExit "))
			((eq action 'debug) (concat "/ProjectConfig " cfg-str "/Run ")))))
    (compile 
     (concat "Devenv \"" solution-name "\" " 
	     prj-str sln-cmd))))


;;
;; Interactive command:
;;

(defun open-sln-project-buffer(sln-file)
  "Open a project buffer"
  (interactive "fSLN file: ")
  (let ((buffer (generate-new-buffer (concat "ms:" (file-name-nondirectory sln-file))))
	(sln-projects (sln-extract-projects sln-file)) ; list of proj-nane / project file
	)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      ;; Make sure the buffer path match the project's path
      (cd (file-name-directory sln-file))
      ;; Turn on the project-buffer-mode
      (project-buffer-mode)
      (make-local-variable 'solution-name)
      (setq solution-name (file-name-nondirectory sln-file))
      (add-hook 'project-buffer-action-hook 'sln-action-handler-2008 nil t)
      ;;
      (while sln-projects
	;; For every project reference in the SLN file,
	(let* ((current (pop sln-projects))
	       (project (car current))
	       (project-dir (file-name-directory (cdr current)))
	       (project-data (and (file-exists-p (cdr current))
				  (vcproj-extract-data (cdr current))))
	       (platforms (car project-data))
	       (configurations (cadr project-data)))
	  ;; Create a project node / update its platform and build configuration...
	  (project-buffer-insert project 'project (cdr current) project)
	  (project-buffer-set-project-platforms project platforms)
	  (project-buffer-set-project-build-configurations project configurations)
	  (when project-data
	    (let ((files (vcproj-update-file-folders (caddr project-data) project-dir)))
	      (while files		
		(let ((file (pop files)))
		  ;; then insert each project file into the buffer
		  (project-buffer-insert (car file) 'file (cdr file) project)))))
	  )))))

(defun create-sln-project-buffer()
  (interactive)
  (let ((solution-name (read-file-name "SLN file: " nil nil t nil 'is-sln-file)))
    (when (and solution-name 
	       (> (length solution-name) 0))
      (open-sln-project-buffer solution-name))))