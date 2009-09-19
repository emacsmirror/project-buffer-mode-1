;;; project-buffer-occur.el --- Occur functionality for Project Mode
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.0
;; Keywords:    occur project buffer makefile filesystem management
;; Description: Occur Functionality for Project-Buffer-Mode
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

;;; History:
;;
;; v1.0: First official release.
;;


(require 'project-buffer-mode)


;;; Code:


(defgroup project-buffer-occur nil
  "An occur mode for project-buffer.")


;;
;;  Global configuration variable:
;;

(defvar project-buffer-occur-context-size 16
  "Size of the context stored for each occurrence; to help retrieving the data after modification.")


(defface project-buffer-occur-file-line
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "yellow")))
  "Project buffer occur face used to highlight file line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-line-number
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Project buffer occur face used to highlight line number."
  :group 'project-buffer-occur)


(defface project-buffer-occur-odd-matching-line
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Project buffer occur face used to highlight odd matching line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-even-matching-line
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray")))
  "Project buffer occur face used to highlight even matching line."
  :group 'project-buffer-occur)


(defface project-buffer-occur-highlight-matching-string
  '((((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:background "yellow")))
  "Project buffer occur face used to highlight the matching string."
  :group 'project-buffer-occur)



;;
;;  Key Bindings:
;;


;; Define the key mapping for the spu mode:
(defvar project-buffer-occur-map
  (let ((project-buffer-occur-map (make-keymap)))
;    (define-key project-buffer-occur-mode-map [return] 'project-buffer-node-find-file)
  ;;  ret - goto-occurence 
  ;;  o   - goto-occurence other window
  ;;  v   - display occurrence
  ;;  n   - next occurence / prev search occurrence
  ;;  p   - prev occurence / next search occurrence
  ;;  M-n - next file
  ;;  M-p - prev file
  ;;  r   - rename buffer
  ;;  g   - revert-buffer??? refresh the research
  ;;  q   - quit-window
  ;;  /   - quick research! :)

    (define-key project-buffer-occur-map [mouse-2] 'project-buffer-occur-mouse-find-file)
    project-buffer-occur-map))


;;
;;  Functions:
;;


(defun project-buffer-occur-clear-overlays()
  "Clear the project-buffer-occur overlays from the current buffer."
  (let ((ovl-lists (overlay-lists)))
    (mapcar (lambda (overlay)
	      (when (overlay-get overlay 'project-buffer-occur-tag)
		(delete-overlay overlay)))
	    (and ovl-lists
		 (append (car ovl-lists) (cdr ovl-lists))))))


(defun project-buffer-occur-get-and-clear-occur-buffer()
  "Retrieve the occur buffer and returns it.
If the buffer exists; the buffer is cleared.  If the buffer
doesn't exist, a new buffer is created and initialized with
project-buffer-occur-major-mode."
  (let ((buffer (get-buffer-create "*Project-Buffer-Occur*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(project-buffer-occur-clear-overlays)
	(erase-buffer))
      (project-buffer-occur-mode))
    buffer))


(defun project-buffer-occur-add-occurrence(file occurrence occurrence-num regexp)
  "Add an OCCURRENCE from FILE in the buffer.

FILE should be the file in which the occurrence has been found,
OCCURRENCE is a list of (#line matching-line before-string after-string).
OCCURRENCE-NUM represents the OCCURENCE-NUM'th occurrence found in FILE"

  (let ((occ-line-num   (car occurrence))
	(occ-line-str   (nth 1 occurrence))
	(occ-before-str (nth 2 occurrence))
	(occ-after-str  (nth 3 occurrence))
	(start-pos      (point))
	(cur-line       (line-number-at-pos (point))))
    (insert (propertize (format "%8i:" occ-line-num)
			'follow-link t
			'mouse-face 'highlight
			'face 'project-buffer-occur-line-number))
    (insert " ")
    (insert (propertize occ-line-str
			'follow-link t
			'mouse-face 'highlight
			'face (if (oddp occurrence-num) 
				  'project-buffer-occur-odd-matching-line
				  'project-buffer-occur-even-matching-line)))
    (when (not (= (point) (point-at-bol)))
      (insert "\n"))

    ;; Highlight matching string:
    (goto-char start-pos)
    (forward-char 10) ; skip the line number
    (while (re-search-forward regexp nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	(overlay-put overlay 'face 'project-buffer-occur-highlight-matching-string)
	(overlay-put overlay 'project-buffer-occur-tag t)
	))

    ;; Fix the indentation:
    (goto-char (point-max))
    (forward-line -1)
    (while (not (= cur-line (line-number-at-pos (point))))
      (goto-char (point-at-bol))
      (insert (propertize (make-string 10 32)
			  'follow-link t
			  'mouse-face 'highlight))
      (forward-line -1))

    ;; Data overlay:
    (let ((overlay (make-overlay start-pos (point-max))))
      (overlay-put overlay 'project-buffer-occur-tag t)
      (overlay-put overlay 'project-buffer-occur-context (list file occurrence regexp)))
    (goto-char (point-max))))


(defun project-buffer-occur-collect-occurrences(regexp)
  "Create a list of occurrences by searching REGEXP in the current buffer.

The return value is a list of ( line# matching-line before-string
after-string ).  This function doesn't save the position and
assume the position will be saved and restored by the caller if
required."
  (goto-char (point-min))
  (save-match-data
    (let (occurrences
	  next-start
	  occ-beg
	  occ-end
	  occ-bol
	  occ-eol
	  occ-line-num
	  occ-line-str
	  occ-before-str
	  occ-after-str)
      (setq next-start (re-search-forward regexp nil t))
      (while next-start
	;; Collect the data for this occcurrence:
	;;  consider using: jit-lock-fontify-now! To get colors on the line...
	(setq occ-beg (match-beginning 0))
	(setq occ-end (match-end 0))
	(goto-char occ-beg)
	(setq occ-bol (point-at-bol))
	(setq occ-line-num (line-number-at-pos))
	(goto-char occ-end)
	(setq occ-eol (point-at-eol))
	(setq occ-line-str (buffer-substring-no-properties occ-bol occ-eol))
	(setq occ-after-str (and (>= (- (point-max) occ-eol) project-buffer-occur-context-size)
				 (buffer-substring-no-properties occ-eol (+ occ-eol project-buffer-occur-context-size))))
	(setq occ-before-str (and (>= (- occ-bol (point-min)) project-buffer-occur-context-size)
				  (buffer-substring-no-properties (- occ-bol project-buffer-occur-context-size) occ-bol)))
	;; Add the occurrence to the list unless it occurs on the same line.
	(unless (eq occ-line-num (car (car occurrences)))
	  (setq occurrences (cons (list occ-line-num occ-line-str occ-before-str occ-after-str)
				  occurrences)))
	;; Carry on:
	(goto-char next-start)
	(setq next-start (re-search-forward regexp nil t)))
      (reverse occurrences)
    )))


(defun project-buffer-occur-research(project-file-name file-path project-name regexp occur-buffer)
  "Research REGEXP in FILE-PATH and fill OCCUR-BUFFER with the
different occurences found. 
PROJECT-FILE-NAME and PROJECT-NAME are ignored."
  (let (occurrences)
    (message "Project '%s' -- Searching in '%s'" project-name file-path)
    ;; Collect all occurrences in this file:
    (let ((file-buf (get-file-buffer file-path)))
      (if file-buf
	  (with-current-buffer file-buf
	    (save-excursion
	      (setq occurrences (project-buffer-occur-collect-occurrences regexp))))
	  (with-temp-buffer
	    (insert-file-contents file-path)
	    (setq occurrences (project-buffer-occur-collect-occurrences regexp)))))

    ;; Then populate the occurr buffer with it:
    (when occurrences
      (with-current-buffer occur-buffer
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (let ((start-pos (point)))
		(insert (propertize (format "%i occurrences found in %s" (length occurrences) project-file-name)
				    'follow-link t
				    'mouse-face 'highlight
				    'face 'project-buffer-occur-file-line))
		(let ((overlay (make-overlay start-pos (point))))
		  (overlay-put overlay 'project-buffer-occur-tag t)
		  (overlay-put overlay 'project-buffer-occur-context (list project-file-name nil regexp))))
	  (insert "\n")
	  (let ((occ-count 1))
	    (while occurrences
	      (let ((occurrence (pop occurrences)))
		(project-buffer-occur-add-occurrence project-file-name occurrence occ-count regexp)
		(setq occ-count (1+ occ-count))))))))))


(defun project-buffer-occur-mode()
  "Major mode."
  (kill-all-local-variables)
  (use-local-map project-buffer-occur-map)
  ;;  
  (setq major-mode 'project-buffer-occur-mode)
  (setq mode-name "pbm-occur")
  ;(set (make-local-variable 'revert-buffer-function) 'occur-revert-function)
  ;(make-local-variable 'occur-revert-arguments)
  ;(add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
  ;(setq next-error-function 'occur-next-error)
  ;(run-mode-hooks 'occur-mode-hook)
  (setq buffer-read-only t)
  (setq buffer-undo-list t) ; disable undo recording
  )


(defun project-buffer-occur-goto-file(file &optional other-window)
  "Go to the selected files."
  (if other-window
      (find-file-other-window file)
      (find-file file)))


(defun project-buffer-occur-goto-matching-string(file line matching-line before-string after-string regexp &optional other-window)
  "Go to an occurrence."
  (let* ((current-buffer (find-file-noselect file))
	(window (get-buffer-window current-buffer)))
    (if window 
	(select-window window)
	(if other-window 
	    (switch-to-buffer-other-window current-buffer)
	    (switch-to-buffer current-buffer)))
    (set-buffer current-buffer)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))
    ;; note: need to make sure the file hasn't change since last time
    ;; so basically need to refine using before-string and after-string
    ;; finally; search regexp between the two spots. 
    ;; ALSO another todo: 
    ;; consider highlighting all occurrence of the string (or at least the current one!) in the buffer.
    ;; with a clear-highlight stuff as soon as a key is pressed!
    ))


(defun project-buffer-occur-goto-occurrence(pos)
  "Go to the occurence found at POS."
  (let (context)
    ;; Check if there is a context at that line:
    (mapcar (lambda (overlay) (when (overlay-get overlay 'project-buffer-occur-context)
				(setq context (overlay-get overlay 'project-buffer-occur-context))))
	    (overlays-at pos))
    (unless context
      (error "No occurrence on this line"))
    (let ((file-name (car context))
	  (occurrence (nth 1 context))
	  (regexp (nth 2 context)))
      (if occurrence
	  (let ((occ-line-num   (car occurrence))
		(occ-line-str   (nth 1 occurrence))
		(occ-before-str (nth 2 occurrence))
		(occ-after-str  (nth 3 occurrence)))
	    (project-buffer-occur-goto-matching-string file-name occ-line-num occ-line-str occ-before-str occ-after-str regexp t))
	  (project-buffer-occur-goto-file file-name t)))))
  

;;
;;  Interactive commands:
;;


(defun project-buffer-occur-mouse-find-file(event)
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (project-buffer-occur-goto-occurrence (posn-point (event-end event)))))


(defun project-buffer-occur(regexp all-files)
  "Search REGEXP in the project files; if ALL-FILES is t the
research will occur in all project's files; if ALL-FILES is
false, the research will occur in all marked files unless there
are none in which case it will occur in all files of the current
project (current project is determined by the cursor position)."
  (interactive
   (list (project-buffer-read-regexp (format "List lines matching regexp%s: " (if current-prefix-arg " [all files]" "")))
	 current-prefix-arg))
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless (and regexp (not (string-equal regexp "")))
    (error "Invalid regexp."))
  ;; Generate an occur buffer:
  (let ((occur-buffer (project-buffer-occur-get-and-clear-occur-buffer)))
    ;; Fill the occur buffer with all occurrences:
    (if all-files 
	(project-buffer-apply-to-each-file 'project-buffer-occur-research regexp occur-buffer)
	(unless (project-buffer-apply-to-marked-files 'project-buffer-occur-research regexp occur-buffer)
	  (project-buffer-apply-to-project-files (project-buffer-get-current-project-name)
						 'project-buffer-occur-research regexp occur-buffer)))
    ;; Reparse the occur buffer to add file headers:

    (display-buffer occur-buffer)
    (message "Done.")))
  
