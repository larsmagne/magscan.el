;;; magscan.el --- Scan Magazines -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: ocr

;; magscan is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; magscan is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:


;;; Code:

(require 'cl)
(require 'rmc)
(require 'tcor)

(defvar magscan-magazine "AH")

(defun magscan-find-device ()
  (let ((bits (split-string (file-truename "/dev/epson") "/")))
    (cons (car (last bits 2))
	  (car (last bits 1)))))

(defun magscan-scan (file mode &optional magazine width height rotate)
  (let ((device (magscan-find-device)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (apply #'call-process
	   `("~/src/sane/backends/frontend/scanimage"
	     nil ,(list (list :file file) nil) nil
	     ,(format "--mode=%s" mode)
	     "-d" ,(format "epsonds:libusb:%s:%s" (car device)
			   (cdr device))
	     "--resolution" "300dpi"
	     "--format=png"
	     ;; US comics size
	     ;;,@(and (not magazine) (list "-x" "260" "-y" "332"))
	     ,@(cond
		((or width height)
		 (list "-x" (format "%s" width) "-y" (format "%s" height)))
		((not magazine)
		 (list "-x" "260" "-y" "336"))
		;; Magazine size
		(t
		 ;;(list "-x" "268" "-y" "418"))
		 (list "-x" "280" "-y" "432")))))
    (start-process "reset" nil
		   "~/src/usbreset/usbreset"
		   (format "/dev/bus/usb/%s/%s"
			   (car device) (cdr device)))
    (when rotate
      (call-process "mogrify" nil nil nil
		    "-rotate" (format "%s" rotate)
		    (expand-file-name file)))))

(defun magscan-file (issue spec)
  (format "~/src/kwakk/magscan/%s/%s/%s"
	  magscan-magazine
	  (if (string-match "\\`[0-9]+\\'" issue)
	      (format "%03d" (string-to-number issue))
	    issue)
	  spec))

(defun magscan-magazine (issue &optional start)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \np")
  (magscan issue start t))

(defun magscan (issue &optional start magazine)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \np")
  (let ((i (if start
	       (/ start 2)
	     0))
	(buffer (current-buffer))
	colour
	file)
    (when (and (file-exists-p (magscan-file issue ""))
	       (= start 1)
	       (not (y-or-n-p (format "%s exists.  Really rescan?"
				      issue))))
      (error "Already exists"))
    (cl-loop for choice = (read-multiple-choice
			   (cond
			    ((= i 0)
			     "Scan front and back cover")
			    ((= i 1)
			     "Scan inside front and page 3")
			    (t
			     (format "Scan page %d and %d"
				     (- (* i 2) 0) (- (1+ (* i 2)) 0))))
			   '((?b "Yes")
			     (?q "Quit")
			     (?c "Colour next")
			     (?n "Redo previous")
			     (?p "Page number")))
	     while (not (eql (car choice) ?q))
	     when (or (eql (car choice) ?\r)
		      ;; Pedal.
		      (eql (car choice) ?b))
	     do (cl-incf i)
	     when (or (eql (car choice) ?c)
		      (eql (car choice) ?b))
	     do (setq colour t)
	     when (eql (car choice) ?p)
	     do (setq i (let ((number (read-string "Page number: ")))
			  (/ (+ (string-to-number number) 2)
			     2)))			  
	     do (setq file
		      (magscan-file
		       issue
		       (concat
			"pre-"
			(cond
			 ((= i 1)
			  "fc-bc")
			 (t
			  (format "%03d-%03d"
				  (- (* i 2) 2) (- (1+ (* i 2)) 2))))
			".png")))
	     (unless (eql (car choice) ?c)
	       (magscan-scan file 
			     (if (or (= i 1)
				     colour)
				 "color"
			       "gray")
			     magazine)
	       (magscan-display file)
	       (setq colour nil)))
    ;; Rename the first file now that we know how long the issue was.
    (rename-file (magscan-file issue "pre-fc-bc.png")
		 (magscan-file
		  issue (concat "pre-"
				(format "%03d-%03d.png"
					1 (* i 2)))))
    (pop-to-buffer buffer)))

(defun magscan-single-pages (issue width height &optional start)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \nnWidth: \nnHeight: \np")
  (let ((i start)
	colour
	file)
    (when (and (file-exists-p (magscan-file issue ""))
	       (= start 1)
	       (not (y-or-n-p (format "%s exists.  Really rescan?"
				      issue))))
      (error "Already exists"))
    (cl-loop for choice = (read-multiple-choice
			   (format "Scan page %d" i)
			   '((?b "Yes")
			     (?q "Quit")
			     (?c "Colour next")
			     (?n "Redo previous")
			     (?p "Page number")))
	     while (not (eql (car choice) ?q))
	     when (or (eql (car choice) ?c)
		      (eql (car choice) ?b))
	     do (setq colour t)
	     when (eql (car choice) ?p)
	     do (setq i (read-string "Page number: "))
	     do (setq file (magscan-file issue (format "page-%03d.png" i)))
	     (unless (eql (car choice) ?c)
	       (magscan-scan file
			     "color" nil
			     width height 180)
	       (magscan-display file 0)
	       (setq colour nil))
	     when (or (eql (car choice) ?\r)
		      ;; Pedal.
		      (eql (car choice) ?b))
	     do (cl-incf i)
	     )))

(defun magscan-display (file &optional rotation)
  (clear-image-cache)
  (pop-to-buffer "*scan*")
  (erase-buffer)
  (insert-image
   (create-image
    file
    nil nil
    :max-height (- (window-pixel-width) 60)
    :max-width (- (window-pixel-height) 60)
    :rotation (or rotation 90)))
  (goto-char (point-min)))

(defun magscan-image-size (file)
  (prog1
      (image-size (create-image file nil nil :scale 1) t)
    (clear-image-cache)))

(defun magscan-split-page (file)
  (let ((pages (cdr (split-string (replace-regexp-in-string
				   ".png$" "" (file-name-nondirectory file))
				  "-")))
	(size (magscan-image-size file)))
    ;; The front/back covers are scanned in opposite order than all
    ;; other pages.
    (when (equal (car pages) "001")
      (setq pages (nreverse pages)))
    (call-process "convert" nil nil nil
		  "-rotate" "90"
		  "-crop" (format "%sx%s-0-0" (/ (cdr size) 2) (car size))
		  (file-truename file)
		  (expand-file-name (format "page-%s.png" (car pages))
				    (file-name-directory file)))
    (call-process "convert" nil nil nil
		  "-rotate" "90"
		  "-crop" (format "%sx%s+%s-0"
				  (/ (cdr size) 2)
				  (car size)
				  (/ (cdr size) 2))
		  ;; "1949x3064+1949-0"
		  (file-truename file)
		  (expand-file-name (format "page-%s.png" (cadr pages))
				    (file-name-directory file)))))

(defun magscan-ocr (directory &optional dont-split)
  (unless dont-split
    (dolist (file (directory-files directory t "pre-.*png"))
      (message "Splitting %s" file)
      (magscan-split-page file)))
  (dolist (file (directory-files directory t "page-.*png"))
    (message "OCR-ing %s" file)
    (tcor-ocr file))
  (dolist (file (directory-files directory t "page-.*png"))
    (magscan-mogrify-jpeg file))
  (magscan-cover directory "AH"))

(defun magscan-cover (directory mag)
  (let ((dir (format "~/src/kwakk/covers/%s" mag)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (call-process
     "convert" nil nil nil
     "-normalize"
     "-resize" "150x"
     (let ((png (expand-file-name "page-001.png" directory)))
       (if (file-exists-p png)
	   png
	 (expand-file-name "page-001.jpg" directory)))
     (let ((path (split-string (directory-file-name directory) "/")))
       (expand-file-name
	(format "%s-%s.jpg"
		(car (last path 2))
		(car (last path 1)))
	dir)))))

(defun magscan-ocr-new ()
  ;; First do all new directories.
  (dolist (dir (directory-files "~/src/kwakk/magscan/IM/" t))
    (when (and (file-directory-p dir)
	       (not (member (file-name-nondirectory dir) '("." "..")))
	       (null (directory-files dir nil "json")))
      (magscan-ocr dir)))
  (magscan-count-pages "~/magscan/IM/"))

(defun magscan-mogrify-jpegs ()
  (dolist (file (directory-files-recursively "~/magscan/AH/" "page.*png"))
    (message "%s" file)
    (magscan-mogrify-jpeg file)))

(defun magscan-mogrify-rotate ()
  (dolist (file (directory-files-recursively "~/magscan/AH/" "page.*png"))
    (let* ((json (replace-regexp-in-string "[.]png$" ".json" file))
	   (rotation
	    (and (file-exists-p json)
		 (with-temp-buffer
		   (insert-file-contents json)
		   (and (re-search-forward "TextOrientation.:.\\([0-9]+\\)"
					   nil t)
			(match-string 1))))))
      (when (and rotation
		 (not (equal rotation "0")))
	(setq rotation (string-to-number rotation))
	(message "%s" file)
	(magscan-mogrify-jpeg file (format "%d" (- 360 rotation)))))))

(defun magscan-mogrify-jpeg (file &optional rotation)
  (if (string-match "grayscale" (with-temp-buffer
				  (call-process "file" nil (current-buffer)
						nil
						(expand-file-name file))
				  (buffer-string)))
      (apply #'call-process "convert" nil nil nil file
	     `("-level" "0%,45%"
	       "-quality" "80"
	       ,@(and rotation (list "-rotate" rotation))
	       ,(replace-regexp-in-string "[.]png\\'" ".jpg" file)))
    (apply #'call-process "convert" nil nil nil file
	   `("-normalize"
	     "-quality" "80"
	     ,@(and rotation (list "-rotate" rotation))
	     ,(replace-regexp-in-string "[.]png\\'" ".jpg" file)))))

(defun magscan-covers-and-count ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" nil "\\`[A-Z0-9]+\\'"))
    (magscan-do-covers mag)))

(defun magscan-do-covers (mag &optional force)
  (let* ((dir (format "~/src/kwakk/magscan/%s/" mag))
	 (got-new nil))
    (dolist (file (directory-files dir t))
      (when (and (file-directory-p file)
		 (not (member (file-name-nondirectory file) '("." "..")))
		 (or force
		     (not (file-exists-p
			   (format "~/src/kwakk/covers/%s/%s-%s.jpg"
				   mag mag (file-name-nondirectory file))))))
	(setq got-new t)
	(magscan-cover file mag)))
    (when (or got-new force)
      (magscan-count-pages dir))))

(defun magscan-count-pages (dir)
  (let ((issues (make-hash-table :test #'equal))
	(suppressed
	 (with-temp-buffer
	   (when (file-exists-p (expand-file-name "suppress-covers.txt" dir))
	     (insert-file-contents (expand-file-name "suppress-covers.txt" dir))
	     (split-string (buffer-string) "\n" t))))
	(page1s
	 (with-temp-buffer
	   (when (file-exists-p (expand-file-name "page1.txt" dir))
	     (insert-file-contents (expand-file-name "page1.txt" dir))
	     (split-string (buffer-string) "\n" t)))))
    (dolist (issue (directory-files dir))
      (let ((idir (expand-file-name issue dir)))
	(when (and (file-directory-p idir)
		   (not (member issue '("." ".."))))
	  (setf (gethash issue issues)
		(let ((elem (make-hash-table :test #'equal)))
		  (setf (gethash "pages" elem)
			(length (directory-files idir nil "page.*jpg")))
		  (setf (gethash "page1" elem)
			(not (not (member issue page1s))))
		  (when (member issue suppressed)
		    (setf (gethash "no-flash-cover" elem) t))
		  (setf (gethash "time" elem)
			(truncate (float-time (file-attribute-modification-time
					       (file-attributes idir)))))
		  elem)))))
    (with-temp-buffer
      (insert (json-encode issues))
      (write-region (point-min) (point-max)
		    (expand-file-name "issues.json" dir)))))

(defun magscan-split-double (file)
  (interactive (list (dired-file-name-at-point)))
  (let ((size (magscan-image-size file)))
    (call-process "convert" nil nil nil
		  "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		  (file-truename file)
		  (concat (file-name-sans-extension file) "-1.jpg"))
    (call-process "convert" nil nil nil
		  "-crop" (format "%sx%s+%s-0"
				  (/ (car size) 2)
				  (cdr size)
				  (/ (car size) 2))
		  ;; "1949x3064+1949-0"
		  (file-truename file)
		  (concat (file-name-sans-extension file) "-2.jpg"))))

(defun magscan-split-doubles (dir)
  (let ((i 0))
    (dolist (file (directory-files dir nil ".jpg$"))
      (setq file (expand-file-name file dir))
      (if (string-match "fc\\|bc" file)
	  (rename-file file (expand-file-name
			     (format "page-%03d.jpg" (cl-incf i))
			     dir))
	(let ((size (magscan-image-size file)))
	  (call-process "convert" nil nil nil
			"-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
			(file-truename file)
			(expand-file-name (format "page-%03d.jpg" (cl-incf i))
					  dir))
	  (call-process "convert" nil nil nil
			"-crop" (format "%sx%s+%s-0"
					(/ (car size) 2)
					(cdr size)
					(/ (car size) 2))
			;; "1949x3064+1949-0"
			(file-truename file)
			(expand-file-name (format "page-%03d.jpg" (cl-incf i))
					  dir)))))))

(defun magscan-detect-and-split-mag (mag)
  (let ((dir (concat "~/src/kwakk/magscan/" mag))
	(ndir (concat "~/src/kwakk/magscan/n" mag)))
    (dolist (issue (directory-files dir nil "[0-9]$"))
      (magscan-detect-and-split-doubles
       (expand-file-name issue dir)
       (expand-file-name issue ndir)))))

(defun magscan-detect-and-split-doubles (dir out-dir)
  (message "%s" dir)
  (unless (file-exists-p out-dir)
    (make-directory out-dir t))
  (let ((i 0))
    (dolist (file (directory-files dir t "page.*.jpg$"))
      (let ((size (magscan-image-size file)))
	(if (< (car size) (cdr size))
	    ;; Vertical
	    (copy-file file (expand-file-name (format "page-%03d.jpg"
						      (cl-incf i))
					      out-dir))
	  ;; Horizontal.
	  (call-process "convert" nil nil nil
			"-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
			(file-truename file)
			(expand-file-name (format "page-%03d.jpg" (cl-incf i))
					  out-dir))
	  (call-process "convert" nil nil nil
			"-crop" (format "%sx%s+%s-0"
					(/ (car size) 2)
					(cdr size)
					(/ (car size) 2))
			;; "1949x3064+1949-0"
			(file-truename file)
			(expand-file-name (format "page-%03d.jpg" (cl-incf i))
					  out-dir)))))))

(defun magscan-renumber-current-directory ()
  (cl-loop for jpg in (directory-files "." nil "page-[0-9][0-9][0-9][.]jpg$")
	   for num from 1
	   do (progn
		(rename-file jpg (format "page-%03d.jpg" num))
		(when (file-exists-p (file-name-with-extension jpg "json"))
		  (rename-file (file-name-with-extension jpg "json")
			       (format "page-%03d.json" num)))
		(when (file-exists-p (file-name-with-extension jpg "txt"))
		  (rename-file (file-name-with-extension jpg "txt")
			       (format "page-%03d.txt" num))))))

(defun magscan-create-url-map (mag)
  (let ((table (make-hash-table :test #'equal))
	(file (format "~/src/kwakk/covers/%s/url-map.txt" mag)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(while (not (eobp))
	  (when (looking-at "\\([^ ]+\\) +\\(.*\\)")
	    (setf (gethash (concat mag "-" (match-string 1)) table)
		  (match-string 2)))
	  (forward-line 1)))
      (magscan-write-url-map file table)
      table)))

(defun magscan-write-url-map (file table)
  (with-temp-buffer
    (insert "var issueMap = "
	    (json-encode table)
	    ";\n")
    (write-region (point-min) (point-max)
		  (file-name-with-extension file ".js"))))

(defun magscan-create-url-maps ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (mag (directory-files "~/src/kwakk/covers/"))
      (when-let ((sub (magscan-create-url-map mag)))
	(cl-loop for key being the hash-keys of sub
		 do (setf (gethash key table) (gethash key sub)))))
    (magscan-write-url-map "~/src/kwakk/covers/ALL/url-map.txt" table)))

(defun magscan-move-first-to-last ()
  (interactive)
  (let ((first (car (directory-files "." t "page.*jpg"))))
    (rename-file first "page-999.jpg")
    (when (file-exists-p (file-name-with-extension first "txt"))
      (rename-file (file-name-with-extension first "txt") "page-999.txt"))
    (when (file-exists-p (file-name-with-extension first "json"))
      (rename-file (file-name-with-extension first "json") "page-999.json")))
  (magscan-renumber-current-directory))

(provide 'magscan)

;;; magscan.el ends here
