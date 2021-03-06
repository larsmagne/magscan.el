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

(defun magscan-scan (file mode)
  (let ((device (magscan-find-device)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (call-process "/usr/slocal/bin/scanimage" nil (list :file file) nil
		  (format "--mode=%s" mode)
		  "-d" (format "epsonds:libusb:%s:%s" (car device)
			       (cdr device))
		  "--resolution" "300dpi"
		  "--format=png"
		  ;; US comics size
		  "-x" "260" "-y" "330"
		  ;; Magazine size
		  ;;"-x" "277" "-y" "416"
		  )
    (start-process "reset" nil
		   "~/src/usbreset/usbreset"
		   (format "/dev/bus/usb/%s/%s"
			   (car device) (cdr device)))))

(defun magscan-file (issue spec)
  (format "~/magscan/%s/%s/%s"
	  magscan-magazine
	  (if (string-match "\\`[0-9]+\\'" issue)
	      (format "%03d" (string-to-number issue))
	    issue)
	  spec))

(defun magscan (issue &optional start)
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
    (loop for choice = (read-multiple-choice
			(cond
			 ((= i 0)
			  "Scan front and back cover")
			 ((= i 1)
			  "Scan inside front and page 3")
			 (t
			  (format "Scan page %d and %d"
				  (- (* i 2) 0) (- (1+ (* i 2)) 0))))
			'((?\r "Yes")
			  (?q "Quit")
			  (?c "Colour next")
			  (?n "Redo previous")
			  (?p "Page number")))
	  while (not (eql (car choice) ?q))
	  when (eql (car choice) ?\r)
	  do (incf i)
	  when (eql (car choice) ?c)
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
			    "gray"))
	    (magscan-display file)
	    (setq colour nil)))
    ;; Rename the first file now that we know how long the issue was.
    (rename-file (magscan-file issue "pre-fc-bc.png")
		 (magscan-file
		  issue (concat "pre-"
				(format "%03d-%03d.png"
					1 (* i 2)))))
    (pop-to-buffer buffer)))

(defun magscan-display (file)
  (clear-image-cache)
  (pop-to-buffer "*scan*")
  (erase-buffer)
  (insert-image
   (create-image
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (call-process "convert" nil (current-buffer) nil
		    "-rotate" "90"
		    "-resize" (format "%sx" (- (frame-pixel-width) 30))
		    (file-truename file) "jpg:-")
      (buffer-string))
    'jpeg t))
  (goto-char (point-min)))

(defun magscan-image-size (file)
  (prog1
      (image-size (create-image file) t)
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
  (let ((dir (format "/var/www/html/covers/%s" mag)))
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
  (dolist (dir (directory-files "~/magscan/AH/" t))
    (when (and (file-directory-p dir)
	       (not (member (file-name-nondirectory dir) '("." "..")))
	       (null (directory-files dir nil "json")))
      (magscan-ocr dir)))
  ;; Then do any straggling files.
  (dolist (file (directory-files-recursively "~/magscan/AH/" "page.*png"))
    (unless (file-exists-p (replace-regexp-in-string "[.]png$" ".json" file))
      (tcor-ocr file)))
  (magscan-count-pages "~/magscan/AH/"))

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

(defun magscan-redo-covers-jpegs ()
  (dolist (mag '("TCJ" "AH"))
    (dolist (file (directory-files (format "~/magscan/%s/" mag) t))
      (when (and (file-directory-p file)
		 (not (member (file-name-nondirectory file) '("." ".."))))
	(magscan-cover file mag)))
    (magscan-count-pages (format "~/magscan/%s/" mag))))

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

(provide 'magscan)

;;; magscan.el ends here
