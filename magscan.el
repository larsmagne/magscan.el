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

(require 'rmc)
(require 'tcor)

(defvar magscan-magazine "CSN")

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
	     nil ,(list (list :file file) "scan output") nil
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

(defvar magazine--mag-history nil)
(defvar magazine--issue-history nil)
(defvar magazine--width-history nil)
(defvar magazine--height-history nil)

(defun magscan-it (issue)
  (interactive "sIssue: ")
  (magscan-with-size "KONK" issue 204 269))

(defun magscan-with-size (mag issue width height &optional start)
  "Scan a magazine, prompting for width/height."
  (interactive (list (read-string "Magazine: " (car magazine--mag-history)
				  'magazine--mag-history)
		     (read-string "Issue: " 
				  (format
				   "%d" (1+ (string-to-number
					     (car (or magazine--issue-history
						      '("0"))))))
				  'magazine--issue-history)
		     (string-to-number
		      (read-string "Width: " (car magazine--width-history)
				   'magazine--width-history))
		     (string-to-number
		      (read-string "Height: " (car magazine--height-history)
				   'magazine--height-history))
		     (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (setq magscan-magazine mag)
  (magscan issue start nil width height))

(defun magscan-magazine (issue &optional start)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \np")
  (magscan issue start t))

(defun magscan (issue &optional start magazine width height)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \np")
  (let* ((i (if start
		(/ start 2)
	      0))
	 (default-colour t)
	 (buffer (current-buffer))
	 (colour t)
	 file)
    (when (and (file-exists-p (magscan-file issue ""))
	       (= (or start 1) 1)
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
			     (?2 "Colour next")
			     (?w "Black and white next")
			     (?n "Redo previous")
			     (?p "Page number")))
	     while (not (eql (car choice) ?q))
	     when (or (eql (car choice) ?\r)
		      ;; Pedal.
		      (eql (car choice) ?b))
	     do (cl-incf i)
	     when (or (eql (car choice) ?c)
		      (eql (car choice) ?2))
	     do (setq colour t)
	     when (eql (car choice) ?w)
	     do (setq colour nil)
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
	     (unless (member (car choice) '(?c ?w ?2))
	       (magscan-scan file 
			     (if colour
				 "color"
			       "gray")
			     magazine height (and width (* width 2)))
	       (magscan-display file)
	       (setq colour default-colour)))
    ;; Rename the first file now that we know how long the issue was.
    (rename-file (magscan-file issue "pre-fc-bc.png")
		 (magscan-file
		  issue (concat "pre-"
				(format "%03d-%03d.png"
					1 (* i 2)))))
    (pop-to-buffer buffer)))

(defun magscan-scan-cover (width height)
  (interactive (list
		(string-to-number
		 (read-string "Width: " (car magazine--width-history)
			      'magazine--width-history))
		(string-to-number
		 (read-string "Height: " (car magazine--height-history)
			      'magazine--height-history))))
  (let ((file (expand-file-name "page-001.png")))
    (unless (file-exists-p file)
      (error "Not in an issue directory"))
    (magscan-scan file "color" nil width height)
    (when (file-exists-p "page-001.json")
      (delete-file "page-001.json"))
    (magscan-mogrify-jpeg file)
    (magscan-display (file-name-with-extension file "jpg") 0)))

(defun magscan-single-pages (issue width height &optional start)
  "Scan a magazine.
If START, start on that page."
  (interactive "sIssue: \nnWidth: \nnHeight: \np")
  (setq start (or start 1))
  (let ((i start)
	colour
	file)
    (when (and (file-exists-p (magscan-file issue ""))
	       (= (or start 1) 1)
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
			     (if colour
				 "color"
			       "gray")
			     nil
			     width height 180)
	       (magscan-display file 0)
	       (setq colour nil))
	     when (or (eql (car choice) ?\r)
		      ;; Pedal.
		      (eql (car choice) ?b))
	     do (cl-incf i))))

(defun magscan-display (file &optional rotation)
  (clear-image-cache)
  (switch-to-buffer "*scan*")
  (let ((inhibit-read-only t))
    (delete-other-windows)
    (buffer-disable-undo)
    (erase-buffer)
    (insert-image
     (create-image
      file
      nil nil
      :max-height (- (if rotation
			 (window-pixel-height)
		       (window-pixel-width))
		     60)
      :max-width (- (if rotation
			(window-pixel-width)
		      (window-pixel-height))
		    60)
      :rotation (or rotation 90)))
    (goto-char (point-min))
    (special-mode)))

(defun magscan-image-size (file)
  (prog1
      (image-size (create-image file nil nil :scale 1) t)
    (clear-image-cache)))

(defun magscan-split-page (file)
  (let ((pages (cdr (split-string (replace-regexp-in-string
				   ".png$" "" (file-name-nondirectory file))
				  "-")))
	(size (tcor-image-size file)))
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

(defun magscan-split (directory &optional dont-split)
  (unless dont-split
    (dolist (file (directory-files directory t "pre-.*png"))
      (message "Splitting %s" file)
      (magscan-split-page file)))
  (dolist (file (directory-files directory t "page-.*png"))
    (magscan-mogrify-jpeg file)))

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
	(format "%s-%s.webp"
		(car (last path 2))
		(car (last path 1)))
	dir)))))

(defun magscan-split-new (mag)
  ;; First do all new directories.
  (dolist (dir (directory-files (format "~/src/kwakk/magscan/%s/" mag) t))
    (when (and (file-directory-p dir)
	       (not (member (file-name-nondirectory dir) '("." "..")))
	       (null (directory-files dir nil "json")))
      (message "Splitting %s" dir)
      (magscan-split dir))))

(defun magscan-mogrify (mag)
  (dolist (issue (directory-files (format "~/src/kwakk/magscan/%s/" mag) t "[0-9]$"))
    (dolist (file (directory-files issue t "page-.*png"))
      (magscan-mogrify-jpeg file))))

(defun magscan-mogrify-jpeg (file &optional rotation)
  (if (string-match "grayscale" (with-temp-buffer
				  (call-process "file" nil (current-buffer)
						nil
						(expand-file-name file))
				  (buffer-string)))
      (apply #'call-process "convert" nil nil nil file
	     `(;;"-level" "0%,75%"
	       "-level" "0%,85%"
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
    (magscan-do-covers mag))
  (magscan-hidpi-covers))

(defun magscan-do-covers (mag &optional force)
  (let* ((dir (format "~/src/kwakk/magscan/%s/" mag))
	 (sup (expand-file-name "suppress-covers.txt" dir))
	 (denied (and (file-exists-p sup)
		      (with-temp-buffer
			(insert-file-contents sup)
			(split-string (buffer-string) "\n"))))
	 (got-new nil)
	 cover)
    (dolist (file (directory-files dir t))
      (setq cover (format "~/src/kwakk/covers/%s/%s-%s.webp"
			  mag mag (file-name-nondirectory file)))
      (when (and (file-directory-p file)
		 (not (member (file-name-nondirectory file) '("." "..")))
		 (not (member (file-name-nondirectory file) denied))
		 (or force
		     (not (file-exists-p cover))
		     (file-newer-than-file-p file cover)))
	(setq got-new t)
	(message "Doing cover %s" file)
	(magscan-cover file mag)))
    (when (or got-new force)
      (magscan-count-pages dir))))

(defun magscan-hidpi-covers ()
  (dolist (cover (seq-filter (lambda (name)
			       (not (string-search "-2x.webp" name)))
			     (directory-files-recursively "~/src/kwakk/covers/" "[.]webp\\'")))
    (let ((2x (replace-regexp-in-string "[.]webp\\'" "-2x.webp" cover))
	  (bits (split-string (file-name-nondirectory cover) "[-.]")))
      (when (or (not (file-exists-p 2x))
		(file-newer-than-file-p cover 2x))
	(message "Doing %s..." 2x)
	(call-process "convert" nil (get-buffer-create "*errors*") nil
		      "-normalize" "-resize" "300x"
		      (expand-file-name
		       (format "~/src/kwakk/magscan/%s/%s/page-001.jpg"
			       (car bits)
			       (string-join (seq-take (cdr bits) (1- (length (cdr bits))))
					    "-")))
		      (expand-file-name 2x))))))

(defun magscan-create-entry-time-file (dir)
  (with-temp-buffer
    (insert-file-contents "~/src/kwakk/issue-times.txt")
    (dolist (page (directory-files-recursively dir "page-001.jpg$"))
      (goto-char (point-min))
      (let ((id (magscan-issue-id page)))
	(unless (search-forward (concat "\n" id) nil t)
	  (goto-char (point-max))
	  (insert id
		  (format
		   "%s"
		   (truncate (float-time (file-attribute-modification-time
					  (file-attributes
					   (file-name-directory page))))))
		  "\n"))))
    (write-region (point-min) (point-max) "~/src/kwakk/issue-times.txt"
		  nil 'silent)))

(defun magscan-count-pages (dir)
  (let ((issues (make-hash-table :test #'equal))
	(suppressed
	 (with-temp-buffer
	   (when (file-exists-p (expand-file-name "suppress-covers.txt" dir))
	     (insert-file-contents (expand-file-name "suppress-covers.txt" dir))
	     (split-string (buffer-string) "\n" t))))
	(double
	 (with-temp-buffer
	   (when (file-exists-p (expand-file-name "double-issues.txt" dir))
	     (insert-file-contents (expand-file-name "double-issues.txt" dir))
	     (split-string (buffer-string) "\n" t))))
	(page1s
	 (with-temp-buffer
	   (when (file-exists-p (expand-file-name "page1.txt" dir))
	     (insert-file-contents (expand-file-name "page1.txt" dir))
	     (split-string (buffer-string) "\n" t)))))
    (dolist (issue double)
      (setf (gethash issue issues)
	    (let ((elem (make-hash-table :test #'equal)))
	      (setf (gethash "no-flash-cover" elem) t)
	      (setf (gethash "double" elem) t)
	      elem)))
    ;; Record earliest time the issue appeared.
    (magscan-create-entry-time-file dir)
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
			(magscan-issue-time idir))
		  elem)))))
    (with-temp-buffer
      (insert (json-encode issues))
      (write-region (point-min) (point-max)
		    (expand-file-name "issues.json" dir)))))

(defun magscan-issue-id (page)
  (concat (car (last (file-name-split page) 3))
	  "/"
	  (car (last (file-name-split page) 2))
	  " "))

(defun magscan-issue-time (dir)
  (let ((id (magscan-issue-id (expand-file-name "page" dir))))
    (with-temp-buffer
      (insert-file-contents "~/src/kwakk/issue-times.txt")
      (search-forward (concat "\n" id))
      (buffer-substring (point) (pos-eol)))))

(defun magscan-split-double (file)
  (interactive (list (dired-file-name-at-point)))
  (let ((size (tcor-image-size file)))
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
	(let ((size (tcor-image-size file)))
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
      (let ((size (tcor-image-size file)))
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

(defun magscan-renumber-current-directory (&optional start)
  (make-directory "r")
  (cl-loop for jpg in (directory-files "." nil "page-[0-9][0-9][0-9][.]jpg$")
	   for num from (or start 1)
	   do (progn
		(rename-file jpg (format "r/page-%03d.jpg" num))
		(when (file-exists-p (file-name-with-extension jpg "json"))
		  (rename-file (file-name-with-extension jpg "json")
			       (format "r/page-%03d.json" num)))
		(when (file-exists-p (file-name-with-extension jpg "txt"))
		  (rename-file (file-name-with-extension jpg "txt")
			       (format "r/page-%03d.txt" num)))))
  (dolist (file (directory-files "r" nil "page"))
    (rename-file (concat "r/" file) file))
  (delete-directory "r"))

(defun magscan-concatenate-pages (first)
  (interactive (list (dired-file-name-at-point)))
  (let ((second (save-excursion
		  (forward-line 1)
		  (dired-file-name-at-point))))
    (call-process "convert" nil nil nil
		  (expand-file-name first) (expand-file-name second)
		  "+append" "out.jpg")
    (rename-file "out.jpg" first t)
    (delete-file second)))

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
  (when (file-exists-p (expand-file-name "issues.json" ".."))
    (delete-file (expand-file-name "issues.json" "..")))
  (let* ((mag (file-name-nondirectory (directory-file-name (expand-file-name "../"))))
	 (cover (concat (format "~/src/kwakk/covers/%s/%s-%s.webp"
				mag mag
				(file-name-nondirectory (directory-file-name (expand-file-name "./")))))))
    (when (file-exists-p cover)
      (delete-file cover)))
  (magscan-renumber-current-directory))

(defun magscan-pack (files)
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (magscan-pack-magazine (file-name-nondirectory (directory-file-name (file-name-directory (car files))))
			 nil files))

(defun magscan-pack-magazine (mag &optional outside files make-small)
  (dolist (dir (or files
		   (directory-files (format (if outside
						"~/magscan/%s/"
					      "~/src/kwakk/magscan/%s/")
					    mag)
				    t "[0-9]$")))
    (let* ((pages (directory-files dir nil "page-.*jpg"))
	   (issue (file-name-nondirectory dir))
	   (mags (tcor-magazines))
	   (default-directory dir)
	   (cbr (expand-file-name
		 (format "%s %s.cbr"
			 (cdr (assq 'name (cdr (assq (intern mag) mags))))
			 (if (string-match "\\`[0-9]+\\'" issue)
			     (format "#%d" (string-to-number issue))
			   issue))
		 "~/src/kwakk/upload/")))
      (message "Issue: %s" issue)
      (unless (file-exists-p cbr)
	(apply #'call-process "rar" nil nil nil "a" cbr pages)
	(when make-small
	  (let* ((png-pages (directory-files dir nil "page-.*png"))
		 (ratio (/ (file-attribute-size (file-attributes cbr))
			   100000000.0))
		 (width (max 1000 (truncate (/ (car (tcor-image-size (car png-pages))) ratio)))))
	    (while (> (file-attribute-size (file-attributes cbr)) 100000000)
	      (unless (string-match-p "-small.cbr" cbr)
		(setq cbr (replace-regexp-in-string ".cbr\\'" "-small.cbr" cbr)))
	      (message "%s is big; rescaling to width %s" cbr width)
	      (unless (file-exists-p "/tmp/pack")
		(make-directory "/tmp/pack"))
	      (dolist (page png-pages)
		(call-process "convert" nil nil nil
			      "-resize" (format "%dx" width)
			      page (expand-file-name (file-name-with-extension page "jpg")
						     "/tmp/pack")))
	      (message "Repackaging %s for %s" issue width)
	      (let ((default-directory "/tmp/pack/"))
		(when (file-exists-p cbr)
		  (delete-file cbr))
		(apply #'call-process "rar" nil (get-buffer-create "*rar*") nil "a" cbr pages)
		(cl-decf width 200)))))))))

(defun magscan-csn (files)
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (dolist (dir files)
    (magscan-split-csn dir)))

(defun magscan-split-csn (dir)
  (let* ((whole (car (last (directory-files dir t "page.*png"))))
	 (size (tcor-image-size whole)))
    (call-process "convert" nil nil nil
		  "-crop" (format "%sx%s-0-0" (car size) (/ (cdr size) 2))
		  "-rotate" "90"
		  (file-truename whole)
		  (expand-file-name "page-000.png"
				    (file-name-directory whole)))
    (call-process "convert" nil nil nil
		  "-crop" (format "%sx%s+0+%s"
				  (car size) (/ (cdr size) 2)
				  (1- (/ (cdr size) 2)))
		  "-rotate" "90"
		  (file-truename whole)
		  (expand-file-name "page-999.png"
				    (file-name-directory whole)))
    (rename-file whole (expand-file-name "cover.png" dir))
    (rename-file (expand-file-name "page-999.png"
				   (file-name-directory whole))
		 whole)
    (let ((default-directory dir))
      (magscan-renumber-current-png-directory))))

(defun magscan-renumber-current-png-directory (&optional start)
  (make-directory "r")
  (cl-loop for png in (directory-files "." nil "page-[0-9][0-9][0-9][.]png$")
	   for num from (or start 1)
	   do (progn
		(rename-file png (format "r/page-%03d.png" num))
		(when (file-exists-p (file-name-with-extension png "json"))
		  (rename-file (file-name-with-extension png "json")
			       (format "r/page-%03d.json" num)))
		(when (file-exists-p (file-name-with-extension png "txt"))
		  (rename-file (file-name-with-extension png "txt")
			       (format "r/page-%03d.txt" num)))))
  (dolist (file (directory-files "r" nil "page"))
    (rename-file (concat "r/" file) file))
  (delete-directory "r"))

(provide 'magscan)


;;; magscan.el ends here
