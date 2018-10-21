;;; magscan.el --- Scan Magazines -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

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
		  "-x" "260"
		  "-y" "330")
    (start-process "reset" nil
		   "~/src/usbreset/usbreset"
		   (format "/dev/bus/usb/%s/%s"
			   (car device) (cdr device)))))

(defun magscan-file (issue spec)
  (format "~/magscan/%s/%03d/%s"
	  magscan-magazine
	  (string-to-number issue)
	  spec))

(defun magscan (issue)
  "Scan a magazine."
  (interactive "sIssue: ")
  (let ((i 0)
	colour
	file)
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
			  (?n "Redo previous")))
	  while (not (eql (car choice) ?q))
	  when (eql (car choice) ?\r)
	  do (incf i)
	  when (eql (car choice) ?c)
	  do (setq colour t)
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
					1 (* i 2)))))))

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
		    "-resize" "600x"
		    (file-truename file) "jpg:-")
      (buffer-string))
    'jpeg t))
  (goto-char (point-min)))

(defun magscan-split-page (file)
  (let ((pages (cdr (split-string (replace-regexp-in-string
				   ".png$" "" (file-name-nondirectory file))
				  "-"))))
    ;; The front/back covers are scanned in opposite order than all
    ;; other pages.
    (when (equal (car pages) "001")
      (setq pages (nreverse pages)))
    (call-process "convert" nil nil nil
		  "-rotate" "90"
		  "-crop" "1949x3064-0-0"
		  (file-truename file)
		  (expand-file-name (format "page-%s.png" (car pages))
				    (file-name-directory file)))
    (call-process "convert" nil nil nil
		  "-rotate" "90"
		  "-crop" "1949x3064+1949-0"
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
  (call-process "convert" nil nil nil
		"-resize" "150x"
		(expand-file-name "page-001.png" directory)
		(let ((path (split-string (directory-file-name directory) "/")))
		  (format "/var/www/html/covers/%s-%s.jpg"
			  (car (last path 2))
			  (car (last path 1))))))

(defun magscan-ocr-new ()
  ;; First do all new directories.
  (dolist (dir (directory-files "~/magscan/AH/" t))
    (when (and (file-directory-p dir)
	       (null (directory-files dir nil "json")))
      (magscan-ocr dir)))
  ;; Then do any straggling files.
  (dolist (file (directory-files-recursively "~/magscan/AH/" "page.*png"))
    (unless (file-exists-p (replace-regexp-in-string "[.]png$" ".json" file))
      (tcor-ocr file))))      

(defun magscan-mogrify-jpegs ()
  (dolist (file (directory-files-recursively "~/magscan/AH/" "page.*png"))
    (message "%s" file)
    (magscan-mogrify-jpeg file)))

(defun magscan-mogrify-jpeg (file)
  (if (string-match "grayscale" (with-temp-buffer
				  (call-process "file" nil (current-buffer)
						nil
						(expand-file-name file))
				  (buffer-string)))
      (call-process "convert" nil nil nil file
		    "-level" "0%,45%"
		    "-quality" "80"
		    (replace-regexp-in-string "[.]png\\'" ".jpg" file))
    (call-process "convert" nil nil nil file
		  "-quality" "80"
		  (replace-regexp-in-string "[.]png\\'" ".jpg" file))))

(provide 'magscan)

;;; magscan.el ends here
