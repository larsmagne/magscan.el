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

(defvar magscan-magazine "AH")

(defun magscan-scan (file mode)
  (call-process "/usr/slocal/bin/scanimage" nil file nil
		(format "--mode=%s" mode)
		"-d" (format "epsonds:libusb:%s:%s" major minor)
		"--resolution" "300dpi"
		"--format=png")
  (start-process "reset" nil
		  "~/src/usbreset/usbreset"
		  (format "/dev/usb/%s/%s" major minor)))

(defun magscan-file (issue spec)
  (format "~/magscan/%s/%s/%s" magscan-magazine issue spec))

(defun magscan (issue)
  "Scan a magazine."
  (interactive "sIssue: ")
  (let ((i 0)
	file)
    (loop for choice in (read-multiple-choice
			 (cond
			  ((= i 0)
			   "Scan front and back cover")
			  ((= i 1)
			   "Scan inside front and page 1")
			  (t
			   (format "Scan page %d and %d"
				   (- (* i 2) 2) (- (1+ (* i 2)) 2))))
			 '((?y "Yes")
			   (?\r "Yup")
			   (?q "Quit")
			   (?n "Redo previous")))
	  while (not (eql (car choice) ?q))
	  when (or (eql (car choice) ?y)
		   (eql (car choice) ?\r))
	  do (incf i)
	  do (setq file
		   (magscan-file
		    issue
		    (concat
		     "pre-"
		     (cond
		      ((= i 1)
		       "fc-bc")
		      ((= i 2)
		       "ifc-001")
		      (t
		       (format "%03d-%03d"
			       (- (* i 2) 4) (- (1+ (* i 2)) 4))))
		     ".png")))
	  (magscan-scan file 
			(if (= i 1)
			    "color"
			  "gray"))
	  (magscan-display file))
    ;; Rename the last file.
    (rename-file file (magscan-file
		       issue (concat "pre-"
				     (format "%03d-ibc" (- (* i 2) 4)))))))

(defun magscan-display (file)
  (pop-to-buffer "*scan*")
  (erase-buffer)
  (insert-image
   (create-image
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (call-process "convert" nil nil nil
		    "-rotate" "-90"
		    "-resize" "700x"
		    file "jpg:-")
      (buffer-string))
    'jpeg t))
  (goto-char (point-min)))

(provide 'magscan)

;;; magscan.el ends here
