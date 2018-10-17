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
  ;; Do cover in colour.
  ;; Do the rest of the pages.
  )

(provide 'magscan)

;;; magscan.el ends here
