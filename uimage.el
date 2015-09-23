;;; uimage.el --- Url image minor mode.

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Uimage is a iimange like minor mode that displays images, when image-data 
;; could be retrived from url.
;;
;; ** Display images in *Info* buffer.
;;
;; (add-hook 'info-mode-hook 'uimage-mode)
;;
;; .texinfo:   @file{file://foo.png}
;; .texinfo:   @file{http://xxx.com/foo.png}
;; .texinfo:   @file{https://xxx.com/foo.png}
;; .info:      `file://foo.png'
;; .info:      `http://xxx.com/foo.png'
;; .info:      `https://xxx.com/foo.png'
;;
;; ** Display images in Wiki buffer.
;;
;; (add-hook 'wiki-mode-hook 'uimage-mode)
;;
;; wiki-file:   [[file://foo.png]]
;; wiki-file:   [[http://xxx.com/foo.png]]
;; wiki-file:   [[https://xxx.com/foo.png]]


;;; Code:

(eval-when-compile
  (require 'image-file))

(defgroup uimage nil
  "Support for url images."
  :version "22.1"
  :group 'image)



(defvar uimage-mode-image-filename-regex
  (concat "[-+./_0-9a-zA-Z:]+\\."
	  (regexp-opt (nconc (mapcar #'upcase
				     image-file-name-extensions)
			     image-file-name-extensions)
		      t)))
;; (setq uimage-mode-image-regex-alist
;;   `((,(concat "\\(`\\|\\[\\[\\|<)\\)?"
;; 	      "\\(\\(file:\\|ftp://\\|http://\\|https://\\)" uimage-mode-image-filename-regex "\\)"
;; 	      "\\(\\]\\]\\|>\\|'\\)?") . 2)))

(defcustom uimage-mode-image-regex-alist
  `((,(concat "\\(`\\|\\[\\[\\|<)\\)?"
	      "\\(\\(file://\\|ftp://\\|http://\\|https://\\)" uimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 2))
  "Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp.

Examples of image filename patterns to match:
    file://foo.png
    `file://foo.png'
    \\[\\[file://foo.gif]]
    <file://foo.png>
"
  :type '(alist :key-type regexp :value-type integer)
  :group 'uimage)

(defvar uimage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" 'uimage-recenter)
    map)
  "Keymap used in `uimage-mode'.")

(defun uimage-recenter (&optional arg)
  "Re-draw images and recenter."
  (interactive "P")
  (uimage-mode-buffer nil)
  (uimage-mode-buffer t)
  (recenter arg))

;;;###autoload
(define-obsolete-function-alias 'turn-on-uimage-mode 'uimage-mode "24.1")

(defun turn-off-uimage-mode ()
  "Unconditionally turn off uimage mode."
  (interactive)
  (uimage-mode 0))

(defun uimage-modification-hook (beg end)
  "Remove display property if a display region is modified."
  ;;(debug-print "ii1 begin %d, end %d\n" beg end)
  (let ((inhibit-modification-hooks t)
        (beg (previous-single-property-change end 'display
                                              nil (line-beginning-position)))
        (end (next-single-property-change     beg 'display
                                              nil (line-end-position))))
    (when (and beg end (plist-get (text-properties-at beg) 'display))
      ;;(debug-print "ii2 begin %d, end %d\n" beg end)
      (remove-text-properties beg end
                              '(display nil modification-hooks nil)))))


(defun uimage-display-inline-images-callback (status start end type ori-buffer)
  (unwind-protect
	  (let (file-data)
		(goto-char (point-min))
		(search-forward-regexp "^$")
		(unless (= (point) (point-max))
		  (setq file-data (buffer-substring-no-properties (+ (point) 1) (point-max))))
		(when file-data
		  (with-current-buffer ori-buffer
			(add-text-properties start end
								 `(display ,(create-image file-data type t)
										   modification-hooks
										   (uimage-modification-hook))))))
	(kill-buffer)))

(defun uimage-mode-buffer (arg)
  "Display images if ARG is non-nil, undisplay them otherwise."
  (let (url url-type url-readable-p)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (dolist (pair uimage-mode-image-regex-alist)
          (while (re-search-forward (car pair) nil t)
			(setq url (match-string (cdr pair)))
            
			;; FIXME: we don't mark our images, so we can't reliably
			;; remove them either (we may leave some of ours, and we
			;; may remove other packages's display properties).
			(if arg
				(progn
				  (save-match-data
					(setq url-type (url-type (url-generic-parse-url url)))
					(setq file-readable-p (cond ((equal url-type "ftp")
												 (url-ftp-file-readable-p url))
												((equal url-type "file")
												 (url-file-file-readable-p url))
												((equal url-type "http")
												 (url-http-file-readable-p url))
												((equal url-type "https")
												 (url-https-file-readable-p url)))))
				  (when file-readable-p
					(url-retrieve url #'uimage-display-inline-images-callback `(,(match-beginning 0) ,(match-end 0) nil ,(current-buffer)))))
			  (remove-text-properties (match-beginning 0) (match-end 0)
									  '(display modification-hooks)))))))))

;;;###autoload
(define-minor-mode uimage-mode nil
  :group 'uimage :lighter " uImg" :keymap uimage-mode-map
  (uimage-mode-buffer uimage-mode))

(provide 'uimage)

;;; uimage.el ends here
