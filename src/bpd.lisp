;; bigpond-downloader - a downloader for Telstra Bigpond Music
;; Copyright (C) 2012 "Duncan Bayne" <dhgbayne@gmail.com>
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>


(defpackage :bpd
  (:use :common-lisp)
  (:export #:main))

(in-package :bpd)

(require :sb-posix)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '("drakma" 
		"closure-html" 
		"cxml-stp" 
		"net-telent-date"
		"cl-ppcre"
		"do-urlencode"
		"getopt"))

(defun show-usage () 
  (format t "bigpond-downloader - a downloader for Telstra Bigpond Music.~%")
  (format t "Copyright (C) 2012 \"Duncan Bayne\" <dhgbayne@gmail.com>~%~%")
  (format t "This program is distributed in the hope that it will be useful,~%")
  (format t "but WITHOUT ANY WARRANTY; without even the implied warranty of~%")
  (format t "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the~%")
  (format t "GNU Lesser General Public License for more details.~%~%")
  (format t "Usage: bpd USERNAME PASSWORD~%~%")
  (format t "  USERNAME  Your Telstra Bigpond Music username~%")
  (format t "  PASSWORD  Your Telstra Bigpond Music password~%")
  (format t "Example:~%~%")
  (format t "  bpd bob@example.com b0bsp4ss!~%~%")
  (format t "This will log into the Telstra Bigpond Music site with the username~%")
  (format t "'bob@example.com' and the password 'b0bsp4ss!'.  It will then~%")
  (format t "download all newly purchased music into the current directory.~%"))

(defun login (username password csrf-token)
  "Logs in to www.bigpondmusic.com.au.  Returns a cookie-jar containing authentication details."
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (drakma:http-request "https://bigpondmusic.com/Login/LoginFromNav"
			 :method :post
			 :parameters `(
				       ("usernameNav" . ,username) 
				       ("passwordNav" . ,password)
				       ("__AntiCSRFToken" . ,csrf-token)
				       ("Log In.x" . "33")
				       ("Log In.y" . "24")
				       ("remember" . "false")
				       ("parentUrl" . "http://bigpondmusic.com/")
				       ("CheckUrl" . "https://bigpondmusic.com/Login/CheckSplit"))
			 :cookie-jar cookie-jar)
    cookie-jar))

(defun get-bpd-uri (cookie-jar)
  "Loads the login page and extracts the BPD URI."
  (let* ((body (drakma:http-request "http://bigpondmusic.com/my/downloads" :cookie-jar cookie-jar))
	 (valid-xhtml (chtml:parse body (cxml:make-string-sink)))
	 (xhtml-tree (chtml:parse valid-xhtml (cxml-stp:make-builder))))
    (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
			   (xpath:string-value (xpath:evaluate "//a[contains(@href, 'bpd://bigpondmusic.com/DownloaderData.aspx?MediaIds=waiting')]/@href" xhtml-tree)))))

(defun get-bpd-file (bpd-uri cookie-jar)
  "Retrieves a BPD file, assuming UTF-8 encoding."
  (let ((http-uri (cl-ppcre:regex-replace-all "^bpd" bpd-uri "http")))
    (let ((body (drakma:http-request http-uri :cookie-jar cookie-jar)))
      (let ((utf8-body (flexi-streams:octets-to-string body :external-format :utf-8)))
	utf8-body))))

(defun get-encoded-mp3-uris (bpd-file)
  "Extracts the individual encoded MP3 URIs from the contents of a BPD file."
  (let ((scanner (cl-ppcre:create-scanner "^FILE URL.*" :multi-line-mode t)))
    (cl-ppcre:all-matches-as-strings scanner bpd-file)))

(defun decode-uri (encoded-mp3-uri)
  "Decodes a URI from the format peculiar to BPD files"
  (cl-ppcre:regex-replace "&PATH.*" 
			  (replace-all 
			   (replace-all 
			    (replace-all
			     (replace-all encoded-mp3-uri "FILE URL=" "")
			     "http%3a%2f%2fbigpondmusic.com%2fMarkAsDownloaded.aspx%3f" "http://bigpondmusic.com/MarkAsDownloaded.aspx?")
			    "%26" "&")
			   "%3d" "=")
			  ""))

(defun decode-uris (mp3-uris)
  "Decodes a list of MP3 URIs"
  (mapcar #'decode-uri mp3-uris))

(defun get-csrf-token ()
  "Loads the login page and extracts the anti-CSRF token for subsequent use during login."
  (let* ((body (drakma:http-request "http://bigpondmusic.com/"))
	 (valid-xhtml (chtml:parse body (cxml:make-string-sink)))
	 (xhtml-tree (chtml:parse valid-xhtml (cxml-stp:make-builder))))
    (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
			   (xpath:string-value (xpath:evaluate "//input[@name='__AntiCSRFToken']/@value" xhtml-tree)))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
			 (loop with part-length = (length part)
			       for old-pos = 0 then (+ pos part-length)
			       for pos = (search part string
						 :start2 old-pos
						 :test test)
			       do (write-string string out
						:start old-pos
						:end (or pos (length string)))
			       when pos do (write-string replacement out)
			       while pos)))

(defun download-music (username password)
  "Downloads all recently purchased Telstra Bigpond Music for the specified user."
  (let* ((token (get-csrf-token))
	 (cookie-jar (login username password token))
	 (bpd-uri (get-bpd-uri cookie-jar))
	 (bpd-file (get-bpd-file bpd-uri cookie-jar))
	 (encoded-uris (get-encoded-mp3-uris bpd-file))
	 (decoded-uris (decode-uris encoded-uris)))
    (mapcar #'print decoded-uris)))

(defun main (args)
  "The entry point for the application when compiled with buildapp."
  (if (= (length args) 3)
      (let* ((username (nth 1 args))
	     (password (nth 2 args)))
	(download-music username password))
    (show-usage)))