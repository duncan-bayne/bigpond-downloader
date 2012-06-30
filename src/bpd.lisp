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
		"net-telent-date"))

(defun show-usage () 
  (format t "TODO~%"))

(defun main (args)
  "The entry point for the application when compiled with buildapp."
  (show-usage))

