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

(load "bpd.lisp")
(ql:quickload '("lisp-unit"))

(in-package :bpd)

(defmacro run-tests-and-quit (&body tests)
  `(if (lisp-unit:run-tests ,@tests)
       (sb-ext:quit :unix-status 1)
     (sb-ext:quit :unix-status 0)))

(defmacro define-test-with-cookie-jar (test-name cookie-domain cookie-name test)
  `(lisp-unit:define-test
    ,test-name
    (let* ((cookie-jar (make-instance 'drakma:cookie-jar))
	   (cookie ,(if (and cookie-name cookie-domain)
			`(make-instance 'drakma:cookie :name ,cookie-name :domain ,cookie-domain)
		      `nil)))
      (if cookie
	  (setf (drakma:cookie-jar-cookies cookie-jar) (list cookie)))
      ,test)))

(lisp-unit:define-test
  noop
  (lisp-unit:assert-equal 1 1))
