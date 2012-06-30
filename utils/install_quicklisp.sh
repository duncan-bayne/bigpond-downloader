#!/bin/sh

curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --script install_quicklisp.lisp
rm -f install_quicklisp.lisp
