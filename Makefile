# bigpond-downloader - a downloader for Telstra Bigpond Music
# Copyright (C) 2012 "Duncan Bayne" <dhgbayne@gmail.com>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

all: clean bpd-tests bpd

clean:
	rm -rf bin

bpd:
	mkdir -p bin
	cd src; buildapp --output ../bin/bpd --load bpd.lisp --entry "bpd:main"; cd ..

bpd-tests:
	cd src; sbcl --script bpd-test-runner.lisp; cd ..
