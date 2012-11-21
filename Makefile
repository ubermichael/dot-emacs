# $Id: Makefile 9 2010-04-30 18:25:55Z michael $
# $Name:  $
# Author: Michael S. Joyce <michael@negativespace.net>
# Keywords: 

HOME=/Users/michael
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
OPTS=-avhP --exclude .svn

default: install

install:
	cp dotemacs.el	 			$(HOME)/.emacs

	mkdir -p $(HOME)/.emacs-lisp
	rsync $(OPTS)  emacs-lisp/ 			$(HOME)/.emacs-lisp/

	mkdir -p $(HOME)/.emacs-local
	rsync $(OPTS)	 emacs-local/* 		$(HOME)/.emacs-local/

	mkdir -p $(HOME)/.emacs-templates
	rsync $(OPTS)  emacs-templates/* 	$(HOME)/.emacs-templates/

test: clean 
	$(EMACS) -q -l dotemacs.el &

clean:
	rm -f *~
	rm -f emacs-lisp/*~
	rm -f emacs-local/*~
	rm -f emacs-templates/*~

