EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
OPTS=-av --exclude .git

default: install

install:
	cp dotemacs.el	 			$(HOME)/.emacs

	mkdir -p $(HOME)/.emacs-lisp
	rsync $(OPTS)  emacs-lisp/		$(HOME)/.emacs-lisp/

	mkdir -p $(HOME)/.emacs-templates
	rsync $(OPTS)  emacs-templates/* 	$(HOME)/.emacs-templates/

test: clean 
	$(EMACS) -q -l dotemacs.el &

clean:
	rm -f *~
	rm -f emacs-lisp/*~
	rm -f emacs-templates/*~

