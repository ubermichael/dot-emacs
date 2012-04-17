Readme File for nxhtml-mode
---------------------------

nxhtml-mode is a major mode for editing XHTML files in Emacs.  It is
based upon nxml-mode and inherits it validation and completion
facilities.  For more information please see
http://www.emacswiki.org/.

nxhtml-mode is distributed in one zip file.  To install it unpack the
zip file to an empty directory.  In .emacs first set up for using
nxml-mode and then nxhtml-mode, something like this:

    (load "path-to-nxml-directory/nxml-mode-20041004/rng-auto.el")
    (load "path-to-nxhtml-directory/nxhtml-autoload")

Files ending in .HTML or .HTM will now open in major mode nxhtml-mode.
