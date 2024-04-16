emacs ?= emacs
FILES := flymake-x.el
ELC := $(FILES:.el=.elc)

compile: $(ELC)

%.elc: %.el
	${emacs} -Q --batch -L . -f batch-byte-compile $<

# Run emacs -Q with flymake-x
_baremacs: ${ELC}
	${emacs} -Q -L . -l flymake-x -l sample-checkers -f flymake-x-setup

update-copyright-years:
	year=`date +%Y`;                                                      \
	sed -i *.el *.md -r                                                   \
	   -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
