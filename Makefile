emacs ?= emacs
FILES := flymake-x.el
ELC := $(FILES:.el=.elc)

compile: $(ELC)

%.elc: %.el
	${emacs} -Q --batch -L .                                               \
	    --eval '(setq byte-compile-error-on-warn t)'                       \
	    -f batch-byte-compile $<

lint:
	file=$$(mktemp)                                                        \
	&& ${emacs} -Q --batch flymake-x.el                                    \
	  --eval '(checkdoc-file (buffer-file-name))' 2>&1 | tee $$file        \
	&& test -z "$$(cat $$file)"                                            \
	&& (grep -n -E "^.{80,}" flymake-x.el `# Catch long lines`             \
	    | sed                                                              \
		-r '1d;s/^([0-9]+).*/flymake-x.el:\1: Too long/;q1')

# Run emacs -Q with flymake-x
_baremacs: ${ELC}
	${emacs} -Q -L . \
	         -l flymake-x -l flymake-x-sample-checkers -f flymake-x-setup  \
	         -f flymake-x-sample-checkers-setup

update-copyright-years:
	year=`date +%Y`;                                                       \
	sed -i *.el *.md -r                                                    \
	   -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
