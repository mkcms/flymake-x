emacs ?= emacs
FILES := flymake-x.el
ELC := $(FILES:.el=.elc)

compile: $(ELC)

%.elc: %.el
	${emacs} -Q --batch -L .                                               \
	    --eval '(setq byte-compile-error-on-warn t)'                       \
	    -f batch-byte-compile $<

lint:
	lint_dir=$$(mktemp -d)                                                 \
	&& linted=$$lint_dir/flymake-x.el                                      \
	&& sed 's/;;lint://' flymake-x.el > $$linted                           \
	&& file=$$(mktemp)                                                     \
	&& ${emacs} -Q --batch $$linted -l $$linted                            \
		--eval '(checkdoc-file (buffer-file-name))' 2>&1               \
	    | grep '^flymake.x' | grep -v  '__ignore'  | tee $$file            \
	&& test -z "$$(cat $$file)"                                            \
	&& (grep -n -E "^.{80,}" $$linted `# Catch long lines`                 \
	    | sed                                                              \
		-r '1d;s/^([0-9]+).*/flymake-x.el:\1: Too long/;q1')

# Run emacs -Q with flymake-x
_baremacs: ${ELC}
	${emacs} -Q -L . \
	         -l flymake-x -l flymake-x-sample-checkers -f flymake-x-setup

update-copyright-years:
	year=`date +%Y`;                                                       \
	sed -i *.el *.md -r                                                    \
	   -e 's/Copyright \(C\) ([0-9]+)(-[0-9]+)?/Copyright (C) \1-'$$year'/'

clean:
	rm -f *.elc
