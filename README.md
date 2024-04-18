# flymake-x #

Emacs package that provides an easy way to define flymake checkers.

For example, a [pycodestyle][pycodestyle] checker can be defined like this:

```elisp
(pycodestyle
  :class pipe
  :modes (python-mode)
  :command "pycodestyle -"
  :error-patterns
  ((:warning
    (line-start file ":" line ":" column ": " (message (zero-or-more nonl)) line-end))))
```

## Usage ##

Call `flymake-x-setup` once, and modify `flymake-x-checkers`.  This variable is
a list of checker definitions.

See [sample-checkers.el](./sample-checkers.el) for an example.

Each checker should look like this:

```elisp
(NAME                           ; a symbol
  :class flymake-x-TYPE-checker
  :modes MODE-LIST
  :command "COMMAND"
  :stderr t
  :lines-start-from-0 t
  :columns-start-from-1 t
  :error-patterns
  ((:KIND
    RX-STYLE-CONSTRUCT)
   (:KIND
    RX-STYLE-CONSTRUCT)
   ...))
```

The keyword arguments to each checker are:

- ``:class CLASS``

  Required.  This says how the checker process will accept input.  This is one of:

    - `flymake-x-pipe-checker` (or just `pipe`)

    The checker will accept the buffer string via stdin.

    - `flymake-x-temp-file-checker` (or just `temp-file`)

    A temporary file will be created with the buffer contents, and it's path
    will be passed as an argument to the checker process.

- ``:command COMMAND``

  Required.  This is the shell command (string) to run.  It can also be a
  function that accepts no arguments and returns the command string.

- ``:error-patterns PATTERNS``

  Required.  This defines the patterns to search for in the checker process output.

  Each pattern is a list ``(KIND RX-PAT [MSG])``.  KIND can be ``:note``,
  ``:warning`` or ``:error``.

  RX-PAT is an `rx`-style pattern (a sexp) and these additional `rx` constructs
  are available in it:

    - ``line``

    matches ``"[0-9]+"`` and is used to extract line number

    - ``column``

    matches ``"[0-9]+"`` and is used to extract column number

    - ``(message PAT)``

    matches PAT and is used to extract the error message

    - ``file``

    matches ``".*"`` and is used to extract the file name

  MSG can be provided to set the diagnostic message for the pattern.
  Otherwise, it should be matched by the pattern.

  The patterns are searched in order, line-by-line from the beginning of the
  checker's output.

- ``:modes MODES``

  Optional.  If this is present, only run the checker in given modes.  By
  default, a checker will be run in all modes where `flymake-mode` is enabled.

- ``:stderr t``

  Optional.  If this is non-nil, standard error of the checker process will be
  used to search for the error patterns instead of stdout.

- ``:lines-start-from-0 t``

  Optional.  If non-nil, assume lines start from 0.

- ``:columns-start-from-1 t``

  Optional.  If non-nil, assume columns start from 1.


## License ##

```
Copyright (C) 2019-2024 Michał Krzywkowski

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

[pycodestyle]: https://github.com/PyCQA/pycodestyle

<!-- Local Variables: -->
<!-- coding: utf-8 -->
<!-- fill-column: 79 -->
<!-- indent-tabs-mode: nil -->
<!-- End: -->
