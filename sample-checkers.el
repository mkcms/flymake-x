(require 'flymake-x)

(setq
 flymake-x-checkers
 '((long-lines
    :class pipe
    :command (lambda ()
               (format "grep -n -E ^.{%s,}" (1+ (or fill-column 80))))
    :error-patterns
    ((:note
      (line-start line ":" (zero-or-more nonl) line-end)
      "Line too long")))
   (trailing-whitespace
    :class pipe
    :command "grep -n -E \"[\t ]+$\""
    :error-patterns
    ((:note
      (line-start line ":" (zero-or-more nonl) line-end)
      "Trailing whitespace")))
   (pylint
    :class temp-file
    :modes (python-mode)
    :command (lambda ()
               (if-let
                   ((pylintrc-dir (locate-dominating-file (or (buffer-file-name)
                                                              default-directory)
                                                          ".pylintrc")))
                   (format "pylint --rcfile=%s/.pylintrc" pylintrc-dir)
                 "pylint --disable=C0103"))
    :error-patterns
    ((:error
      (line-start file ":" line ":" column ": "
                  (message
                   (or "E" "F")
                   (one-or-more digit) ":" (zero-or-more nonl))
                  line-end))
     (:warning
      (line-start file ":" line ":" column ": "
                  (message "W" (one-or-more digit) ":" (zero-or-more nonl))
                  line-end))
     (:note
      (line-start file ":" line ":" column ": "
                  (message
                   (or "C" "R" "I")
                   (one-or-more digit) ":" (zero-or-more nonl))
                  line-end))))
   (shellcheck
    :class temp-file
    :modes (sh-mode)
    :command "shellcheck -f gcc"
    :columns-start-from-1 t
    :error-patterns
    ((:error
      (line-start file ":" line ":" column ": error: " (message (zero-or-more nonl)) line-end))
     (:warning
      (line-start file ":" line ":" column ": warning: " (message (zero-or-more nonl)) line-end))
     (:note
      (line-start file ":" line ":" column ": note: " (message (zero-or-more nonl)) line-end))))
   (isort
    :class pipe
    :stderr t
    :modes (python-mode)
    :command "isort -c -"
    :error-patterns
    ((:warning
      (line-start "ERROR:" (zero-or-more blank) (message (zero-or-more nonl)) line-end))))))

