This is a faster version of `looking-back`.

It works by reversing the regular expression, then matching in reverse
order against the buffer.

This relies on the `lex` project to provide a regular expression
parser, and a lexer (which this hacks up to work in reverse).

Some regexp features are not supported.  Backreferences do not work
and because `lex` is written in Emacs Lisp, it can't set the match
data; however, this latter problem can be overcome by re-matching the
original regexp at the buffer position returned by
`looking-back-fast`.
