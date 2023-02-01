#lang scribble/manual

@(require(for-label lang/htdp-intermediate-lambda))

@title[#:tag "v0"]{ISBN extraction in ISL+}

Consider the problem of extracting the ISBN, if any, from an input string.

This involves, of course, domain knowledge @margin-note*{See
@link["https://www.isbn-international.org/content/isbn-users-manual"]{ISBN
User Manual} for more information (specifically, Section 5)}. Let's
summarize this knwoledge:

An @tt{ISBN} @margin-note*{Words that might point to representations of the
information at hand are formatted in typewriter font.} can be one of two
types (@tt{Format}s): @tt{ISBN-10} and @tt{ISBN-13}. An
@tt{ISBN-String} is a @tt{String} of thirteen or ten @tt{Digit}s (from
@tt{"0"} to @tt{"9"}), respectively. @tt{ISBN-10-String}s accept @tt{"X"}
as last digit, too. An @tt{ISBN-10} number consists of four groups of
digits. In turn, an @tt{ISBN-13} has one more group, a prefix of three
digits, currently: 978 or 979. Groups may appear separated by hyphens or
spaces. So we could see forms like the following:

@itemlist[
@item{0262062186}
@item{0-262-06218-6}
@item{0 262 06218 6}
@item{9780201896831}
@item{978-0-201-89683-1}
@item{978 0 201 89683 1}
@item{978-0201896831}]

Additionally, in printed books such a number is typically preceded by an
identifier that can appear under different guises:

@itemlist[
@item{ISBN}
@item{ISBN:}
@item{ISBN-10}
@item{ISBN-13}
@item{ISBN-10:}
@item{ISBN-13:}]

followed by a space before the @tt{ISBN-String} itself.

Finally, a string matching that format is an actual @tt{ISBN} if it passes
a validity check, that involves an arithmetic operation@margin-note*{See
the
@link["https://www.isbn-international.org/content/isbn-users-manual"]{ISBN
User Manual} for a description of this operation}.

The first section of @filepath{isbn.v0.rkt} is, as @italic{HtDP} mandates,
a description of the data types we are going to use to represent the
information at hand.

All data types should be self-explained, except @tt{ISBN-Word}.
@tt{ISBN-Word} represents chunks of @tt{ISBN-Letter}s, those allowed by the
ISBN specification excluding space separators. It is called @tt{Word}
precisely due to such an exclusion.

While the other data types derive from the domain knowledge, @tt{ISBN-Word}
and @tt{ISBN-Letter} make sense only with relation to the strategy we are
going to apply to solve the task.

A natural way of tackling the problem would be based on regular
expressions.  ISBNs could be extracted by matching the given string against
a certain regular expression. Regular expression support is not available
in ISL+, though. We need then a different strategy like the following one:

@itemlist[#:style 'ordered
@item{Split the input string into lines.}
@item{Split each line into words (strings without spaces inside) and
remove space separators between @tt{ISBN-Word}'s in each line.}
@item{Remove hyphens from all resulting strings.}
@item{Filter those strings that are actual ISBNs.}
]

Step 2 is necessary because an ISBN can contain spaces as separators of
groups.

Step 3 just normalizes the result of previous steps (by removing hyphens)
to make easier the computation that validates ISBNs in Step 4.

Step 1, in turn, is justified by the ISBN specification, according to which
the ISBN should appear in its own line. On the other hand, working on lines
should be more efficient than processing possibly very long multi-line
strings.

@racket[find-isbn/list] realizes this strategy. In turn,
@racket[find-isbn], the other main function, produces the first ISBN of a
given format found by @racket[find-isbn/list].

The rest of the code is the result of applying the recipe.

The implementation uses several string functions that are not directly
provided by ISL+, and we have defined them from scratch in
@filepath{isbn-lib.rkt}.

