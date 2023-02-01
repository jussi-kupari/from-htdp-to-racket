#lang scribble/manual

@(require (for-label racket))

@title[#:tag "v4"]{Regular expressions}

Every general-purpose programming language supports regular expressions.
Racket supports regular expressions too and the mini-language of regular
expressions (@italic{regexes}) is something you need to learn to be
proficient in Racket or in any language. If you know @italic{regexes} from
other languages, you can easily apply what you already know to Racket. If
not, learning @italic{regexes} from Racket would help you use them in other
languages with minor adjustments, since all elaborate upon the same basic
building blocks.

A @italic{regular expression} (short: @italic{regex}) is a kind of string
describing a pattern which tries to match against another string.

In fact, the problem of extracting an ISBN from a given string can be
entirely solved with @italic{regexes} only. For instance, the following
expression extracts @tt{ISBN-13}s (with its ISBN identifier included) from a
given string, assuming that it is normalized regarding its
spaces:

@racketblock[
(regexp-match*
  #px"\\d{13}|(?:ISBN(?:-13)?:? )?97[89][ -]\\d{10}|(?:ISBN(?:-13)?:? )?(?=97[89][ -]\\d{1,5}[ -])97[89][ -]\\d{1,5}[ -]\\d{1,7}[ -]\\d{1,6}[ -]\\d"
  str)
]

For pedagogical reasons@margin-note*{The monstrous one-liner above is against
any style rules. This is intentional. Just to show how cryptic and
overwhelming @italic{regexes} could become.} it seems to be better to go
through this at a slower pace and rely only on more basic @italic{regexes}.
@margin-note{The complete description of the syntax of @italic{regexes} in
Racket can be found in
@link["https://docs.racket-lang.org/reference/regexp.html"]{The Racket
Reference, Regular Expressions}.}

@section{Data type definitions with @italic{regexes}}

With @italic{regexes} in our toolbox we can simplify and/or specify with
greater precision data type descriptions and definitions.

An @tt{ISBN-13-String} is more precisely defined by its leading digits, 978
or 979:

@verbatim{
;; An ISBN-13-String is a String consisting of 13 digits, that begins
;; with "978" or "979"
}

The corresponding regular expression is as follows: 

@racketblock[
#px"97[89]\\d{10}"
]

This pattern matches against any string beginning with  @litchar{"978"} or 
@litchar{"979"} followed by 10 digits.

@litchar{#px} starts a literal regular expression following a syntax
similar to the Perl regular expression syntax@margin-note*{A quick intro
to Perl syntax for @italic{regexes} is available at the
@link["https://perldoc.perl.org/perlrequick.html"]{perlrequick doc}.}.

An @tt{ISBN-10-String} can also be described by a simple pattern:

@racketblock[
#px"\\d{9}[X\\d]"
]

which matches any string consisting of 9 digits followed by @litchar{"X"}
or a digit.

In these definitions the pattern @litchar{"\\d"} stands for digit. So the
@tt{Digit} data type and the @racket[digit?] predicate in
@filepath{isbn.v3.rkt} are no longer needed.

An @tt{ISBN-Word}, in turn, can be expressed as:

@racketblock[
#px"[\\dX-]+"
]

so a string consisting of one or more (the @litchar{+} means that)
occurrences of a digit, @litchar{"X"}, or hyphen.

As before, we no longer need the @tt{ISBN-Letter} data nor its
corresponding predicate. 

With @italic{regexes} we can also express more formally what a @tt{Word}
is, a string that matches:

@racketblock[
#rx"[^ ]+"
]

so a string with one or more letters excluding space (@litchar{[^ ]} 
matches any character except space).

By the way, note that the literal @italic{regex} is now introduced by
@litchar{#rx} @margin-note*{@tt{egrep} is a widely known Unix command for
searching for patterns in a string. For more information about its syntax,
see the
@link["https://www.gnu.org/software/grep/manual/html_node/Regular-Expressions.html#Regular-Expressions"]{GNU
@tt{grep} info page, Regular Expressions}.}, which begins a regular
expression that follows an @tt{egrep}-like syntax.

Once defined we can use @tt{Word} as data type in the signatures of
several functions.

@section{@italic{regexes} in predicates}

Equipped with @italic{regexes} our remaining data type predicates get
nicely simplified.  In particular, @racket[regex-match-exact?] allows us to
check whether a value matches exactly the regular expressions at work.

For example, consider how the long definition of @racket[isbn-10-string?]
in @filepath{isbn.v3.rkt}:

@racketblock[
(define (isbn-10-string? v)
  (and (string? v) (= (string-length v) 10) (digits+optional-x? v)))

(define (digits+optional-x? str)
  (define init (substring str 0 9))
  (define last (substring str 9))
  (and (all-digits? init) (or (digit? last) (string=? last "X"))))

(define (all-digits? init)
  (andmap digit? (map string (string->list init))))

(define (digit? v) (pair? (member v digits)))
]

is transformed into:

@racketblock[
(define (isbn-10-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-10 v)))
]

Likewise, @racket[isbn-13-string?]:

@racketblock[
(define (isbn-13-string? v)
  (and (string? v) (= (string-length v) 13) (all-digits? v)))

(define (all-digits? init)
  (andmap digit? (map string (string->list init))))

(define (digit? v) (pair? (member v digits)))
]

becomes a one-liner:

@racketblock[
(define (isbn-13-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-13 v)))
]

Finally, @racket[isbn-word?] which in @filepath{isbn.v3.rkt} was defined as follows:

@racketblock[
(define (isbn-word? str)
  (for/and ([c (in-string str)]) (isbn-letter? (string c))))

(define (isbn-letter? str) (pair? (member str isbn-letters)))
]

is translated into: 

@racketblock[
(define (isbn-word? word)
  (regexp-match-exact? re-isbn-word word))
]

