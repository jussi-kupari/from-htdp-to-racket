#lang scribble/manual

@(require (for-label (only-in lang/htdp-intermediate-lambda
                              explode implode member? local) 
                     (only-in teachpack/2htdp/batch-io read-file)
                     test-engine/racket-tests
                     (except-in racket local)))

@title[#:tag "v2"]{Getting into Racket}

@section{The Racket language}

It is time to transition from @italic{HtDP} to real Racket. You may guess how to do
that: @hash-lang[].

The initial decision has to do with the specific language which you are
going to write in: full Racket, the language with all batteries included,
or a basic layer of Racket. Put it in code, @hash-lang[]
@racketmodname[racket] or @hash-lang[] @racketmodname[racket/base].

We choose the latter for two reasons. If we load only the base of Racket we
get what we need with minimal resource demands. The second reason is
pedagogical. With @racketmodname[racket/base] we have to import explicitly
everything that is not in the base, and as a consequence we end up learning
which parts of Racket provide the tools we are going to require each
time.

@section{Requiring what you need}

It might come to a surprise to know that functions as fundamental as
@racket[first], @racket[rest], @racket[cons], @racket[empty?] are not in
@racketmodname[racket/base] but in other module,
@racketmodname[racket/list] that you need to require if you are going to
use them.  Similarly, @racket[match] is not provided by default, it is
defined in @racketmodname[racket/match]. You need to require all extra
modules providing those and other not-in-@racketmodname[racket/base]
functions: 

@(begin
#reader scribble/comment-reader
(racketblock
(require racket/file)    ;file->string
(require racket/match)   ;match
(require racket/list)    ;cons?, empty?, first, range, rest
(require racket/string)  ;string-split
))

Special mention deserves @racket[check-expect]. @racket[check-expect] is
not in Racket.  For the moment we keep testing functions with
@racket[check-expect] and to do that we need a specialized module:

@racketblock[
(require test-engine/racket-tests)
]

To run tests we'll also need to request it with

@racketblock[
(test)
]

at the end of the file.

@section{Replacing teaching functions}

Furthermore, some nice @italic{HtDP} functions are not defined in any Racket module
other than those specifically written with @italic{HtDP} students in mind.
Such is the case of @racket[explode], @racket[implode] and
@racket[member?].  @racket[read-file] (from @racketmodname[2htdp/batch-io])
is another example.  If we want to live entirely in pure Racket we need to
define them on our own, or to find out Racket substitutes that behave like
them.

@racket[explode] can be replaced by @racket[string->list], while,
conversely, @racket[list->string] replaces @racket[implode]. Since
@racket[string->list] produces @tt{[List-of Char]} instead of a
@tt{[List-of 1String]}, and @racket[list->string] takes @tt{Char}s instead
of @tt{1String}s, we have to convert the @tt{Char} to its corresponding
@tt{String}, or to use a @tt{Char} in patterns instead of a @tt{String}.

@filepath{isbn.v2.rkt} shows several examples of these transformations. For
instance:

@racketblock[
(explode "1234567890")
]

becomes

@racketblock[
(map string (string->list "1234567890"))
]

Likewise,

@racketblock[
(match d 
  ["X" 10] 
  [_ (string->number d)])
]

is transformed to

@racketblock[
(match d 
  [#\X 10] 
  [_ (string->number (string d))])
]

As for @racket[read-file] from @racketmodname[2htdp/abstraction] we can use
@racket[file->string] from @racketmodname[racket/file] instead.

And @racket[member?], what about it? The natural replacement is
@racket[member].  Unfortunately, @racket[member] is a @italic{faux-ami}.
It works like @racket[member?] if the given element is not a member of a
list:

@racketblock[
(member 0 '(1 2 3))
] 

evaluates to @litchar{#false}, the same value as 

@racketblock[
(member? 0 '(1 2 3))
] 

However, contrary to what you would expect,

@racketblock[
(member 1 '(1 2 3))
] 

evaluates to @racketresultfont{'(1 2 3)} instead of @litchar{#true}.

In Racket you may stumble upon with these kind of edges, and this one is
related to the same fundamental fact commented in the previous section,
that values other than @litchar{#false} are treated as @litchar{#true} in
boolean contexts.  Here, instead of @litchar{#true}, @racket[member]
produces the tail of the given list from the first element (included) that
is member of that list.

Therefore, replacing @racket[member?] with @racket[member] implies
further checking whether the result is a non-empty list. We can use
@racket[cons?] to do that:

For instance,

@racketblock[
(member? v digits)
]

can be translated as

@racketblock[
(cons? (member v digits))
]

@section{Replacing your own functions}

Not everything is so laborious. One of the huge benefits of writing in
Racket is that you have access to polished and thoroughly tested
fundamental functions that you may need to define on your own when
teaching languages don't provide them.

The definitions of @racket[split-lines] and @racket[split-words] in
@filepath{isbn.v0.rkt} and @filepath{isbn.v1.rkt} rely upon the
@tt{string-split} functionality that we defined ourselves in
@filepath{isbn-lib.rkt}.

This module can be entirely removed now. The @racketmodname[racket/string]
function @racket[string-split] gives us the same for free. As an aside,
note that Racket functions are usually more flexible and powerful than the
teaching languages counterparts.  In this case, we take advantage of the
keyword argument @racket[#:repeat] to craft the functionality we
want.

In the same vein, we can apply Racket functions in place of our own code.
Thus @racket[remove-isbn-hyphens] can be easily implemented with
@racket[string-replace] from @racketmodname[racket/string]. Therefore,

@racketblock[
(define (remove-isbn-hyphens str)
  (implode (filter (lambda (c) (not (string=? c "-"))) (explode str))))
]

gets simplified to

@racketblock[
(define (remove-isbn-hyphens str) (string-replace str "-" "")) 
]

Finally, we do no longer require @racketmodname[2htdp/abstraction].
@racketmodname[racket/base] provides the same @racket[for] forms (and many
more).

@section{Local definitions}

One substantial change you need to carry out when making the transition
from teaching languages to Racket affects local definitions.

In teaching languages we have the construct @racket[local]. From now on
@racket[local] just drops out. But keep in mind that if something goes
wrong without @racket[local] you may need to look into @racket[let] forms.
Actually, teaching languages provide @racket[local] precisely to avoid
getting overwhelmed with all of those @racket[let] constructs.

