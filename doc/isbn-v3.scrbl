#lang scribble/manual

@(require (for-label racket))

@title[#:tag "v3"]{Full pattern matching and @racket[for] loops}

@section{More on pattern matching}

@racketmodname[2htdp/abstraction] introduces only a minimal part of pattern
matching.  @racketmodname[racket/match] is vastly more powerful and rich.
It is actually a language in itself.

As an example, in @filepath{isbn.v3.rkt} @racket[adjoin] is now defined
with pattern matching. The implementation uses @racket[define/match], that
is a shorthand to @racket[define] a function with @racket[match] clauses
only:

@racketblock[
(define/match (adjoin word lowords)
  [(w '()) (list w)]
  [([? isbn-word? w] [cons (? isbn-word? w1) ws])
   (cons (string-append w w1) ws)]
  [(_ _) (cons word lowords)])
]

Without that shorthand, it should be written as follows:

@racketblock[
(define (adjoin word lowords)
  (match* (word lowords)
    [(w '()) (list w)]
    [([? isbn-word? w] [cons (? isbn-word? w1) ws])
     (cons (string-append w w1) ws)]
    [(_ _) (cons word lowords)]))
]

@racket[match*] allows multiple patterns in clauses. The first clause
matches if @racket[lowords] is empty. The second one matches if both
@racket[word] and the first of @racket[lowords] are @tt{ISBN-Word}s. And
the third one, matches everything else. 

The second pattern in the second clause:

@racketblock[
[cons (? isbn-word? w1) ws]
]

uses two patterns at once:

First, it matches any list with a first (@racket[w1]) and a rest (@racket[ws]): 

@racketblock[
(cons w1 ws)
]

and, secondly, one whose first is an @tt{ISBN-Word}:

@racketblock[
(? isbn-word? w1)
]

@section{More on @racket[for]}

You may observe that some higher-order functions involving iteration in
@filepath{isbn.v0.rkt} remain untouched in @filepath{isbn.v2.rkt}. In
particular, @racket[foldr] in @racket[join-isbn-words], and @racket[filter]
in @racket[isbn-find/list]. That's because we need more powerful
@racket[for] forms.

@subsection{@racket[for/fold]}

Let's consider @racket[join-isbn-words]:

@racketblock[
(define (join-isbn-words lowords)
  (foldr adjoin '() lowords))
]

You already know that we could also implement it with @racket[foldl]:

@racketblock[
(define (join-isbn-words lowords)
  (foldl adjoin '() (reverse lowords)))
]

which, in turn, abstracts from a function definition with accumulators 
like this:

@racketblock[
(define (join-isbn-words lowords0)
  (define (join-isbn-words-aux acc lowords)
    (cond
      [(empty? lowords) acc]
      [else
       (join-isbn-words-aux (adjoin (first lowords) acc)
                            (rest lowords))]))
  (join-isbn-words-aux '() (reverse lowords0))) 
]

A @racket[for] loop corresponding to the last kind of functions, so one
that keeps track of accumulated values along the traversal and produces a
final value from the accumulator is @racket[for/fold]. With
@racket[for/fold] @racket[join-isbn-words] gets translated as follows:

@racketblock[
(define (join-isbn-words lowords)  
  (for/fold ([acc '()]) ([word (in-list (reverse lowords))])
    (adjoin word acc)))
]

@subsection{@racket[for] guards}

It is not immediately clear how to convert @racket[filter] into a 
@racket[for] loop.

For instance,

@racketblock[
(filter even? '(1 2 3))
]

doesn't translate to @racket[for/list]:

@(begin
#reader scribble/comment-reader
(racketblock
(for/list ([x '(1 2 3)]) (if (even? x) x ...)) ;how to fill dots?
))


Racket @racket[for] forms are equipped with a feature to get what we are
wishing for with clarity and ease: @italic{guards}, boolean expressions
that are introduced by  keywords like @racket[#:when] or @racket[#:unless]
attached to @racket[for] clauses.

@racketblock[
(for/list ([x '(1 2 3)] #:when (even? x)) x)
]

Furthermore, the common programming pattern consisting of
@racket[filter]ing over a @racket[map]:

@racketblock[
(filter p? (map f _a-collection))
]

can be roughly translated into:

@racketblock[
(for/list ([x _a-collection]) 
          #:when (p? (f x)) 
  (f x))
]

that avoids the intermediate list created by @racket[map].

This sort of transformation is applied to the definition of
@racket[isbn-find/list] in @filepath{isbn.v3.rkt}:

@racketblock[
(define (isbn-find/list str)
  (for*/list (...
              [candidate (...)]
              #:when (isbn? candidate))
    candidate))
]

@section{Sequences}

An entirely new thing that you will encounter over and over in Racket code
is all those @italic{in}-something expressions that appear in
@filepath{isbn.v3.rkt}. They are @italic{sequence constructors}.  Think of
@italic{sequences} as ordered collections of values. There are many
available, and they appear typically within @racket[for] loops. They can
make your code more readable and concise, at least they can and normally do
improve performance.

What sequence constructor to choose depends on the concrete values 
you want to produce and/or of its kind.

Let's tabulate all of the sequences used in @filepath{isbn.v3.rkt}
along with the values they consume.

@tabular[#:sep @hspace[8]
(list
 (list @bold{sequence constructor} @bold{data type})
 (list @racket[in-list]   "arbitrary lists")
 (list @racket[in-range]  "specific ranges of numbers")
 (list @racket[in-cycle]  "cyclic series")
 (list @racket[in-string] "strings")
 (list @racket[in-value]  "any value"))
]

@racket[in-list], @racket[in-range], and @racket[in-string] are
self-explanatory.

With @racket[in-cycle] we can handle infinite collections of numbers.
Infinite sequences are helpful within @racket[for] loops when we are sure
that the loop will terminate anyway.  Thus the function
@racket[isbn-13-valid?] passes two sequences as first arguments to the
abstract function @racket[isbn-checksumf] which, in turn, iterates over
them to compute the checksum for ISBN validation. The @racket[for/sum] loop 

@racketblock[
(for/sum ([x multiplicands] [y multipliers]) (* x y))
]

traverses, in parallel, @racket[multiplicands] (a sequence constructed via
@racket[in-list]) and @racket[multipliers] (an infinite sequence
constructed via @racket[in-cycle]). The loop terminates once
@racket[multiplicands] has been exhausted, so the infinite cyclic sequence
of @racket[multipliers] doesn't pose any problem.

There is another somehow ``weird'' sequence in @filepath{isbn.v3.rkt}.
@racket[in-value] is a special sequence of only one element. You may wonder
why we need something like that. The
@link["https://docs.racket-lang.org/reference/sequences.html?q=in-value#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._in-value%29%29"]{Racket
documentation on the @racket[in-value] sequence} is exact at this respect,
though probably cryptic for casual readers:

@nested[#:style 'inset]{
This form is mostly useful for let-like bindings in forms such as
@racket[for*/list].
}

In order to grasp the usefulness of @racket[in-value], let's consider again
the function @racket[isbn-find/list].

With all of the constructs presented so far we could write this: 

@racketblock[
(define (isbn-find/list str)
  (for*/list ([line (in-list (split-lines str))]
              [word (in-list (join-isbn-words (split-words line)))]
              #:when (isbn? (remove-isbn-hyphens word)))
    (remove-isbn-hyphens word)))
]

Of course, at this level of programming proficiency we shouldn't tolerate
the repeated computation @racket[(remove-isbn-hyphens word)].

The first thing coming to mind is to resort to a local definition like
this:

@racketblock[
(let ([candidate (remove-isbn-hyphens word)]))
]

There is, though, no feasible way with the tools we currently have to
include such a local definition within the loop. The @racket[in-value]
sequence is helpful precisely in that sort of cases. It provides the
behavior we want. The final implementation with @racket[in-value] should be
now evident:

@racketblock[
(define (isbn-find/list str)
  (for*/list ([line (in-list (split-lines str))]
              [word (in-list (join-isbn-words (split-words line)))]
              [candidate (in-value (remove-isbn-hyphens word))]
              #:when (isbn? candidate))
    candidate))
]

@section{A couple of final tweaks}

The only remaining differences between @filepath{isbn.v2.rkt} and
@filepath{isbn.v3.rkt} are just small touches to keep everything in sync
with the substantial transformations explained in prior sections.

The first one is the signature of @racket[isbn-checksumf]. As commented
above the validating functions pass now sequences instead of concrete lists
to this @racket[isbn-checksumf]. So we coin the @tt{Sequence-of} type
to express this fact.

The second tweak occurs in the definitions of @racket[digit?] and
@racket[isbn-letter?]: they use @racket[pair?] instead of @racket[cons?] to
check out whether the result of member is not @litchar{#false}.
@racket[cons?] is provided by @racketmodname[racket/list], but as we do no
longer need any other list functions, we prefer to use the more primitive
@racket[pair?]  in place of @racket[cons?] and eliminate the
@racketmodname[racket/list] require.  This is, of course, a matter of
taste. Sticking with @racket[cons?] would be equally valid.

