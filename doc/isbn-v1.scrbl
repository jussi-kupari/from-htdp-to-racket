#lang scribble/manual

@(require (for-label (except-in lang/htdp-intermediate-lambda format)
                     teachpack/2htdp/abstraction))

@title[#:tag "v1"]{Preparing for Racket}

@section{The universe of languages}

You already know that one of the first things you need to do when working
on DrRacket is to choose the language for your project. Racket is in fact a
myriad of multiple lang-worlds, and you have to live in one of them at each
time.

@italic{HtDP} teaches programming with languages created specifically for
beginners, the so-called teaching languages (BSL, BSL+, ISL, ISL+, and
ASL).  Our ISBN extraction program so far is written in ISL+. And we
specified that language through @menuitem["Language" "Choose Language"] on
DrRacket. 

There is another, better, way to specify the language at work without the
need of going through the DrRacket menu, the @hash-lang[] shorthand. It is
better, firstly because it is independent of DrRacket (it can be used on
the editor or IDE you like) and, secondly, because it makes
explicit as part of the code the language in which it is written.
Racket programmers determine the languages of their projects that way, and
we do so with the very first line in @filepath{isbn.v1.rkt}.

@codeblock|{
#lang htdp/isl+
}|

@link["https://htdp.org/2019-02-24/i3-4.html"]{@italic{HtDP}, Intermezzo 3}
introduces two prominent families of Racket constructs to give students the
opportunity to practice them in a friendly manner, @italic{pattern
matching} and @italic{for loops}. These constructs are provided by the
@racketmodname[2htdp/abstraction] module.

Both are intended to produce more readable, typically more concise
code, as well as better performance (in the case of @racket[for] loops).

For those reasons, Racket programmers make heavy use of them, and we do so 
in @filepath{isbn.v1.rkt}.

@subsection{Beginning pattern matching}

The transformation from conditionals to the @racket[match] forms provided
by @racketmodname[2htdp/abstraction] is straightforward and clearly
explained in the @italic{HtDP} Intermezzo referred above.

The easiest case appears in the function @racket[isbn-format->predicate?],
where the @racket[cond] expression:

@racketblock[
(cond
  [(symbol=? 'isbn-13 format) isbn-13-string?]
  [(symbol=? 'isbn-10 format) isbn-10-string?])
]

becomes:

@racketblock[
(match format
  ['isbn-13 isbn-13-string?]
  ['isbn-10 isbn-10-string?])
]

Note how test expressions in @racket[cond] clauses are replaced by patterns.

Similarly, in @racket[isbn-string->numbers] test expressions become
patterns, so that

@racketblock[
(cond
  [(string=? d "X") 10]
  [else (string->number d)])
]

translates into

@racketblock[
(match d
  ["X" 10]
  [_ (string->number d)])
]

The only novelty is the wildchar pattern @litchar{_}, which translates 
@racket[else].

A more advanced pattern can be seen in @racket[isbn-find]:

@racketblock[
(match candidate
  [(? p? c) c]
  [_ #f])
]

Note, though, that in this case, there is no direct correspondence, as
before, between patterns in the @racket[match] clauses and the
@racket[cond] clauses of @racket[isbn-find] in @filepath{isbn.v0.rkt}:

@racketblock[
(cond
  [(empty? isbns) #f]
  [else (first isbns)])
]

That's because the new version of @racket[isbn-find] is the result of
applying two transformations at once. 

The first implicit transformation replaces @racket[filter] with
@racket[for/or], as we will see later. If we wanted to make it explicit we
would have: 

@racketblock[
(define (isbn-find str format)
  (local [(define p? (isbn-format->predicate? format))]
    (for/or ([candidate (isbn-find/list str)])
      (cond
        [(p? candidate) candidate]
        [else #f]))))
]

So the @racket[cond] expression behind the scene is rather:

@racketblock[
(cond
  [(p? candidate) candidate]
  [else #f])
]

which translates as:

@racketblock[
(match candidate
  [(? p? c) c]
  [_ #f])
]

Here, the pattern @racket[(p? c)] matches if @racket[(p? candidate)]. Since
@racket[p?] stands for @racket[(isbn-format->predicate? format)], it
evaluates to @litchar{#true} if @tt{candidate} (each of the elements of the
result of the list produced by applying @racket[isbn-find/list] to the
input string) is of the given format.

@section{Beginning @racket[for] loops}

@racket[for] loops are handy whenever we are going to traverse compound data
and process each of its elements at a time to produce some result.

Higher-order functions are thus the main target for loop transformations.

@subsection{@racket[for/list] and @racket[for*/list]}

@racket[map] iterates over a list to produce another list. The @racket[for] 
form that does the same thing is @racket[for/list].

In the initial version of @racket[isbn-digit->number] we used @racket[map]
to apply to each letter of a given input string a function that converts it
into a number:

@racketblock[
(map isbn-digit->number (explode str))
]

The same is achieved in the next version via @racket[for/list]:

@racketblock[
(for/list ([d (explode str)])
  (match d
    ["X" 10]
    [_ (string->number d)]))
]

As @racket[for/list] is syntactically more flexible than @racket[map], we
can get rid of the extra local function @racket[isbn-digit->number] without
sacrifying readability. In case you prefer to stick to the local function,
which is perfectly sound too, the loop would look like as follows:

@racketblock[
(for/list ([d (explode str)])
  (isbn-digit->number d))
]

@racket[for/list] can also be used instead of @racket[map] to implement
@racket[isbn-find/list].  In this case two iterations are involved. The
first one consumes the lines of the input string and produces a list of
words per line. Over the list produced by appending all the resulting list
of words, another @racket[_map] is applied to remove hyphens:

@racketblock[
(map remove-isbn-hyphens 
     (append-map remove-isbn-spaces/list lines))
]

@racket[append-map] is not in ISL+, take it thus as pseudocode to convey
the idea of a @italic{map} function that produces here a list of lists
followed by the operation of appending all the resulting sub-lists to get a
flat list.

This kind of nested iteration whose final result is a list can be
translated into a @racket[for*/list] loop as follows:

@racketblock[
(for*/list ([line lines]
            [word (remove-isbn-spaces/list line)])
  (remove-isbn-hyphens word))
]

@subsection{@racket[for/and]}

The @racketmodname[2htdp/abstraction] module does also export @racket[for]
forms whose value is not a list. They are useful whenever the result of
traversing a sequence is synthesized into a single value.

Thus, the function @racket[isbn-word?] determines whether a given
@tt{String} is an @tt{ISBN-Word} by traversing the list of its letters and
checking whether all of them are @tt{ISBN-Letter}s. As the final result is
a @tt{Boolean} it can be appropriately implemented with @racket[andmap]:

@racketblock[
(andmap isbn-letter? (explode str))
]

The corresponding @racket[for] loop is @racket[for/and]: 

@racketblock[
(for/and ([s (explode str)]) (isbn-letter? s))
]

@subsection{@racket[for/sum]}

When a function traverses a list to produce a number we can use a
@racket[_fold] form. As you already know, the typical example is a function
that takes a list of numbers and produces its sum: 

@racketblock[
(foldl + 0 numbers)
]

In @filepath{isbn.v0.rkt} the function @racket[isbn-checksumf] defines a
local @tt{sum} that is computed in that way. In particular, the numbers
given as last argument to @racket[foldl] are the one-by-one multiplication
of two series of numbers of equal length obtained via @racket[map]:

@racketblock[
(foldl + 0 (map * multiplicands multipliers))
]

There is a @racket[for] form useful is such cases: @racket[for/sum]. In the
didactic example, the @racket[foldl] expression would be converted to:

@racketblock[
(for/sum ([n numbers]) n)
]

For the @filepath{isbn.v0.rkt} example the transformation could follow
exactly the same structure (using @racket[for/list] instead of
@racket[map]):

@racketblock[
(for/sum ([n (for/list ([x multiplicands] [y multipliers]) (* x y))]) n)
]

But it is much better this alternative:

@racketblock[
(for/sum ([x multiplicands] [y multipliers]) (* x y))
]

which is not only more concise, but also avoids entirely the intermediate
list, taking advantage of how @racket[for/sum] accumulates results along
the way.

@subsection{More on @racket[for/or] and @racket[for/and]}

You may have noted that the implementation of @racket[isbn-find] in
@filepath{isbn.v0.rkt} cannot use @racket[ormap]. In particular, the
following version doesn't work in ISL+:

@racketblock[
(define (isbn-find str format)
  (local [(define p? (isbn-format->predicate? format))]
    (ormap (lambda (s) (if (p? s) s #f)) (isbn-find/list str))))
]

Actually, if you are a well-versed @italic{HtDP} reader you know that such
an use violates the signature of ormap:

@code{;; [List-of X] [X -> Boolean] -> Boolean}

On the other hand, the @racket[for/or] loop (and, @italic{mutatis
mutandis}, @racket[for/and]) is less restrictive. It still produces
@litchar{#false} if all values in the given sequence evaluate to
@litchar{#false}, but the iteration terminates as soon as a
non-@litchar{#false} value is encountered, and this value becomes the
result of @racket[for/or].

Therefore, what follows is a perfectly valid and working use of
@racket[for/or]:

@racketblock[
(define (isbn-find str format)
  (local [(define p? (isbn-format->predicate? format))]
    (for/or ([s (isbn-find/list str)])
      (if (p? s) s #f))))
]

Replacing the @racket[if] expression with a @racket[match] expression leads
to the @filepath{isbn.v1.rkt} version of @racket[isbn-find].

You may be wondering about why @racket[ormap] and @racket[andmap] are not
like those @racket[for] forms in this respect. In fact, they are, but in
Racket, not in teaching languages. A beginner can better understand
@racket[ormap] and @racket[andmap] as they behave in teaching languages.
Nevertheless, @racket[for/or] and @racket[for/and] bring this more 
versatile and powerful constructs over the teaching world.



