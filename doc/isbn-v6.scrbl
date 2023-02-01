#lang scribble/manual

@(require (for-label racket 
                     rackunit
                     (only-in test-engine/racket-tests check-expect)))

@title[#:tag "v6"]{More on modules and @racketmodname[rackunit]}

@section{From submodules to modules}

@filepath{isbn.v5.rkt} gave high-level organization to our code by means of
modules within the same file, actually submodules.

It was a preliminary step. It is usually better to keep modules in 
separate files.

We create now two files, @filepath{isbn-utils.rkt} and
@filepath{isbn.v6.rkt} to hold each of the submodules in
@filepath{isbn.v5.rkt}.

This transformation implies some small changes. Let's outline them. 

Modules in a single file:

@codeblock|{
;; -- isbn.v5.rkt

#lang racket/base

(module utils racket/base
  ...)

(module main racket/base
  ...
  
  (require (submod ".." utils))

  ...)

(require 'main)
}|

Modules in their own files:

@codeblock|{
;; -- isbn-utils.rkt (prev. utils submodule)

#lang racket/base

...
}|

@codeblock|{
;; -- isbn.v6.rkt (prev. main submodule)

#lang racket/base

...

(require "isbn-utils.rkt")

...
}|

First, initial imports (second argument of @racket[module] forms) become
the language names for @hash-lang[] shorthands.

Second, the module path in the @racket[require] of the @racket[utils]
module takes the name of the file holding that module, as a string.

@section{Rackunit and testing submodules}

@subsection{Unit testing with rackunit}

Like in @italic{HtDP}, we have been testing functions with
@racket[check-expect] from @racketmodname[test-engine/racket-tests]. This
is perfectly fine, but most Racket programmers use @racketmodname[rackunit]
for unit testing, and we are going to do so, too.

We no longer need @racketmodname[test-engine/racket-tests]. Instead, we
require @racketmodname[rackunit]:

@racketblock[
(require rackunit)
]

The following table shows @racketmodname[rackunit] replacements to
@racket[check-expect]s used so far:

@tabular[#:sep @hspace[8]
         #:row-properties '(bottom-border ())
(list
 (list @racketmodname[test-engine/racket-tests] @racketmodname[rackunit])
 (list @racket[(check-expect (f _x) #t)]         @racket[(check-true (f _x))])
 (list @racket[(check-expect (f _x) #f)]         @racket[(check-false (f _x))])
 (list @racket[(check-expect (f _x) _a-value)]    @racket[(check-equal? (f _x) _a-value)]))
]

@subsection{Test submodules}

Unlike @racketmodname[test-engine/racket-tests], @racketmodname[rackunit]
tests must come after the function definitions they are testing. In
practical terms this would mean to place all tests at the end of the file,
or into a separate file.

However, one of the hallmarks of @italic{HtDP} is that test cases appear
before each function definition, instilling the right testing-first mindset
in students. It would indeed come at a loss being forced by a framework to
abandon it, consequently, many Racket programmers do stick to interleaving
tests by means of submodules. Let's see how they do that.

The @racket[module+] form allows us to declare a submodule with a given
name:

@racketblock[
(module+ _name
  ...)
]

which can access to the definitions in the enclosing module. For instance,
in

@codeblock|{
#lang racket/base

(module+ test1
  (require rackunit)
  (check-equal? (foo 5) 1))

(define (foo x) 1)
}|

the submodule named @racket[test1] can access directly to the definition of
@racket{foo} in the enclosing module even though it is not provided, and we
don't need to require the submodule in order to be executed.

You may guess that we can now put tests before functions in this way:

@racketblock[
(module+ test1
  (require rackunit)
  (check-equal? (foo 5) 1))

(define (foo x) 1)

(module+ test2
  (require rackunit
  (check-equal? (bar 5) 2)))

(define (bar x) 2)
]

Not exactly! There is a better way that completely avoids redundancy: with
@racket[module+]  the same submodule can be decomposed into different
pieces that will be later combined for us. So what we actually do is this:

@racketblock[
(module+ test
  (require rackunit))

(module+ test
  (check-equal? (foo 5) 1))

(define (foo x) 1)

(module+ test
  (check-equal? (bar 5) 2))

(define (bar x) 2)
]

The first form declares the module named @racket[test] and requires
@racketmodname[rackunit]. Subsequent forms pointing to the same
@racket[test] module just add on it.

@section{Real-world issues}

The real-world is often messy and wild. This is an a striking example:

@racketblock[
(check-equal? (isbn-find "0—262—06218—6")  "0262062186")
]

We absolutely expect that the above test passes, but it fails because,
shockingly:

@racketblock[
(remove-isbn-hyphens "0—262—06218—6")
]

doesn't remove hyphens!

A closer inspection would show that the Unicode code point for the hyphen
in that string is @litchar{U+2014}, while the code point for the ASCII
hyphen is @litchar{U+002d}. In other words, what appears to be an ASCII
hyphen is actually a Unicode dash.

This example is not so contrived as it seems to be. It comes
actually from converting to text a book published by a well-regarded
academic publisher, and many of its ebooks use the same dash for
representing ISBN hyphens.

The morale is that real-world facts could break our code and thus reveal 
unjustified assumptions. In this case, we wrongly assumed that an ISBN
hyphen could only be an ASCII hyphen. 

Fortunately, we can fix this easily in Racket. Racket regular expressions
allow for matching against Unicode categories, one of which is the
@italic{punctuation-dash category} (@litchar{Pd}), which includes many sort
of dashes.  Among them are a few like wavy dashes that shouldn't be
representations of ISBN hyphen. We could be as specific as we want and
exclude weird dashes. For simplicity we include them all under the
assumption that the probability of ISBN candidates with those weirdos is
practically null.

The pattern to match the punctuation-dash category is @litchar{"\\p{Pd}"},
and a regular expression for this pattern can be build with:

@racketblock[
(pregexp "\\p{Pd}")
]

which is equivalent to the literal regular expression:

@racketblock[
#px"\\p{Pd}"
]

Corresponding constants are:

@racketblock[
(define isbn-dash "\\p{Pd}")
(define re-isbn-dash (pregexp isbn-dash))
]

Removing hyphens can still be implemented with @racket[string-replace],
since this function can take regular expressions as second argument:

@racketblock[
(string-replace word re-isbn-dash "")
]

On the other hand, since ISBN hyphens are allowed letters in ISBN words 
we also need to re-implement @racket[isbn-word?] as follows:  

@racketblock[
(define re-isbn-word (pregexp (string-append "(" "[\\dX]" "|" isbn-dash ")+")))
(regexp-match-exact? re-isbn-word word)
]

where @racket[re-isbn-word], expressed as a literal, would look like this:

@racketblock[
#px"([\\dX]|\\p{Pd})+"
]

meaning: @italic{one or more occurrences of digit, or "X", or Unicode
dash}.

