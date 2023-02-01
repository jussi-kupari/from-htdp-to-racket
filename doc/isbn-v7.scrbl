#lang scribble/manual

@(require (for-label (except-in racket format) 
                     syntax/parse))

@title[#:tag "v7"]{Syntactic abstraction and function definitions}

@section{Improving performance}

In real-word applications, the @racket[isbn-find] function will have to
consume much longer strings than the examples proposed as test cases. 

This means that the current implementation of @racket[isbn-find], which
always calls @racket[isbn-find/list], will take more time than what the
context of its application may need.

On the other hand, most potential applications of @racket[isbn-find] might
require only the first ISBN found, whatever its format.

All of this calls for a simpler and more efficient @racket[isbn-find]. Such
an implementation, that now takes a single argument, could be based on
@racket[for/or] combined with a @racket[#:when] guard, taking
advantage of the fact that the loop will terminate as soon as the guard is
satisfied: 

@racketblock[
(define (isbn-find str)
  (for*/or ([line (in-list (split-lines str))]
            [word (in-list (join-isbn-words (split-words line)))]
            [candidate (in-value (isbn-normalize word))]
            #:when (isbn? candidate))
    candidate))
]

@section{A glimpse into syntactic abstraction}

Do you note the similarity between this new @racket[isbn-find] definition
and the definition of @racket[isbn-find/list]?

@racketblock[
(define (isbn-find/list str)
  (for*/list ([line (in-list (split-lines str))]
              [word (in-list (join-isbn-words (split-words line)))]
              [candidate (in-value (isbn-normalize word))]
              #:when (isbn? candidate))
    candidate))
]

Everything is identical save for the @racket[for] forms, @racket[for*/or]
and @racket[for*/list], respectively.

Of course, this should remind you
@link["https://htdp.org/2019-02-24/part_three.html"]{@italic{HtDP}, Part
III}, and it should encourage you to try this abstraction:

@racketblock[
(define (isbn-find-abs iter str)
  (iter ([line (in-list (split-lines str))]
         [word (in-list (join-isbn-words (split-words line)))]
         [candidate (in-value (isbn-normalize word))]
         #:when (isbn? candidate))
    candidate))

(define (isbn-find str) (isbn-find-abs for*/or str))
(define (isbn-find/list str) (isbn-find-abs for*/list str))
]

Unfortunately, this doesn't work because those @racket[for] forms are not
functions and cannot be passed like you pass functions to other functions.

There is still hope, though, and more than a hope, a whole underlying
universe that sustains everything in Racket. Given the scope of this
project we can just point to it and leave you exploring the new magic on
your own.

The abstraction you are wishing for is this one:

@racketblock[
(define-syntax (isbn-find-abs stx)
  (syntax-parse stx
    [(_ iter str)
     #'(iter ([line (in-list (split-lines str))]
              [word (in-list (join-isbn-words (split-words line)))]
              [candidate (in-value (isbn-normalize word))]
              #:when (isbn? candidate))
             candidate)]))
]

And to use it you also need this require:

@racketblock[
(require (for-syntax syntax/parse racket/base)) 
]

@section{More on Racket functions}

Changing the purpose and implementation of a function doesn't mean
eliminating the former behavior. So we retain the functionality of finding
the first ISBN of a given format. We change the name appropriately to
@racket[find-isbn/format]. And we take the opportunity to make the
function more flexible by means of new Racket constructs.

@subsection{Keyword arguments}

A Racket function can be defined with @italic{keyword arguments}.
This enhances readability when functions take multiple arguments. In the
new version of @racket[isbn-find/format] the second argument is named with
the keyword @racket[#:format]:

@racketblock[
(define (find-isbn/format str #:format format) ...)
]

Callers of this function must include that keyword. For instance:

@racketblock[
(find-isbn/format "123" #:format 'isbn-13)
]

A contract for this function must also include the keyword:

@racketblock[
(-> string? #:format isbn-format? (or/c isbn? #f))
]

The benefit of keywords shows up when the function takes several
arguments, some or all of which are keyworded. For instance,

@racketblock[
(define (person->string #:age a #:gender g #:name n) ...)
]

can be called with any order of arguments:

@racketblock[
(person->string #:gender "female" #:age 22 #:name "Amy")
]

is as valid as

@racketblock[
(person->string #:name "Amy" #:age 22 #:gender "female")
]

@subsection{Optional arguments}

Sometimes we don't want users waste time specifying things that have
a sane default value, but we still want to give them the option of
doing so. For this purpose, function definitions in Racket, as in many
other programming languages, support @italic{optional arguments}.

@racketblock[
(define (find-isbn/format str #:format [format 'isbn-13]) ...)
]

The above definition declares the keyword argument @racket[_format] as
optional with @racket['isbn-13] as default value.

Callers can rely on defaults:

@racketblock[
(find-isbn/format "123")
]

or provide its own value as usual:

@racketblock[
(find-isbn/format "123" #:format 'isbn-10)
]

The corresponding contract is special, too. It uses @racket[->*] as
contract combinator, followed by the group of contracts for non-optional
arguments (surrounded by parentheses), the group of contracts for optional
arguments (surrounded by parentheses, too), and the contract for the
result:

@racketblock[
(->* (string?) (#:format isbn-format?) (or/c isbn? #f))
]


