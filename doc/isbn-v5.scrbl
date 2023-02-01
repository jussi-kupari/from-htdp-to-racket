#lang scribble/manual

@(require (for-label racket))

@title[#:tag "v5"]{Modules and Contracts}

Let's recap what we have achieved so far. We have translated our initial
ISL+ version into a Racket version with the aid of two powerful
mini-languages (@italic{pattern matching} and @italic{regular expressions})
and the whole collection of @racket[for] constructs along with their
accompanying @italic{sequences}.  The result is as readable and
systematically designed as before, but more concise and hopefully more
efficient.

With the exception of the @hash-lang[] declaration at the top of the code
everything looks familiar. You may even consider the new material
as a mere extension of ISL+.  Just a matter of absorbing new constructs
and applying them where it makes sense. 

What comes next, though, is a step away from the known landscape into real
Racket, such as it is used by the programmers who write the libraries which
we rely upon.

@section{Modules}

Code organization is at the heart of systematic program design. You go from
data definitions (that may need auxiliary data definitions) to
well-organized functions according to the known @italic{HtDP} mantras: one
function per task, top-level design (wished-for function), abstraction,
etc.

This systematic organization is exercised on the micro-level of a delimited
code body. But when code gets larger and more complex, and remarkably when
some parts of your code might be helpful for several other parts, you may
feel the need of a higher-level organization.  In particular, you may wish
to split your code into separate collections, each around a certain general
purpose. For instance, in our ISBN extraction program we can clearly
distinguish core functions, those that find the ISBN(s) in a string, and
everything else, constants, predicates and functions that help main
functions fulfill their job. Furthermore, you could imagine that the latter
utils may be useful for code that doesn't need to find ISBNs (for instance,
a program to only validate already existing candidates).

Such high-level organization is achieved in Racket via @italic{modules}.
Modules are code components that collect related definitions and constructs
to be used by other parts of your code, by other modules. 

In fact, in @italic{HtDP} and teaching languages you are working on and
with modules all the time, just that they remain behind the scenes. For
instance, whenever you need tools to build images you require the
definitions contained in the @racketmodname[2htdp/image] module. Moreover,
all of your @italic{HtDP} projects are modules themselves collecting the
definitions to solve the problem at hand. 

The next natural step is being able not only to use modules but rather to
create your own modules.

Although a module typically resides in its own file, you can also
organize code that already exists in a single file. This is a
convenient technique, modularize first your existing code within the
boundaries of a file, and once you are ready, split the resulting modules
into separate files. 

@subsection{The @racket[module] form}

Modules are primary introduced with the @racket[module] form, that
specifies the name of the module and the initial imports. So the form:

@racketblock[
(module utils racket/base ...)
]

means that whatever goes in place of the dots belongs to a module that uses
@racketmodname[racket/base] definitions (aka. is written in the
@racketmodname[racket/base] language), and whose name is @racket[utils].

If you think that this has something to do with the @hash-lang[]
declaration of previous versions, you are right. Indeed,

@hash-lang[] @racketmodname[racket/base]

is just a shorthand for 

@racketblock[
(module _name racket/base)
]

where @racket[_name] is derived from the filename of the code containing
the @hash-lang[] shorthand.

@subsection{Module sections}

In place of the dots in the @racket[module] form above, you just need to
put all the code you want to be there. For documentation is also
recommended to include a purpose statement about the goal of this code. 

For instance, the module containing utilities to be used in our
ISBN extraction program can be design as follows:

@(begin
#reader scribble/comment-reader
(racketblock
(module utils racket/base
  ; Utils for ISBN extraction

  (require racket/match)
  ...

  (define re-isbn-13 ...)
  (define re-isbn-10 ...)
  ...

  ;; Any -> Boolean
  ;; is v an ISBN-13-String?
  (check-expect (isbn-13-string? ...) ...)
  ...

  (define (isbn-13-string? v) ...)
  ...)
))

In this form, though, the module is useless. For one reason, all
definitions inside a module are private by default. In other words,
they can only be used within the module unless we provide them, make
them available to other modules.

A module exports its definitions via the @racket[provide] form. By
convention the @racket[provide] form constitutes the first section of a
module.  This makes sense because the first thing one will want to know
about a module is what it provides. 

With the provide section the previous example would be written 
as follows:

@(begin
#reader scribble/comment-reader
(racketblock
(module utils racket/base
  ; Utils for ISBN extraction ; <- Module Purpose

  (provide isbn-13-string?    ; <- Provides
           ...)

  (require racket/match)      ; <- Requires
  ...

  (define re-isbn-13 ...)     ; <- Constants
  (define re-isbn-10 ...)
  ...

  ;; Any -> Boolean           ; <- Function Definitions
  ;; is v an ISBN-13-String?
  (check-expect (isbn-13-string? ...) ...)
  ...

  (define (isbn-13-string? v) ...)
  ...)
))

Potential users of a module need to know more than just the name of 
provided definitions, they should also know their purposes and signatures. 
That way the @italic{interface} of a module gets sufficiently specified:

@(begin
#reader scribble/comment-reader
(racketblock
(module utils racket/base
  ; Utils for ISBN extraction ; <- Module Purpose

  ; -- INTERFACE
  (provide                    ;  <- Provides
     ;; Any -> Boolean        
     ;; is v an ISBN-13-String?
     isbn-13-string?
     ...)

  ; -- IMPLEMENTATION
  (require racket/match)      ; <- Requires
  ...

  (define re-isbn-13 ...)     ; <- Constants
  (define re-isbn-10 ...)
  ...

  (check-expect (isbn-13-string? ...) ...) ; <- Function definitions
  ...

  (define (isbn-13-string? v) ...)
  ...)
))

We can now create a module for the @racket[main] functions as before:

@(begin
#reader scribble/comment-reader
(racketblock
(module main racket/base
  ; ISBN extraction from Strings

  ; -- INTERFACE
  (provide
    ;; String ISBN-Format -> [Maybe ISBN]
    ;; extracts the first ISBN ...
    isbn-find
    ...)

  ; -- IMPLEMENTATION
  (require racket/file)
  ...

  (check-expect (isbn-find "" 'isbn-13) #f)
  ...
  
  (define (isbn-find str format) ...)

  ...)
))

@subsection{Requiring our own modules}

There is, though, a crucial missing piece in the @racket[main] module. In
order to use the definitions provided by the @racket[utils] module the
@racket[main] module must require @racket[utils]. Again, this is nothing
new. We have required Racket modules all the time, so we have to require
our own modules when needed. As other requires this essential require
should be placed into the require section:

The new thing is the syntax for requiring our own modules, that may differ
from the syntax for requiring Racket modules, it'll depend on how those
modules are organized themselves.

In the current case the syntax is as follows:

@racketblock[
(require (submod ".." utils))
]

Why this?

Recall what we have said before about the @hash-lang[] shorthand. Here we
have a file, which is itself a module (!), with two (sub)modules inside,
and in one of them we refer to the another one.

@racketblock[
(module name-derived-from-isbn.v5.rkt racket/base
  (module utils racket/base
    ...)
  
  (module main racket/base
    ...
    (require (submod ".." utils))
    ...))
]

The reference uses a relative location (@litchar{".."}), that indicates the
parent module to the current one@margin-note*{This is a shorthand borrowed
from the the Unix convention to represent relative file paths, where
@litchar{.} means the current directory, and @litchar{..}, the parent
directory.}.

Thus, if we are located in the @racket[main] module, the current module
path  would be the @racket[main] module itself, while the parent
(enclosing) module path is the module that corresponds to the file. In
other words, @litchar{".."} in the @racket[submod] form above points to the
@filepath{isbn.v5.rkt} module, while the @racket[_utils] name points to the
@racket[utils] module inside that file. 

On the other hand, since the enclosing module uses the @racket[main] module
(to run tests), we need to require it from within this enclosing module:

@(begin
#reader scribble/comment-reader
(racketblock
(module _name-derived-from-isbn.v5.rkt racket/base
  (module utils racket/base
    ...)

  (module main racket/base
    ...
    (require (submod ".." utils))
    ...)
  
  (require 'main)) 
))

where 

@racketblock[
(require 'main)
] 

says shortly the same as

@racketblock[
(require (submod "." main))
]

@subsection{What to export?}

In @filepath{isbn.v5.rkt} we haven't exported every function. You can, and
normally you should export only what you consider useful for other possible
modules, and keep private what only belongs to implementation details.

Here, only predicates related to the representation and a few core utils
have been exported. The rest of functions are actually helpers of
these core utils, and it doesn't make sense to export them. 

@subsection{Resource-awareness @racket[require]s}

You should already observed that @filepath{isbn.v5.rkt} differs form
@filepath{isbn.v4.rkt} in the form of @racket[require]s.  In the former,
@racket[only-in] constructs appear in many @racket[require] specifications:

@racketblock[
(require (only-in racket/match define/match match))
(require (only-in racket/sequence sequence/c))
...
]

This form allows you to specify exactly what you are going to import from a
module. It is common practice to do so: we import only what we need, and at
the same time we are explicit about our requirements. In our case this also
formalizes what was stated as code comments in prior versions.

@section{Contracts}

The next new and substantial thing introduced in @filepath{isbn.v5.rkt} is
Racket @italic{contracts}.

There is no better way to express what contracts are than the first
paragraphs in the
@link["https://docs.racket-lang.org/guide/contract-boundaries.html"]{Racket
Guide, Contracts, Contracts and Boundaries}:

@nested[#:style 'inset]{
Like a contract between two business partners, a software contract is an
agreement between two parties. The agreement specifies obligations and
guarantees for each “product” (or value) that is handed from one party to
the other.

A contract thus establishes a boundary between the two parties. Whenever a
value crosses this boundary, the contract monitoring system performs
contract checks, making sure the partners abide by the established
contract.

In this spirit, Racket encourages contracts mainly at module boundaries.
Specifically, programmers may attach contracts to provide clauses and thus
impose constraints and promises on the use of exported values.
}

Coming from @italic{HtDP}, you can surely recognize that contracts allows us to
formalize data types and function signatures, which in @italic{HtDP} are only
comments for the programmer that, nonetheless, she should follow. With
contracts we formulate the same obligations (and promises) but with the
advantage of an actual checking mechanism about them. Now if something
violates our contracts the system (not only the teacher) will complain.

The good news is that Racket contracts are as expressive and flexible
as signatures. The not so good news is that that comes at a price of
being a complex subject in itself that may be intimidating the first
time we encounter it.

@subsection{Data types and contracts}

Let's list how our data types, as used in our function signatures,
can be converted into contracts.

@tabular[#:sep @hspace[8]
(list
 (list @bold{Signatures}  @bold{Contracts})
 (list @litchar{#f}       @litchar{#f})
 (list @tt{Boolean}       @racket[boolean?])
 (list @tt{String}        @racket[string?])
 (list @tt{N}             @racket[natural-number/c]))
]

The @litchar{#f} value can be used as a contract that recognizes itself.
Also, Racket core predicates like @racket[boolean?] or @racket[string?] can
be used as contracts.  @racket[natural-number/c] is a contract defined by
the contract system to recognize natural numbers, including 0. It is more
common in contracts than @racket[natural?] because the latter is not
provided by @racketmodname[racket/base].

@tabular[#:sep @hspace[8]
(list
 (list @bold{Signatures}   @bold{Contracts})
 (list @tt{ISBN}           @racket[isbn?])
 (list @tt{ISBN-10-String} @racket[isbn-10-string?])
 (list @tt{ISBN-13-String} @racket[isbn-13-string?])
 (list @tt{ISBN-Format}    @racket[isbn-format?])
 (list @tt{Word}           @racket[word?]))
]

This transformation is really easy: our own predicates double as contracts,
too.

@tabular[#:sep @hspace[8]
(list
 (list @bold{Signatures}   @bold{Contracts})
 (list @tt{[List-of ...]}  @racket[(listof ...)])
 (list @tt{or}             @racket[(or/c ...)])
 (list @tt{[Maybe ...]}    @racket[(or/c ... #f)]))
]

This table lists @italic{contract combinators} corresponding to commonly
used signature ``combinators''. A contract combinator is just a function
that takes contracts and produce other contracts.

@racket[listof] is the contract combinator corresponding to the
@tt{List-of} signature.  For instance @racket[(listof boolean?)]
corresponds to @tt{[List-of Boolean]}.

@racket[or/c] translates itemizations in signatures. For instance,
@tt{String} or @tt{Symbol} becomes @racket[(or/c string? symbol?)].

One particular form of @racket[(or/c ...)], namely, @racket[(or/c ... #f)],
translates our @tt{Maybe} data types. For instance, @racket[(or/c isbn?
#f)] corresponds to @tt{[Maybe ISBN]}.

@tabular[#:sep @hspace[8]
(list
 (list @bold{Signatures}   @bold{Contracts})
 (list @tt{Any}            @racket[any/c])
 (list @tt{Sequence-of}    @racket[sequence/c]))
]

@racket[any/c] is a contract that recognizes any kind of value, it
corresponds to the @tt{Any} data type in signatures, hardly used in @italic{HtDP}.

@racket[sequence/c] (from @racketmodname[racket/sequence]), is a
contract combinator for sequences.

@subsection{Function contracts}

Contracts for functions are specified with the contract combinator
@racket[->]. So,

@racketblock[
(-> ...)
]

is the contract for a function, where dots are filled with the contracts
for the values consumed followed by the contract for the value produced.

For instance:

@racketblock[
(-> string? boolean?)
]

is the contract for a function that takes a @racket[string?] and produces a
@racket[boolean?]. 

By the way, a function whose contract is:

@racketblock[
(-> any/c boolean?)
]

is so common that there exists a contract shorthand:

@racketblock[
predicate/c
]

@subsection{From signatures to contracts}

The constructs presented in the preceding sections are what we need to
know in oder to translate all the signatures in @filepath{isbn.v4.rkt} into
contracts@margin-note*{We specify contracts for all functions, not only
for the exported functions, just for practising. In the final version,
signatures remain instead of contracts when functions are private.}.

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-find])
 (list @bold{Signature:} @tt{String? ISBN-Format -> [Maybe ISBN]})
 (list @bold{Contract:}  @racket[(-> string? isbn-format? (or/c isbn? #f))]))]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-format->predicate?])
 (list @bold{Signature:} @tt{ISBN-Format -> [Any -> Boolean]})
 (list @bold{Contract:}  @racket[(-> isbn-format? predicate/c)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-13-string?])
 (list @bold{Signature:} @tt{Any -> Boolean})
 (list @bold{Contract:}  @racket[predicate/c]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-10-string?])
 (list @bold{Signature:} @tt{Any -> Boolean})
 (list @bold{Contract:}  @racket[predicate/c]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-find/list])
 (list @bold{Signature:} @tt{String -> [List-of ISBN]})
 (list @bold{Contract:}  @racket[(-> string? (listof isbn?))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[join-isbn-words])
 (list @bold{Signature:} @tt{[List-of Word] -> [List-of Word]})
 (list @bold{Contract:}  @racket[(-> (listof word?) (listof word?))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[adjoin])
 (list @bold{Signature:} @tt{Word [List-of Word] -> [List-of Word]})
 (list @bold{Contract:}  @racket[(-> word? (listof word?) (listof word?))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-word?])
 (list @bold{Signature:} @tt{Word -> Boolean})
 (list @bold{Contract:}  @racket[(-> word? boolean?)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn?])
 (list @bold{Signature:} @tt{Any -> Boolean})
 (list @bold{Contract:}  @racket[predicate/c]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-13?])
 (list @bold{Signature:} @tt{Any -> Boolean})
 (list @bold{Contract:}  @racket[predicate/c]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-13-valid?])
 (list @bold{Signature:} @tt{ISBN-13-String Boolean})
 (list @bold{Contract:}  @racket[(-> isbn-13-string? boolean?)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-checksumf])
 (list @bold{Signature:} @tt{[Sequence-of N] [Sequence-of N] N -> Boolean})
 (list @bold{Contract:}  @racketblock[(-> (sequence/c natural-number/c)
                                          (sequence/c natural-number/c)
                                          natural-number/c
                                          boolean?)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-string->numbers])
 (list @bold{Signature:} @tt{ISBN-String -> [List-of N]})
 (list @bold{Contract:}  @racket[(-> isbn-string? (listof natural-number/c))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-10?])
 (list @bold{Signature:} @tt{Any -> Boolean})
 (list @bold{Contract:}  @racket[predicate/c]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[isbn-10-valid?])
 (list @bold{Signature:} @tt{ISBN-10-String -> Boolean})
 (list @bold{Contract:}  @racket[(-> isbn-10-string? boolean?)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[remove-isbn-hyphens])
 (list @bold{Signature:} @tt{String -> String})
 (list @bold{Contract:}  @racket[(-> string? string?)]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[remove-isbn-spaces/list])
 (list @bold{Signature:} @tt{String -> [List-of Word]})
 (list @bold{Contract:}  @racket[(-> string? (listof word?))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[split-lines])
 (list @bold{Signature:} @tt{String -> [List-of String]})
 (list @bold{Contract:}  @racket[(-> string? (listof string?))]))
]

@tabular[#:sep @hspace[2]
(list
 (list @bold{Function:}  @racket[split-words])
 (list @bold{Signature:} @tt{String -> [List-of String]})
 (list @bold{Contract:}  @racket[(-> string? (listof word?))]))
]

As an aside, a close inspection shows that @filepath{isbn.v5.rkt}
introduces two new predicates that are not in @filepath{isbn.v4.rkt},
@racket[isbn-format?] and @racket[isbn-string?]. The reason is that they
are used as contracts.

Once we have transformed function signatures into contracts, we can replace
the signatures of our functions with those contracts.  To do so we need a
bit more of syntax. 

First, to associate definitions with their respective contracts we must use
@racket[contract-out], that goes inside the @racket[provide] form enclosing
all the exported definitions.

Second, to use contracts in the provide section we have to import contract
support (not included in @racketmodname[racket/base]) before the
@racket[provide] form:

@racketblock[
(require racket/contract)
]

With these final additions a module like @racket[utils] would show this
shape: 

@(begin
#reader scribble/comment-reader
(racketblock
(module utils racket/base
  ; Utils for ISBN extraction

  (require racket/contract)

  (provide
    (contract-out 
      ;; is v an ISBN-13-String?
      [isbn-13-string? predicate/c]
      ...))

  (require racket/match)
  ...

  (define re-isbn-13 ...)
  (define re-isbn-10 ...)
  ...

  (check-expect (isbn-13-string? ...) ...)
  ...

  (define (isbn-13-string? v) ...)
  ...)
))

@subsection{Aside: what about data type definitions?}

You may be wondering about what happened to data definitions in prior
versions. Why have they been removed from @filepath{isbn.v5.rkt}.

Well, there are two reasons for that, apart from the fact that those
data type descriptions have been already given in preceding versions.

First, predicates and contracts should be enough documentation most of the
time. Second, even if they aren't, the module documentation should fill the
gaps by describing how the information at work has been represented. The
doc for @filepath{isbn.v*.rkt} sources is precisely this manual.

In Racket source code as it appears in production you won't probably find
the kind of data type specifications we are familiar with in @italic{HtDP} 
projects. @filepath{isbn.v5.rkt} is in this sense like other Racket
libraries: code should be enough by itself, and when not, just read the
doc.

