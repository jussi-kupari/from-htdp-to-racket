#lang scribble/manual

@(require (for-label racket))

@title[#:tag "intro"]{Introduction}

If you have read the book @link["https://htdp.org"]{@italic{How to Design
Programs}} (@italic{@italic{HtDP}}) you already know some parts of Racket, the
language on which BSL, ISL, etc. are built. And you may wish to get more
stuff about Racket itself and find out how to translate your @italic{@italic{HtDP}}
skills into full Racket.

You could read the book @link["https://www.realmofracket.com/"]{Realm of
Racket}, where Racket is introduced as needed in a friendly setting
familiar to @italic{@italic{HtDP}} readers. You could even try
@link["https://docs.racket-lang.org/guide/"]{The Racket Guide}, a gentle
and complete introduction to the language. But you may miss something in
the middle, something not so terse as @italic{The Guide}, written from
(professional) programmers to (professional) programmers, but more
systematic than the @italic{Realm}.

That's the intent of this project. Its purpose is to show by means of an
example how to start the transition from @italic{@italic{HtDP}} languages
(in particular, ISL+) to Racket. At least, what new things to consider when
working directly in Racket.

An example cannot be complete at all, though. Racket is a very rich
language, and sooner than later you'll need to go deeply into @italic{The
Guide} and @link["https://docs.racket-lang.org/reference/"]{The Racket
Reference}. So take them as a first step into Racket for whom already
masters @italic{@italic{HtDP}}.

The sample problem we are going to tackle says as follows:

@nested[#:style 'inset]{ 
Create a program to obtain bibliographic information about pdf books.
Assume that those pdfs are readable (meaning that they are not made of
photos of pages) and, for simplicity, unencrypted.
}

This task can be naturally designed as a composition of the following
sub-tasks:

@itemlist[#:style 'ordered
@item{Read the pdf book as text.}
@item{Search for the ISBN in the resulting text pages.}
@item{Retrieve from a remote provider information about the book with that
ISBN.}
@item{Parse the response to represent it as a Racket data type.}
]

For completeness a preliminary solution to tasks 1, 3, and 4 is given
directly in Racket. In this documentation, we will focus on task 2. A
solution to this sub-task is implemented entirely in ISL+.  From this
implementation a Racket version is derived incrementally, presenting
Racket features and constructs step by step in self-contained new versions.

The sequence of these transformations can be summarized as follows:

@itemlist[
@item{@filepath{isbn.v0.rkt} (aka. v0) is the ISL+ solution.}

@item{v1, still is ISL+, introduces the @litchar{#lang} shorthand and uses
the @racketmodname[2htdp/abstraction] @italic{@italic{HtDP}} module to
replace conditionals and some higher-order functions with @italic{pattern
matching} and @italic{for} loops as presented in that module.}

@item{v2 transitions to Racket. ISL+ functions and constructs are replaced
by Racket functions wherever it is necessary.}

@item{v3 makes full use of @italic{pattern matching} and @italic{for} loops
along with Racket @italic{sequences}.}

@item{v4 introduces @italic{regular expressions}.}

@item{v5, the more substantial transformation, presents @italic{modules}
(yet within a single file), @italic{contracts} and fine-grained
@italic{require}s.}

@item{v6 splits modules in v5 into separate files. It also introduces unit
testing with @racket[rackunit] and @italic{test submodules}.}

@item{v7 talks a bit about performance improvement, introduces
@italic{keyword arguments} and @italic{optional arguments} in functions,
and offers a glimpse into @literal{syntactic abstraction}.}
]

Each part of this documentation corresponds to each of those
transformations.

Once you get familiar with Racket, you won't need to go from teaching
languages to Racket, the ultimate goal is that you can think and write in
Racket from the beginning. The only essential advise will remain the same:
stick always to systematic design. What @italic{@italic{HtDP}} teaches has
nothing to do with particularities of programming languages, it is all
about solving problems in a systematic way. So don't forget the crucial
point when learning new languages, including Racket.

