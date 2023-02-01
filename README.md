# From HtDP to Racket

This repo contains code to extract the ISBN(s) from a given string,
as an example of translation from ISL+ into Racket.

Additionally, this repo provides support for extracting ISBN(s) from
pdf files, as well as for obtaining book information about those files
from remote providers.

## Software requirements

You need to install the following packages:

* Racket distribution: version 7.3 or above [1].

* Extra packages: `pdf-read`, `json-pointer` [2].

[1]. You can still make the code work on a Racket version
below 7.3 if you remove `#lang htdp/isl+` from `isbn.v1.rkt` 
and use instead ISL+ as Language selected from the DrRacket
menu like in `isbn.v0.rkt`.

[2]. `pdf-read` is required by the `pdf-isbn.rkt` module.
`json-pointer` is required by the `book-info.rkt` module.

## Other requirements (Disclaimer)

The `provider-gb.rkt` implements functionality to retrieve and parse records
using the Google Books API. Google requires an API key or authorization token
to use this API. As this code is only for didactic purposes they are not
provided. If you are going to use this code in production, you must fork it
and modify it as needed to include your own credentials. For further
information see https://developers.google.com/books/docs/v1/using

## Core modules source code

Transformation stages and corresponding files are as follows:

1. `isbn.v0.rkt`: ISBN extraction program written in ISL+.

2. `isbn.v1.rkt`: `#lang htdp/isl+` with `2htdp/abstraction` `for` and `match`.

3. `isbn.v2.rkt`: Switch to `lang# racket/base` + Racket batteries.

4. `isbn.v3.rkt`: Native Racket `for` and `match` constructs.

5. `isbn.v4.rkt`: Regular Expressions.

6. `isbn.v5.rkt`: Contracts and Submodules.

7. `isbn.v6.rkt`: The main functions in its own module; `rackunit` and test
   submodules.

   `isbn-utils.rkt`: Contracts and auxiliary functions in its own module;
`rackunit` and test submodules.

8. `isbn.v7.rkt` (aka. `isbn.rkt`): Optional and keyword arguments; 
   syntatic abstraction; performance.

## Auxiliary files for ISL+ implementation

- `isbn-lib.rkt`: Helpers for the ISL+ implementation in `isbn.v0.rkt` and
  `isbn.v1.rkt`.

- `isbn-teachpack.rkt`: Exports `provide`, which is used by `isbn-lib.rkt`.

<!--
## Refactoring annotations

The file `refactoring.log.md` keeps track of changes along versions of
refactored functions.
-->

## Supplementary modules

In order to show a more realistic application of the code above a few more
modules have been included.

### ISBN Extraction from pdf files

- `pdf-isbn.rkt`

### Bibligraphic information retrieval and parsing

- `book-info.rkt`: The main module for this functionality.

- `ejsp.rkt`: A few extensions to the json-pointer module to be used here.

- `provider.rkt`: Data definitions for providers.

- `provider-gb.rkt`: Google Books provider.

- `provider-lc.rkt`: Library of Congress provider.

- `provider-ol.rkt`: Open Library provider.

### Main

- `main-example.rkt`: A very simple client that uses the Racket code in this
  repo.

## Examples for test cases

### ISBN extraction

- `test-isbn-examples`

### ISBN extraction from pdf

- `test-damaged.pdf`: An invalid pdf file.

- `test-isbn-examples.pdf`: ISBN examples in pdf.

- `test-with-image-only.pdf`: A pdf with only an image.

- `test-with-isbn-10-only.pdf`: A pdf with only and ISBN-10.

- `test-without-isbn.pdf`: A pdf without ISBN.

### Bibliographic information 

- `test-gb-json-example.json`: A book JSON record from Google Books.

- `test-lc-xml-example.xml`: A book XML record from the Library of Congress.

- `test-ol-json-example.json`: A book JSON record from the Open Library.

- `test-gb-json-no-item-example.json`: A JSON response from Google Books
   that doesn't contain any book record.

- `test-lc-xml-no-item-example.xml`: An XML response from the Library of
  Congress that doesn't contain any book record.

- `test-ol-json-no-item-example.json`: A JSON response from the Open
  Library that doesn't contain any book record.

## Documentation

`isbn.v*.rkt` transformations are documented in `doc/`. The main
document is `htdp2racket.html`. Scribble sources for all parts are also
included.

## Acknowledgments

Matthias Felleisen derserves special thanks for his support, comments, 
suggestions, and thoughtful code improvements.


