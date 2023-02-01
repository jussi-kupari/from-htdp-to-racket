#lang racket/base

;; =============================================================================
; Book meta information retriever and parser

(require racket/contract)

(provide
 (contract-out
  ; produces a book-info for the given isbn with the given provider
  ; if provider doesn't have records only the isbn is included
  [get-book-info          (-> isbn? provider? book-info?)]

  ; parses a book record at given path with the given provider
  [parse-book-record-file (-> path-string? provider? book-info?)]))
  
(require (only-in racket/function curry)
         (only-in racket/port port->string)
         (only-in "isbn-utils.rkt" isbn?)
         "provider.rkt"
         "provider-ol.rkt"
         "provider-gb.rkt"
         "provider-lc.rkt")

(module+ test
  (require rackunit)
    
  ; EXAMPLES
  ; - ISBNs
  (define ol-isbn "9780321573513") ;Sedgewick's Algorithms
  (define gb-isbn "9780321635747") ;TAOCP
  (define lc-isbn ol-isbn)
  (define no-lc-item-isbn "9780321635747")
    
  ; - Book record files
  (define ol-record "test-ol-json-example.json")
  (define ol-record/no-item "test-ol-json-no-item-example.json")
  (define gb-record "test-gb-json-example.json")
  (define gb-record/no-item "test-gb-json-no-item-example.json")
  (define lc-record "test-lc-xml-example.xml")
  (define lc-record/no-item "test-lc-xml-no-item-example.xml")

  ; - Book-info's
  (define ol-bi
    (hash 'authors '("Robert Sedgewick")
          'date "2011"
          'title "Algorithms"
          'place "Upper Saddle River, NJ"
          'publisher "Addison-Wesley"))
  (define gb-bi
    (hash 'authors '("Donald E. Knuth")
          'date "1997-07-04"
          'title "The Art of Computer Programming"
          'place ""
          'publisher "Addison-Wesley Professional"))
  (define lc-bi
    (hash 'authors '("Sedgewick, Robert, 1946-" "Wayne, Kevin Daniel, 1971-")
          'date "c2011."
          'title "Algorithms /"
          'place ""
          'publisher "Upper Saddle River, NJ : Addison-Wesley,"))

  ; - Book-info's w/ ISBN
  (define ol-bi/isbn (hash-set ol-bi 'isbn ol-isbn))
  (define gb-bi/isbn (hash-set gb-bi 'isbn gb-isbn))
  (define lc-bi/isbn (hash-set lc-bi 'isbn lc-isbn)))

(module+ test
  (check-equal? (parse-book-record-file ol-record ol) ol-bi)
  (check-equal? (parse-book-record-file gb-record gb) gb-bi)
  (check-equal? (parse-book-record-file lc-record lc) lc-bi)
  (check-equal? (parse-book-record-file ol-record/no-item ol) (hash))
  (check-equal? (parse-book-record-file gb-record/no-item gb) (hash))
  (check-equal? (parse-book-record-file lc-record/no-item lc) (hash)))
    
(define (parse-book-record-file path provider)
  (call-with-input-file path
    (compose (curry bib-parse provider) port->string)))

(module+ test
  ; WARNING: these tests overload remote servers, so they are commented out.
  ; We should use our own "mockup" servers for better testing
  #;
  (check-pred
   (lambda (h)
     (and (book-info? h) (regexp-match? (hash-ref h 'isbn) ol-isbn)))
   (get-book-info ol-isbn ol))
  #;
  (check-pred
   (lambda (h)
     (and (book-info? h) (regexp-match? (hash-ref h 'isbn) gb-isbn)))
   (get-book-info gb-isbn gb))
  #;
  (check-pred
     (lambda (h)
       (and (book-info? h) (regexp-match? (hash-ref h 'isbn) lc-isbn)))
     (get-book-info lc-isbn lc))
  #;
  (check-pred
   (lambda (h)
     (and (book-info? h) (regexp-match? (hash-ref h 'isbn) no-lc-item-isbn)))
   (get-book-info no-lc-item-isbn lc)))

(define (get-book-info isbn provider)
  (define (add-isbn h) (hash-set h 'isbn isbn))
  (define book-record (bib-retrieve provider isbn))
  (define book-info (bib-parse provider book-record))
  (add-isbn book-info))
