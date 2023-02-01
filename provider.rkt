#lang racket/base

;; =============================================================================
; Data definitions for retrieving and parsing book meta information

(require racket/contract)

(provide
 gen:bib
 (contract-out
  ; determines whether the given value is a jsexpr or an xexpr
  [bexpr?          predicate/c]

  ; determines whether the given value is a bibliogrphic key
  [bibkey?         predicate/c]

  ; determines whether the given value is an empty hash or a map of bibkeys to values
  [book-info?      predicate/c]

  ; determines whether the given value is 'json or 'xml
  [response?       predicate/c]

  ; a structure to retrieve bibligraphic information given the uri
  ; template of the request, a collection of functions to extract values
  ; of interest from the response, and the kind of response that the
  ; server produces
  [struct provider ([query string?]
                    [parser (hash/c symbol? (-> bexpr? any/c))]
                    [response response?])]

  ; parses the given book-record with the given provider
  [bib-parse       (-> provider? string? book-info?)]

  ; retrieves a book-record for the given isbn with the given provider
  [bib-retrieve    (-> provider? isbn? string?)]))
    
(require (only-in racket/generic define-generics)
         (only-in racket/match define/match match)
         (only-in racket/port port->string)
         (only-in racket/string string-replace)
         (only-in json jsexpr? string->jsexpr)
         (only-in net/url call/input-url get-pure-port string->url)
         (only-in xml string->xexpr xexpr?)
         (only-in "isbn-utils.rkt" isbn?))

(module+ test
  (require rackunit))

(module+ test
  (check-true (bexpr? #hasheq((turnip . 82))))
  (check-true (bexpr? '(doc () (bold () "hi") " there!")))
  (check-false (bexpr? +inf.0)))

(define (bexpr? v) (or (jsexpr? v) (xexpr? v)))

(module+ test
  (check-true (bibkey? (list-ref bibkeys (random (length bibkeys)))))
  (check-false (bibkey? 'nothing)))

(define bibkeys '(isbn authors date title place publisher))
(define (bibkey? v) (pair? (member v bibkeys)))

(module+ test
  (check-true (book-info? (hash)))
  (check-true (book-info? (hash 'authors '("You") 'title "This")))
  (check-false (book-info? (hash 'nothing 1))))

(define/match (book-info? x)
  [([? hash-empty?]) #t]
  [([hash-table ([? bibkey?] any?) ...]) #t]
  [(_) #f])

(module+ test
  (check-true (response? 'json))
  (check-true (response? 'xml))
  (check-false (response? "what?")))

(define (response? v) (or (equal? v 'json) (equal? v 'xml)))
  
(define-generics bib
  (bib-retrieve bib isbn)
  (bib-parse bib book-record))

(struct provider [query parser response]
  #:methods gen:bib
  [(define (bib-retrieve b isbn)
     (define uri (provider-query b))
     (define isbn-query (build-query uri isbn))
     (call/input-url isbn-query get-pure-port port->string))
   ; String ISBN -> URL
   (define (build-query uri isbn)
     (string->url (string-replace uri "$$isbn$$" isbn)))
     
   (define (bib-parse b book-record)
     (define response (provider-response b))
     (define parser (provider-parser b))
     (define book-expression (parse-book-record book-record response))
     (parse-book-expression parser book-expression))
   ; String Response -> BExpr
   (define (parse-book-record book-record response)
     (match response
       ['json (string->jsexpr book-record)]
       ['xml (string->xexpr book-record)]))
   ; [Hash-of Symbol (BExpr -> Any)] BExpr -> BookInfo
   (define (parse-book-expression parser book-expression)
     (define get-item (hash-ref parser 'item))
     (define item (get-item book-expression))
     (for/hash ([(k extract) (in-hash parser)]
                #:when (and item (bibkey? k)))
       (values k (extract item))))])
