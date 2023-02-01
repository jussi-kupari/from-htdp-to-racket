#lang racket/base

;; =============================================================================
; The 'Open Library' provider
; [API docs: https://openlibrary.org/dev/docs/api/books]  
  
(require racket/contract)

(provide (contract-out [ol provider?]))
  
(require (only-in racket/match match)
         (rename-in "ejsp.rkt"
                    [json-pointer-value/empty-str jspv/empty-str]
                    [json-pointer-value/index jspv/index])
         "provider.rkt")

(module+ test
  (require rackunit))

(define query
  (string-append
   "http://openlibrary.org/api/books?bibkeys=ISBN:$$isbn$$"
   "&format=json"
   "&jscmd=data"))
  
; BExpr -> [Maybe BExpr]
(module+ test
  (check-false (extract-item (hash)))
  (check-equal? (extract-item (hash 'first-key "hi")) "hi"))
  
(define (extract-item bexpr)
  (define (item-key e)
    (list (symbol->string (hash-iterate-key e 0))))
  (match bexpr
    [(? hash-empty?) #f]
    [_ (jspv/empty-str (item-key bexpr) bexpr)]))

(define parser
  (hash
   'item extract-item
   'authors (lambda (e) (jspv/index '("authors") '("name") e))
   'date (lambda (e) (jspv/empty-str '("publish_date") e))
   'title (lambda (e) (jspv/empty-str '("title") e))
   'place (lambda (e) (jspv/empty-str '("publish_places" "0" "name") e))
   'publisher (lambda (e) (jspv/empty-str '("publishers" "0" "name") e))))

(define ol (provider query parser 'json))

