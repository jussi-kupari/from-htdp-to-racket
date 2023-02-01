#lang racket/base

;; =============================================================================
; The 'Google Books' provider
; [API docs: https://developers.google.com/books/docs/v1/reference/volumes]

; ------------------------------------------------------------------------------
;                           !!DISCLAIMER!!
; Google requires an API key or authorization token to use its Books API
; As this code is only for didactic purposes they are not provided.
; If you are going to use this code in production, please fork it and modify
; it as needed to include your own credentials. For further information see
; https://developers.google.com/books/docs/v1/using
; ------------------------------------------------------------------------------

(require racket/contract)

(provide (contract-out [gb provider?]))
  
(require (rename-in "ejsp.rkt"
                    [json-pointer-value/false jspv/false]
                    [json-pointer-value/empty-str jspv/empty-str]
                    [json-pointer-value/index jspv/index])
         "provider.rkt")

(define query
  (string-append
   "https://www.googleapis.com/books/v1/volumes"
   "?q=isbn:$$isbn$$"
   "&maxResults=1"
   "&projection=lite"))

(define parser
  (hash
   'item (lambda (e) (jspv/false '("items" "0" "volumeInfo") e))
   'authors (lambda (e) (jspv/index '("authors") '() e))
   'date (lambda (e) (jspv/empty-str '("publishedDate") e))
   'title (lambda (e) (jspv/empty-str '("title") e))
   'place (lambda (e) "") ;unsupported by GB API
   'publisher (lambda (e) (jspv/empty-str '("publisher") e))))

(define gb (provider query parser 'json))


