#lang racket/base

;; =============================================================================
; The 'Library of Congress' provider
; [API docs: http://www.loc.gov/standards/sru/index.html]
  
(require racket/contract)

(provide (contract-out [lc provider?]))
  
(require (only-in racket/match match)
         (only-in xml/path se-path* se-path*/list)
         "provider.rkt")

(module+ test
  (require rackunit))
  
(define query
  (string-append "http://lx2.loc.gov:210/lcdb"
                 "?version=2.0"
                 "&operation=searchRetrieve"
                 "&query=bath.isbn=$$isbn$$"
                 "&recordSchema=dc"
                 "&maximumRecords=1"))

; BExpr -> [Maybe BExpr]
(module+ test
  (check-false (extract-item '(zs:numberOfRecords () "0")))
  (check-equal? (extract-item '(zs:numberOfRecords () "1"))
                '(zs:numberOfRecords () "1")))
  
(define (extract-item bexpr)
  (define (no-records? e)
    (string=? "0" (se-path* '(zs:numberOfRecords) e)))
  (match bexpr
    [(? no-records?) #f]
    [_ bexpr]))

(define parser
  (hash
   'item extract-item
   'authors (lambda (e) (se-path*/list '(creator) e))
   'date (lambda (e) (se-path* '(date) e))
   'title (lambda (e) (se-path* '(title) e))
   'place (lambda (e) "") ;included in publisher
   'publisher (lambda (e) (se-path* '(publisher) e))))

(define lc (provider query parser 'xml))

