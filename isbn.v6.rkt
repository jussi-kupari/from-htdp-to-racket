;; -----------------------------------------------------------------------------
;; isbn.v6.rkt 
;; -----------------------------------------------------------------------------

#lang racket/base

; ==============================================================================
; Main Module

; Isbn Extraction Functions

(require racket/contract)

(provide
 (contract-out
  ; extracts the first ISBN (of given format) from the given String, if any
  [isbn-find      (-> string? isbn-format? (or/c isbn? #f))]

  ; extracts all ISBNs from the given String
  [isbn-find/list (-> string? (listof isbn?))]))

(require (only-in racket/match match)
         "isbn-utils.rkt")

(module+ test
  (require rackunit
           (only-in racket/file file->string)))

(module+ test
  (check-false (isbn-find "" 'isbn-13) #f)
  (check-false (isbn-find "" 'isbn-10) #f)
  (check-false (isbn-find "0262062186" 'isbn-13) #f)
  (check-false (isbn-find "9781593274917" 'isbn-10) #f)
  (check-equal? (isbn-find "978—0201896831" 'isbn-13)
                "9780201896831")
  (check-equal? (isbn-find (file->string "test-isbn-examples") 'isbn-13)
                "9781593274917")
  (check-equal? (isbn-find (file->string "test-isbn-examples") 'isbn-10)
                "0262062186"))

(define (isbn-find str format)
  (define p? (isbn-format->predicate? format))
  (for/or ([candidate (in-list (isbn-find/list str))])
    (match candidate
      [(? p? c) c]
      [_ #f])))

(module+ test
  (check-equal? (isbn-find/list "") '())
  (check-equal? (isbn-find/list "none") '())
  (check-equal? (isbn-find/list "978—0201896831") '("9780201896831"))
  (check-equal?
   (isbn-find/list (file->string "test-isbn-examples"))
   (list
    ;isbn normalized
    "0262062186" "026256114X" "1593274912"
    "9781593274917" "0201896834" "9780201896831"
    ;isbn w/ several id's and sep's
    "0262062186" "026256114X" "0262062186" "0201896834"
    "9780201896831" "026256114X" "9780201896831")))

(define (isbn-find/list str)
  (for*/list ([line (in-list (split-lines str))]
              [word (in-list (remove-isbn-spaces/list line))]
              [candidate (in-value (remove-isbn-hyphens word))]
              #:when (isbn? candidate))
    candidate))

