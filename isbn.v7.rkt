;; -----------------------------------------------------------------------------
;; isbn-v7.rkt
;; -----------------------------------------------------------------------------

#lang racket/base

;; =============================================================================
;; Isbn Extraction Functions

(require racket/contract)

(provide
 (contract-out
  ; extract the first ISBN, if any, from the given String
  [isbn-find        (-> string? (or/c isbn? #f))]

  ; extracts the first ISBN, if any, (of given format) from the given String
  ; default format: 'isbn-13
  [isbn-find/format (->* (string?) (#:format isbn-format?) (or/c isbn? #f))]
  
  ; extracts all ISBNs from the given String
  [isbn-find/list   (-> string? (listof isbn?))]))

(require (for-syntax syntax/parse racket/base)
         (only-in racket/match match)
         "isbn-utils.rkt")

(module+ test
  (require rackunit
           (only-in racket/file file->string)))

(module+ test
  (check-equal? (isbn-find-abs for*/or "0262062186\n9781593274917")
                "0262062186")
  (check-equal? (isbn-find-abs for*/list "0262062186\n9781593274917")
   '("0262062186" "9781593274917")))

(define-syntax (isbn-find-abs stx)
  (syntax-parse stx
    [(_ iter str)
     #'(iter ([line (in-list (split-lines str))]
              [word (in-list (remove-isbn-spaces/list line))]
              [candidate (in-value (remove-isbn-hyphens word))]
              #:when (isbn? candidate))
             candidate)]))

(module+ test
  (check-false (isbn-find ""))
  (check-equal? (isbn-find "0262062186") "0262062186")
  (check-equal? (isbn-find "9781593274917") "9781593274917")
  (check-equal? (isbn-find "978—0201896831") "9780201896831")
  (check-equal? (isbn-find (file->string "test-isbn-examples")) "0262062186"))

(define (isbn-find str) (isbn-find-abs for*/or str))

(module+ test
  (check-false (isbn-find/format ""))
  (check-false (isbn-find/format "0262062186"))
  (check-false (isbn-find/format "0262062186" #:format 'isbn-13))
  (check-false (isbn-find/format "9781593274917" #:format 'isbn-10))
  (check-equal? (isbn-find/format "978—0201896831" #:format 'isbn-13)
                "9780201896831")
  (check-equal?
   (isbn-find/format (file->string "test-isbn-examples") #:format 'isbn-10)
   "0262062186")
  (check-equal?
   (isbn-find/format (file->string "test-isbn-examples") #:format 'isbn-13)
   "9781593274917"))

(define (isbn-find/format str #:format [format 'isbn-13])
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
    ;isbn w/ several id's a sep's
    "0262062186" "026256114X" "0262062186" "0201896834"
    "9780201896831" "026256114X" "9780201896831")))

(define (isbn-find/list str) (isbn-find-abs for*/list str))

