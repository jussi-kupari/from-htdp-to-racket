;; -----------------------------------------------------------------------------
;; isbn-utils.rkt
;; -----------------------------------------------------------------------------

#lang racket/base

;; =============================================================================
;; Utils for ISBN Extraction 
  
(require racket/contract)

;; -----------------------------------------------------------------------------
  
(provide
 (contract-out
  ; determine whether the given value is an ISBN
  [isbn? predicate/c]

  ; determines whether the given value is an ISBN-13
  [isbn-13? predicate/c]

  ; determines whether the given value is an ISBN-10
  [isbn-10? predicate/c]

  ; determines whether the given value is an ISBN-String
  [isbn-string? predicate/c]

  ; determines whether the given value is an ISBN-13-String
  [isbn-13-string? predicate/c]

  ; determines whether the given value is an ISBN-10-String
  [isbn-10-string? predicate/c]

  ; determines whether the given value is an ISBN-Format
  [isbn-format? predicate/c]

  ; determines whether the given word is an ISBN-Word
  [isbn-word? (-> word? boolean?)]

  ; determines whether the given String is a valid ISBN-13
  [isbn-13-valid?          (-> isbn-13-string? boolean?)]

  ; determines whether the given String is a valid ISBN-10
  [isbn-10-valid?          (-> isbn-10-string? boolean?)]

  ; maps the given ISBN-Format to a predicate
  [isbn-format->predicate? (-> isbn-format? (-> any/c boolean?))]
  
  ; removes hyphens (Unicode dash points) from the given String
  [remove-isbn-hyphens     (-> string? string?)]

  ; splits the given string into words with ISBN spaces removed
  [remove-isbn-spaces/list (-> string? (listof word?))]

  ; splits the given String into lines trimming repeated newlines
  [split-lines             (-> string? (listof string?))]))

;; -----------------------------------------------------------------------------

(require (only-in racket/match define/match match)
         (only-in racket/string string-replace string-split))
  
(module+ test
  (require rackunit))
  
;; -----------------------------------------------------------------------------
; Constants and Predicates

(define re-isbn-13 #px"97[89]\\d{10}")
(define re-isbn-10 #px"\\d{9}[X\\d]")
(define re-word #rx"[^ ]+")
(define isbn-dash "\\p{Pd}")
(define re-isbn-dash (pregexp isbn-dash))
(define re-isbn-word (pregexp (string-append "(" "[\\dX]" "|" isbn-dash ")+")))

(module+ test
  (check-true (isbn? "9781593274917"))
  (check-true (isbn? "0262062186"))
  (check-false (isbn? #f)))
  
(define (isbn? v) (or (isbn-13? v) (isbn-10? v)))

(module+ test
  (check-true (isbn-13? "9781593274917"))
  (check-false (isbn-13? "0262062186"))
  (check-false (isbn-13? "")))

(define (isbn-13? v) (and (isbn-13-string? v) (isbn-13-valid? v)))

(module+ test
  (check-true (isbn-13-string? "9781593274912"))
  (check-false (isbn-13-string? "97815932749122")) ;too long
  (check-false (isbn-13-string? "978159327491"))   ;too short
  (check-false (isbn-13-string? "978c593274a12"))  ;not all digits
  (check-false (isbn-13-string? #f)))

(define (isbn-13-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-13 v)))

(module+ test
  (check-false (isbn-10? "9781593274917"))
  (check-true (isbn-10? "0262062186"))
  (check-false (isbn-10? 1)))

(define (isbn-10? v) (and (isbn-10-string? v) (isbn-10-valid? v)))

(module+ test
  (check-true (isbn-10-string? "026206218X"))
  (check-false (isbn-10-string? "0262062189X")) ;too long
  (check-false (isbn-10-string? "026206218"))   ;too short
  (check-false (isbn-10-string? "026f06218X"))  ;not all digits
  (check-false (isbn-10-string? "02620621X8"))  ;X not last
  (check-false (isbn-10-string? #f)))

(define (isbn-10-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-10 v)))

(module+ test
  (check-true (isbn-format? 'isbn-13))
  (check-true (isbn-format? 'isbn-10))
  (check-false (isbn-format? "isbn-13")))

(define (isbn-format? v) (or (equal? v 'isbn-13) (equal? v 'isbn-10)))

(module+ test
  (check-true (isbn-string? "9780123456789"))
  (check-true (isbn-string? "012345678X"))
  (check-false (isbn-string? "abcde")))

(define (isbn-string? v) (or (isbn-13-string? v) (isbn-10-string? v)))

(module+ test
  (check-true (isbn-word? "0123456789X-"))
  (check-false (isbn-word? "0123456789 X-")))

(define (isbn-word? word) (regexp-match-exact? re-isbn-word word))

(module+ test
  (check-true (word? "a"))
  (check-true (word? "ab123"))
  (check-false (word? ""))
  (check-false (word? "ab c")))

(define (word? s) (regexp-match-exact? re-word s))

;; -----------------------------------------------------------------------------
; Main utils

(module+ test
  (check-true (isbn-13-valid? "9781593274917"))
  (check-true (isbn-13-valid? "9780201896831"))
  (check-false (isbn-13-valid? "9781593274912"))
  (check-false (isbn-13-valid? "9780201896834")))

(define (isbn-13-valid? isbn-str)
  (isbn-checksumf
   (in-list (isbn-string->numbers isbn-str)) (in-cycle '(1 3)) 10))

(module+ test
  (check-true (isbn-10-valid? "0262062186"))
  (check-true (isbn-10-valid? "026256114X"))
  (check-false (isbn-10-valid? "026206218X"))
  (check-false (isbn-10-valid? "0262561141")))

(define (isbn-10-valid? isbn-str)
  (isbn-checksumf
   (in-list (isbn-string->numbers isbn-str)) (in-range 10 0 -1) 11))

(module+ test
  (check-equal? (remove-isbn-hyphens "978-0-201-89683-1") "9780201896831")
  (check-equal? (remove-isbn-hyphens "0—262—06218—6") "0262062186")
  ;output may be empty string (not a word)
  (check-equal? (remove-isbn-hyphens "-") ""))

(define (remove-isbn-hyphens word) (string-replace word re-isbn-dash ""))

(module+ test
  (check-true ((isbn-format->predicate? 'isbn-13) "9781593274912"))
  (check-true ((isbn-format->predicate? 'isbn-10) "026206218X"))
  (check-false ((isbn-format->predicate? 'isbn-13) "026206218X"))
  (check-false ((isbn-format->predicate? 'isbn-10) "9781593274912")))

(define (isbn-format->predicate? format)
  (match format
    ['isbn-13 isbn-13-string?]
    ['isbn-10 isbn-10-string?]))

(module+ test
  (check-equal? (remove-isbn-spaces/list "") '())
  (check-equal? (remove-isbn-spaces/list "123 abc") '("123" "abc"))
  (check-equal? (remove-isbn-spaces/list "12 34 ab") '("1234" "ab")))

(define (remove-isbn-spaces/list str) (join-isbn-words (split-words str)))

(module+ test
  (check-equal? (split-lines "") '())
  (check-equal? (split-lines "hi all") '("hi all"))
  (check-equal? (split-lines "hi\nall\nguys") '("hi" "all" "guys"))
  (check-equal? (split-lines "\n\nhi\n\n\nall\n\n") '("hi" "all")))

(define (split-lines str) (string-split str "\n" #:repeat? #t))

;; -----------------------------------------------------------------------------
; Helpers

;; [Sequence-of N] [Sequence-of N] N -> Boolean
;; abstract checksum algorithm for ISBN validation
(module+ test
  (check-true (isbn-checksumf '(9 7 8 1 5 9 3 2 7 4 9 1 7)
                              '(1 3 1 3 1 3 1 3 1 3 1 3 1)
                              10))
  (check-false (isbn-checksumf '(0 2 6 2 0 6 2 1 8 10)
                               (in-range 10 0 -1)
                               11)))

(define (isbn-checksumf multiplicands multipliers mod)
  (define sum (for/sum ([x multiplicands] [y multipliers]) (* x y)))
  (zero? (modulo sum mod)))
  
;; ISBN-String -> [List-of N]
;; translates str into the numbers their ISBN letters represent
(module+ test
  (check-equal? (isbn-string->numbers "026256114X") '(0 2 6 2 5 6 1 1 4 10)))

(define (isbn-string->numbers str)
  (for/list ([d (in-string str)])
    (match d
      [#\X 10]
      [_ (string->number (string d))])))

;; [List-of Word] -> [List-of Word]
;; joins consecutive ISBN-Word's in lowords
(module+ test
  (check-equal? (join-isbn-words '()) '())
  (check-equal? (join-isbn-words '("123")) '("123"))
  (check-equal? (join-isbn-words '("123" "abc")) '("123" "abc"))
  (check-equal? (join-isbn-words '("abc" "123")) '("abc" "123"))
  (check-equal? (join-isbn-words '("12a" "123")) '("12a" "123"))
  (check-equal? (join-isbn-words '("123" "12a")) '("123" "12a"))
  (check-equal? (join-isbn-words '("12" "34" "ab")) '("1234" "ab"))
  (check-equal? (join-isbn-words '("ab" "12" "34")) '("ab" "1234"))
  (check-equal? (join-isbn-words '("ab" "1" "23" "X" "c")) '("ab" "123X" "c"))
  (check-equal? (join-isbn-words '("ISBN:" "0" "262" "56114" "-X"))
                '("ISBN:" "026256114-X"))
  (check-equal? (join-isbn-words '("ISBN:" "0" "262" "56114" "—X"))
                '("ISBN:" "026256114—X")))

(define (join-isbn-words lowords)
  (for/fold ([acc '()]) ([word (in-list (reverse lowords))])
    (adjoin word acc)))

;; Word [List-of Word] -> [List-of Word]
;; insert word at the front of lowords, joining it with
;; the first Word in lowords if both are ISBN-Word's
(module+ test
  (check-equal? (adjoin "a" '()) '("a"))
  (check-equal? (adjoin "0" '("262" "56114")) '("0262" "56114"))
  (check-equal? (adjoin "0" '("a" "X")) '("0" "a" "X"))
  (check-equal? (adjoin "a" '("0" "cd")) '("a" "0" "cd"))
  (check-equal? (adjoin "a" '("bc" "def")) '("a" "bc" "def")))

(define/match (adjoin word lowords)
  [(w '()) (list w)]
  [([? isbn-word? w] [cons (? isbn-word? w1) ws])
   (cons (string-append w w1) ws)]
  [(_ _) (cons word lowords)])

;; String -> [List-of Word]
;; splits str into Word's trimming repeated spaces
(module+ test
  (check-equal? (split-words "") '())
  (check-equal? (split-words "hi") '("hi"))
  (check-equal? (split-words "hi world") '("hi" "world"))
  (check-equal? (split-words " a  bc d    ef  ") '("a" "bc" "d" "ef")))

(define (split-words str) (string-split str " " #:repeat? #t))




