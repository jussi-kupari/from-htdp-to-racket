;; -----------------------------------------------------------------------------
;; isbn.v5.rkt
;; -----------------------------------------------------------------------------

#lang racket/base

;; =============================================================================

(module utils racket/base
  ; Utils for ISBN Extraction
  
  (require racket/contract)

  ; ----------------------------------------------------------------------------
  
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

    ; determines whether the given string is a valid ISBN-13
    [isbn-13-valid?          (-> isbn-13-string? boolean?)]

    ; determines whether the given string is a valid ISBN-10
    [isbn-10-valid?          (-> isbn-10-string? boolean?)]

    ; maps the given ISBN-Format to a predicate
    [isbn-format->predicate? (-> isbn-format? (-> any/c boolean?))]
  
    ; removes hyphens (Unicode dash points) from the given String
    [remove-isbn-hyphens          (-> string? string?)]

    ; splits the given string into words with ISBN spaces removed
    [remove-isbn-spaces/list (-> string? (listof word?))]

    ; splits the given String into lines trimming repeated newlines
    [split-lines             (-> string? (listof string?))]))

  ; ----------------------------------------------------------------------------

  (require (only-in racket/match define/match match))
  (require (only-in racket/sequence sequence/c))
  (require (only-in racket/string string-replace string-split))
  (require test-engine/racket-tests)
  
  ; ----------------------------------------------------------------------------
  ; Constants and Predicates

  (define re-isbn-13 #px"97[89]\\d{10}")
  (define re-isbn-10 #px"\\d{9}[X\\d]")
  (define re-word #rx"[^ ]+")
  (define re-isbn-word #px"[\\dX-]+")

  (check-expect (isbn? "9781593274917") #t)
  (check-expect (isbn? "0262062186") #t)
  (check-expect (isbn? #f) #f)
  
  (define (isbn? v) (or (isbn-13? v) (isbn-10? v)))

  (check-expect (isbn-13? "9781593274917") #t)
  (check-expect (isbn-13? "0262062186") #f)
  (check-expect (isbn-13? "") #f)

  (define (isbn-13? v) (and (isbn-13-string? v) (isbn-13-valid? v)))

  (check-expect (isbn-13-string? "9781593274912") #t)
  (check-expect (isbn-13-string? "97815932749122") #f) ;too long
  (check-expect (isbn-13-string? "978159327491") #f)   ;too short
  (check-expect (isbn-13-string? "978c593274a12") #f)  ;not all digits
  (check-expect (isbn-13-string? #f) #f)

  (define (isbn-13-string? v)
    (and (string? v) (regexp-match-exact? re-isbn-13 v)))

  (check-expect (isbn-10? "9781593274917") #f)
  (check-expect (isbn-10? "0262062186") #t)
  (check-expect (isbn-10? 1) #f)

  (define (isbn-10? v) (and (isbn-10-string? v) (isbn-10-valid? v)))

  (check-expect (isbn-10-string? "026206218X") #t)
  (check-expect (isbn-10-string? "0262062189X") #f) ;too long
  (check-expect (isbn-10-string? "026206218") #f)   ;too short
  (check-expect (isbn-10-string? "026f06218X") #f)  ;not all digits
  (check-expect (isbn-10-string? "02620621X8") #f)  ;X not last
  (check-expect (isbn-10-string? #f) #f)

  (define (isbn-10-string? v)
    (and (string? v) (regexp-match-exact? re-isbn-10 v)))

  (check-expect (isbn-format? 'isbn-13) #t)
  (check-expect (isbn-format? 'isbn-10) #t)
  (check-expect (isbn-format? "isbn-13") #f)

  (define (isbn-format? v) (or (equal? v 'isbn-13) (equal? v 'isbn-10)))

  (check-expect (isbn-string? "9780123456789") #t)
  (check-expect (isbn-string? "012345678X") #t)
  (check-expect (isbn-string? "abcde") #f)

  (define (isbn-string? v) (or (isbn-13-string? v) (isbn-10-string? v)))

  (check-expect (isbn-word? "0123456789X-") #t)
  (check-expect (isbn-word? "0123456789 X-") #f)

  (define (isbn-word? word) (regexp-match-exact? re-isbn-word word))

  (check-expect (word? "a") #t)
  (check-expect (word? "ab123") #t)
  (check-expect (word? "") #f)
  (check-expect (word? "ab c") #f)

  (define (word? s) (regexp-match-exact? re-word s))

  ; ----------------------------------------------------------------------------
  ; Main utils

  (check-expect (isbn-13-valid? "9781593274917") #t)
  (check-expect (isbn-13-valid? "9780201896831") #t)
  (check-expect (isbn-13-valid? "9781593274912") #f)
  (check-expect (isbn-13-valid? "9780201896834") #f)

  (define (isbn-13-valid? isbn-str)
    (isbn-checksumf
     (in-list (isbn-string->numbers isbn-str)) (in-cycle '(1 3)) 10))

  (check-expect (isbn-10-valid? "0262062186") #t)
  (check-expect (isbn-10-valid? "026256114X") #t)
  (check-expect (isbn-10-valid? "026206218X") #f)
  (check-expect (isbn-10-valid? "0262561141") #f)

  (define (isbn-10-valid? isbn-str)
    (isbn-checksumf
     (in-list (isbn-string->numbers isbn-str)) (in-range 10 0 -1) 11))

  (check-expect ((isbn-format->predicate? 'isbn-13) "9781593274912") #t)
  (check-expect ((isbn-format->predicate? 'isbn-10) "026206218X") #t)
  (check-expect ((isbn-format->predicate? 'isbn-13) "026206218X") #f)
  (check-expect ((isbn-format->predicate? 'isbn-10) "9781593274912") #f)

  (define (isbn-format->predicate? format)
    (match format
      ['isbn-13 isbn-13-string?]
      ['isbn-10 isbn-10-string?]))
    
  (check-expect (remove-isbn-spaces/list "") '())
  (check-expect (remove-isbn-spaces/list "123 abc") '("123" "abc"))
  (check-expect (remove-isbn-spaces/list "12 34 ab") '("1234" "ab"))

  (define (remove-isbn-spaces/list str) (join-isbn-words (split-words str)))

  (check-expect (remove-isbn-hyphens "a-b") "ab")

  (define (remove-isbn-hyphens str) (string-replace str "-" ""))

  (check-expect (split-lines "") '())
  (check-expect (split-lines "hi all") '("hi all"))
  (check-expect (split-lines "hi\nall\nguys") '("hi" "all" "guys"))
  (check-expect (split-lines "\n\nhi\n\n\nall\n\n") '("hi" "all"))

  (define (split-lines str) (string-split str "\n" #:repeat? #t))  

  ; ----------------------------------------------------------------------------
  ; Helpers

  ;; [Sequence-of N] [Sequence-of N] N -> Boolean
  ;; abstract checksum algorithm for ISBN validation
  (check-expect (isbn-checksumf '(9 7 8 1 5 9 3 2 7 4 9 1 7)
                                '(1 3 1 3 1 3 1 3 1 3 1 3 1)
                                10) #t)
  (check-expect (isbn-checksumf '(0 2 6 2 0 6 2 1 8 10)
                                (in-range 10 0 -1)
                                11) #f)

  (define (isbn-checksumf multiplicands multipliers mod)
    (define sum (for/sum ([x multiplicands] [y multipliers]) (* x y)))
    (zero? (modulo sum mod)))
  
  ;; ISBN-String -> [List-of N]
  ;; translates str into the numbers their ISBN letters represent
  (check-expect (isbn-string->numbers "026256114X") '(0 2 6 2 5 6 1 1 4 10))

  (define (isbn-string->numbers str)
    (for/list ([d (in-string str)])
      (match d
        [#\X 10]
        [_ (string->number (string d))])))

  ;; [List-of Word] -> [List-of Word]
  ;; joins consecutive ISBN-Word's in lowords
  (check-expect (join-isbn-words '()) '())
  (check-expect (join-isbn-words '("123")) '("123"))
  (check-expect (join-isbn-words '("123" "abc")) '("123" "abc"))
  (check-expect (join-isbn-words '("abc" "123")) '("abc" "123"))
  (check-expect (join-isbn-words '("12a" "123")) '("12a" "123"))
  (check-expect (join-isbn-words '("123" "12a")) '("123" "12a"))
  (check-expect (join-isbn-words '("12" "34" "ab")) '("1234" "ab"))
  (check-expect (join-isbn-words '("ab" "12" "34")) '("ab" "1234"))
  (check-expect (join-isbn-words '("ab" "1" "23" "X" "c")) '("ab" "123X" "c"))
  (check-expect (join-isbn-words '("ISBN:" "0" "262" "56114" "-X"))
                '("ISBN:" "026256114-X"))

  (define (join-isbn-words lowords)
    (for/fold ([acc '()]) ([word (in-list (reverse lowords))])
      (adjoin word acc)))

  ;; Word [List-of Word] -> [List-of Word]
  ;; insert word at the front of lowords, joining it with
  ;; the first Word in lowords if both are ISBN-Word's
  (check-expect (adjoin "a" '()) '("a"))
  (check-expect (adjoin "0" '("262" "56114")) '("0262" "56114"))
  (check-expect (adjoin "0" '("a" "X")) '("0" "a" "X"))
  (check-expect (adjoin "a" '("0" "cd")) '("a" "0" "cd"))
  (check-expect (adjoin "a" '("bc" "def")) '("a" "bc" "def"))

  (define/match (adjoin word lowords)
    [(w '()) (list w)]
    [([? isbn-word? w] [cons (? isbn-word? w1) ws])
     (cons (string-append w w1) ws)]
    [(_ _) (cons word lowords)])

  ;; String -> [List-of Word]
  ;; splits str into Word's trimming repeated spaces
  (check-expect (split-words "") '())
  (check-expect (split-words "hi") '("hi"))
  (check-expect (split-words "hi world") '("hi" "world"))
  (check-expect (split-words " a  bc d    ef  ") '("a" "bc" "d" "ef"))

  (define (split-words str) (string-split str " " #:repeat? #t)))

;; =============================================================================

(module main racket/base
  ; ISBN extraction from Strings
  
  (require racket/contract)
  
  (provide
   (contract-out
    ; extracts the first ISBN (of given format) from the given String, if any
    [isbn-find      (-> string? isbn-format? (or/c isbn? #f))]

    ; extracts all ISBNs from the given String
    [isbn-find/list (-> string? (listof isbn?))]))

  ; ----------------------------------------------------------------------------

  (require (only-in racket/file file->string))
  (require (only-in racket/match match))  
  (require test-engine/racket-tests)
  (require (submod ".." utils))

  ; ----------------------------------------------------------------------------

  (check-expect (isbn-find "" 'isbn-13) #f)
  (check-expect (isbn-find "" 'isbn-10) #f)
  (check-expect (isbn-find "0262062186" 'isbn-13) #f)
  (check-expect (isbn-find "9781593274917" 'isbn-10) #f)

  (define (isbn-find str format)
    (define p? (isbn-format->predicate? format))
    (for/or ([candidate (in-list (isbn-find/list str))])
      (match candidate
        [(? p? c) c]
        [_ #f])))
  
  (check-expect (isbn-find/list "") '())
  (check-expect (isbn-find/list "none") '())
  (check-expect (isbn-find/list (file->string "test-isbn-examples"))
                (list
                 ;isbn normalized
                 "0262062186" "026256114X" "1593274912"
                 "9781593274917" "0201896834" "9780201896831"
                 ;isbn w/ several id's and sep's
                 "0262062186" "026256114X" "0262062186" "0201896834"
                 "9780201896831" "026256114X" "9780201896831"))

  (define (isbn-find/list str)
    (for*/list ([line (in-list (split-lines str))]
                [word (in-list (remove-isbn-spaces/list line))]
                [candidate (in-value (remove-isbn-hyphens word))]
                #:when (isbn? candidate))
      candidate))

  ; ---------------------------------------------------------------------------
  ; ADDITIONAL TESTS that depend on files 

  (define FILE1 "test-isbn-examples")
  (define ISBN-11-FROM-FILE1 "0262062186")
  (define ISBN-13-FROM-FILE1 "9781593274917")
  (check-expect (isbn-find (file->string FILE1) 'isbn-13) ISBN-13-FROM-FILE1)
  (check-expect (isbn-find (file->string FILE1) 'isbn-10) ISBN-11-FROM-FILE1)

  (test))

(require 'main)
