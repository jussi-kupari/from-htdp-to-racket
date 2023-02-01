;; -----------------------------------------------------------------------------
;; isbn.v4.rkt 
;; -----------------------------------------------------------------------------

#lang racket/base

(require racket/file)    ;file->string (for tests)
(require racket/match)   ;define/match, match
(require racket/string)  ;string-replace, string-split

(require test-engine/racket-tests)

;; -----------------------------------------------------------------------------
;; Data Types

;; An ISBN is one of:
;; - ISBN-13
;; - ISBN-10

;; An ISBN-String is one of:
;; - ISBN-13-String
;; - ISBN-10-String

;; An ISBN-13-String is a String consisting of 13 digits, that begins
;; with "978" or "979"
(define re-isbn-13 #px"97[89]\\d{10}")

;; An ISBN-10-String is a String consisting of 9 digits and a last
;; letter that can be a digit or 'X'
(define re-isbn-10 #px"\\d{9}[X\\d]")

;; An ISBN-13 is a valid ISBN-13-String
;; An ISBN-10 is a valid ISBN-10-String

;; An ISBN-Format is one of:
;; - 'isbn-13
;; - 'isbn-10

;; A Word is a non-empty String without spaces inside
(define re-word #rx"[^ ]+")

;; An ISBN-Word is a Word whose letters are ISBN letters (digits, "X", and "-")
(define re-isbn-word #px"[\\dX-]+")

;; EXAMPLES

(define isbn-13-ex "9781593274917")
(define isbn-10-ex "0262062186")

(define isbn-13-str-ex-1 "9784567890123")
(define isbn-13-str-ex-2 "9794567890123")

(define isbn-10-str-ex-1 "1234567890")
(define isbn-10-str-ex-2 "123456789X")

;; -----------------------------------------------------------------------------
;; Implementation
;; -----------------------------------------------------------------------------

;; String ISBN-Format -> [Maybe ISBN]
;; extracts the first ISBN of given format from str, if any
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

;; -----------------------------------------------------------------------------
;; ISBN-Format -> [Any -> Boolean]
;; maps the symbolic name for an ISBN format to a predicate
(check-expect ((isbn-format->predicate? 'isbn-13) "9781593274912") #t)
(check-expect ((isbn-format->predicate? 'isbn-10) "026206218X") #t)
(check-expect ((isbn-format->predicate? 'isbn-13) "026206218X") #f)
(check-expect ((isbn-format->predicate? 'isbn-10) "9781593274912") #f)

(define (isbn-format->predicate? format)
  (match format
    ['isbn-13 isbn-13-string?]
    ['isbn-10 isbn-10-string?]))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
;; is v an ISBN-13-String?
(check-expect (isbn-13-string? "9781593274912") #t)
(check-expect (isbn-13-string? "97815932749122") #f) ;too long
(check-expect (isbn-13-string? "978159327491") #f)   ;too short
(check-expect (isbn-13-string? "978c593274a12") #f)  ;not all digits
(check-expect (isbn-13-string? #f) #f)

(define (isbn-13-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-13 v)))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
;; is v an ISBN-10-String?
(check-expect (isbn-10-string? "026206218X") #t)
(check-expect (isbn-10-string? "0262062189X") #f) ;too long
(check-expect (isbn-10-string? "026206218") #f)   ;too short
(check-expect (isbn-10-string? "026f06218X") #f)  ;not all digits
(check-expect (isbn-10-string? "02620621X8") #f)  ;X not last
(check-expect (isbn-10-string? #f) #f)

(define (isbn-10-string? v)
  (and (string? v) (regexp-match-exact? re-isbn-10 v)))

;; -----------------------------------------------------------------------------
;; String -> [List-of ISBN]
;; extracts all ISBNs from str
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

;; -----------------------------------------------------------------------------
;; String -> [List-of Word]
;; splits str into words with ISBN spaces removed
(check-expect (remove-isbn-spaces/list "") '())
(check-expect (remove-isbn-spaces/list "123 abc") '("123" "abc"))
(check-expect (remove-isbn-spaces/list "12 34 ab") '("1234" "ab"))

(define (remove-isbn-spaces/list str) (join-isbn-words (split-words str)))

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

;; Word -> Boolean
;; is word a Word whose letters are legal ISBN letters?
(check-expect (isbn-word? "0123456789X-") #t)
(check-expect (isbn-word? "0123456789 X-") #f)

(define (isbn-word? word)
  (regexp-match-exact? re-isbn-word word))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
;; is v an ISBN?
(check-expect (isbn? "9781593274917") #t)
(check-expect (isbn? "0262062186") #t)
(check-expect (isbn? #f) #f)
  
(define (isbn? v) (or (isbn-13? v) (isbn-10? v)))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
;; is v an ISBN-13?
(check-expect (isbn-13? "9781593274917") #t)
(check-expect (isbn-13? "0262062186") #f)
(check-expect (isbn-13? "") #f)

(define (isbn-13? v) (and (isbn-13-string? v) (isbn-13-valid? v)))

;; -----------------------------------------------------------------------------
;; ISBN-13-String -> Boolean
;; is the given ISBN-13-String a valid ISBN-13?
(check-expect (isbn-13-valid? "9781593274917") #t)
(check-expect (isbn-13-valid? "9780201896831") #t)
(check-expect (isbn-13-valid? "9781593274912") #f)
(check-expect (isbn-13-valid? "9780201896834") #f)

(define (isbn-13-valid? isbn-str)
  (isbn-checksumf
   (in-list (isbn-string->numbers isbn-str)) (in-cycle '(1 3)) 10))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; ISBN-String -> [List-of N]
;; translates str into the numbers their ISBN letters represent
(check-expect (isbn-string->numbers "026256114X") '(0 2 6 2 5 6 1 1 4 10))

(define (isbn-string->numbers str)
  (for/list ([d (in-string str)])
    (match d
      [#\X 10]
      [_ (string->number (string d))])))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
;; is v an ISBN-10?
(check-expect (isbn-10? "9781593274917") #f)
(check-expect (isbn-10? "0262062186") #t)
(check-expect (isbn-10? 1) #f)

(define (isbn-10? v) (and (isbn-10-string? v) (isbn-10-valid? v)))

;; -----------------------------------------------------------------------------
;; ISBN-10-String -> Boolean
;; is the given ISBN-10-String a valid ISBN-10?
(check-expect (isbn-10-valid? "0262062186") #t)
(check-expect (isbn-10-valid? "026256114X") #t)
(check-expect (isbn-10-valid? "026206218X") #f)
(check-expect (isbn-10-valid? "0262561141") #f)

(define (isbn-10-valid? isbn-str)
  (isbn-checksumf
   (in-list (isbn-string->numbers isbn-str)) (in-range 10 0 -1) 11))

;; -----------------------------------------------------------------------------
;; STRING FUNCTIONS

;; String -> String
;; removes hyphens from str
(check-expect (remove-isbn-hyphens "a-b") "ab")

(define (remove-isbn-hyphens str) (string-replace str "-" ""))

;; String -> [List-of String]
;; splits str into lines trimming repeated newlines
(check-expect (split-lines "") '())
(check-expect (split-lines "hi all") '("hi all"))
(check-expect (split-lines "hi\nall\nguys") '("hi" "all" "guys"))
(check-expect (split-lines "\n\nhi\n\n\nall\n\n") '("hi" "all"))

(define (split-lines str) (string-split str "\n" #:repeat? #t))  

;; String -> [List-of Word]
;; splits str into Word's trimming repeated spaces
(check-expect (split-words "") '())
(check-expect (split-words "hi") '("hi"))
(check-expect (split-words "hi world") '("hi" "world"))
(check-expect (split-words " a  bc d    ef  ") '("a" "bc" "d" "ef"))

(define (split-words str) (string-split str " " #:repeat? #t))

;; -----------------------------------------------------------------------------
;; ADDITIONAL TESTS that depend on files 

(define FILE1 "test-isbn-examples")
(define ISBN-11-FROM-FILE1 "0262062186")
(define ISBN-13-FROM-FILE1 "9781593274917")
(check-expect (isbn-find (file->string FILE1) 'isbn-13) ISBN-13-FROM-FILE1)
(check-expect (isbn-find (file->string FILE1) 'isbn-10) ISBN-11-FROM-FILE1)

(test)