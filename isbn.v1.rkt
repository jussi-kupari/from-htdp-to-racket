;; -----------------------------------------------------------------------------
;; isbn.v1.rkt
;; -----------------------------------------------------------------------------

#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/batch-io) ;read-file (for tests)
(require "isbn-lib.rkt")

;; -----------------------------------------------------------------------------
;; Data Types

;; An ISBN is one of:
;; - ISBN-13
;; - ISBN-10

;; An ISBN-String is one of:
;; - ISBN-13-String
;; - ISBN-10-String

;; An ISBN-13 is a valid ISBN-13-String
;; An ISBN-10 is a valid ISBN-10-String
;; where valid means that the numbers in that String
;; fullfil a certain mathematical computation. (See
;; ISBN User Manual for information about this computation)

;; An ISBN-Format is one of:
;; - 'isbn-13
;; - 'isbn-10

;; An ISBN-Word is a String that can contain only ISBN-Letter's

;; A Digit is one of: "0", "1", ... "9".
(define digits (explode "0123456789"))

;; An ISBN-Letter is one of:
;; - Digit
;; - "X"
;; - "-"
(define isbn-letters (append digits (list "X" "-")))

;; EXAMPLES 

(define isbn-13-ex "9781593274917")
(define isbn-10-ex "0262062186")

;; An ISBN-13-String is a String consisting of 13 digits
(define isbn-13-str-ex "1234567890123")

;; An ISBN-10-String is a String consisting of 9 digits,
;; and a last letter that can be a digit or 'X'.
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
  (local [(define p? (isbn-format->predicate? format))]
    (for/or ([candidate (isbn-find/list str)])
      (match candidate
        [(? p? c) c]
        [_ #f]))))

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
  (and (string? v) (= (string-length v) 13) (all-digits? v)))

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
  (and (string? v) (= (string-length v) 10) (digits+optional-x? v)))

;; String[10] -> Boolean
;; does str consist of 9 digits plus and X or of 10 digits?
;; ASSUME str consists of 10 1String
(check-expect (digits+optional-x? "0123456789") #t)
(check-expect (digits+optional-x? "012345678X") #t)
(check-expect (digits+optional-x? "012345678a") #f)
(check-expect (digits+optional-x? "01234567aX") #f)

(define (digits+optional-x? str)
  (local [(define init (substring str 0 9))
          (define last (substring str 9))]
    (and (all-digits? init) (or (digit? last) (string=? last "X")))))

;; -----------------------------------------------------------------------------
;; String -> [List-of ISBN]
;; extracts all ISBNs from str
(check-expect (isbn-find/list "") '())
(check-expect (isbn-find/list "none") '())
(check-expect (isbn-find/list (read-file "test-isbn-examples"))
              (list
               ;isbn normalized
               "0262062186" "026256114X" "1593274912"
               "9781593274917" "0201896834" "9780201896831"
               ;isbn w/ several id's and sep's
               "0262062186" "026256114X" "0262062186" "0201896834"
               "9780201896831" "026256114X" "9780201896831"))

(define (isbn-find/list str)
  (local [;; [List-of String]
          (define candidates
            (for*/list ([line (split-lines str)]
                        [word (remove-isbn-spaces/list line)])
              (remove-isbn-hyphens word)))]
    (filter isbn? candidates)))

;; -----------------------------------------------------------------------------
;; String -> [List-of String]
;; splits str into words with ISBN spaces removed
(check-expect (remove-isbn-spaces/list "") '())
(check-expect (remove-isbn-spaces/list "123 abc") '("123" "abc"))
(check-expect (remove-isbn-spaces/list "12 34 ab") '("1234" "ab"))

(define (remove-isbn-spaces/list str) (join-isbn-words (split-words str)))

;; [List-of String] -> [List-of String]
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
(check-expect (join-isbn-words '("0" "262" "56114" "-X")) '("026256114-X"))

(define (join-isbn-words lowords)
  (foldr adjoin '() lowords))

;; String [List-of String] -> [List-of String]
;; insert word at the front of lowords, joining it with
;; the first String in lowords if both are ISBN-Word's
(check-expect (adjoin "a" '()) '("a"))
(check-expect (adjoin "0" '("262" "56114")) '("0262" "56114"))
(check-expect (adjoin "0" '("a" "X")) '("0" "a" "X"))
(check-expect (adjoin "a" '("0" "cd")) '("a" "0" "cd"))
(check-expect (adjoin "a" '("bc" "def")) '("a" "bc" "def"))

(define (adjoin word lowords)
  (cond
    [(empty? lowords) (list word)]
    [else
     (if (and (isbn-word? word) (isbn-word? (first lowords)))
         (cons (string-append word (first lowords)) (rest lowords))
         (cons word lowords))]))

;; String -> Boolean
;; is str an ISBN-Word?
(check-expect (isbn-word? "0123456789X-") #t)
(check-expect (isbn-word? "0123456789 X-") #f)

(define (isbn-word? str) (for/and ([s (explode str)]) (isbn-letter? s)))

;; 1String -> Boolean
;; is str and ISBN-Letter?
(check-expect (isbn-letter? (number->string (random 10))) #t)
(check-expect (isbn-letter? "X") #t)
(check-expect (isbn-letter? "-") #t)
(check-expect (isbn-letter? "a") #f)

(define (isbn-letter? str) (member? str isbn-letters))

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
  (isbn-checksumf (isbn-string->numbers isbn-str)
                  '(1 3 1 3 1 3 1 3 1 3 1 3 1)
                  10))

;; -----------------------------------------------------------------------------
;; [List-of N] [List-of N] N -> Boolean
;; abstract checksum algorithm for ISBN validation
(check-expect (isbn-checksumf '(9 7 8 1 5 9 3 2 7 4 9 1 7)
                              '(1 3 1 3 1 3 1 3 1 3 1 3 1)
                              10) #t)
(check-expect (isbn-checksumf '(0 2 6 2 0 6 2 1 8 10)
                              (range 10 0 -1)
                              11) #f)

(define (isbn-checksumf multiplicands multipliers mod)
  (local [(define sum
            (for/sum ([x multiplicands] [y multipliers]) (* x y)))]
    (zero? (modulo sum mod))))

;; -----------------------------------------------------------------------------
;; ISBN-String -> [List-of N]
;; translates str into the numbers their ISBN letters represent
(check-expect (isbn-string->numbers "026256114X") '(0 2 6 2 5 6 1 1 4 10))

(define (isbn-string->numbers str)
  (for/list ([d (explode str)])
    (match d
      ["X" 10]
      [_ (string->number d)])))

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
  (isbn-checksumf (isbn-string->numbers isbn-str) (range 10 0 -1) 11))

;; -----------------------------------------------------------------------------
;; STRING FUNCTIONS

;; String -> String
;; removes hyphens from str
(check-expect (remove-isbn-hyphens "a-b") "ab")

(define (remove-isbn-hyphens str)
  (implode (filter (lambda (c) (not (string=? c "-"))) (explode str))))

;; -----------------------------------------------------------------------------
;; DIGIT FUNCTIONS

;; String -> Boolean
;; does the String consist of digits?
(check-expect (all-digits? "a012") #f)
(check-expect (all-digits? "3012") #t)

(define (all-digits? init)
  (andmap digit? (explode init)))

;; Any -> Boolean
;; is v a digit?
(check-expect (digit? (number->string (random 10))) #t)
(check-expect (digit? "a") #f)

(define (digit? v) (member? v digits))

;; -----------------------------------------------------------------------------
;; ADDITIONAL TESTS that depend on files 

(define FILE1 "test-isbn-examples")
(define ISBN-11-FROM-FILE1 "0262062186")
(define ISBN-13-FROM-FILE1 "9781593274917")
(check-expect (isbn-find (read-file FILE1) 'isbn-13) ISBN-13-FROM-FILE1)
(check-expect (isbn-find (read-file FILE1) 'isbn-10) ISBN-11-FROM-FILE1)
