#lang htdp/isl+

; ------------------------------------------------------------------------------
; STRING AND LIST FUNCTIONS THAT OUGHT TO BE IN ISL+

(require "isbn-teachpack.rkt")

(provide
 ; String 1String -> [List-of String]
 ; produces the substrings of str separated by sep
 ; trims str repeatedly using sep before splitting
 string-split

 ; String -> [List-of String]
 ; splits str into lines
 split-lines

 ; String -> [List-of String]
 ; splits str into words
 split-words)

;; -----------------------------------------------------------------------------
(define (split-lines str) (string-split str "\n"))

(define (split-words str) (string-split str " "))

; ------------------------------------------------------------------------------
(check-expect (string-split "" " ") '())
(check-expect (string-split "hi" " ") '("hi"))
(check-expect (string-split "hi all bye" " ") '("hi" "all" "bye"))
(check-expect (string-split "hi all bye" "\n") '("hi all bye"))
(check-expect (string-split "hi all\nbye" "\n") '("hi all" "bye"))

(define (string-split str sep)
  (local [;; String -> Boolean 
          (define (not-sep? x) (not (string=? x sep)))
          (define tidy-str (string-trim str sep))
          (define first-word (string-takef tidy-str not-sep?))
          (define rest-words (string-dropf tidy-str not-sep?))]
    (cond
      [(string=? tidy-str "") '()]
      [else (cons first-word (string-split rest-words sep))])))

; ------------------------------------------------------------------------------
; String 1String -> String
; removes leading and trailing seps from str

(check-expect (string-trim "" " ") "")
(check-expect (string-trim "ab" " ") "ab")
(check-expect (string-trim "  \nab c \n  " " ") "\nab c \n")
(check-expect (string-trim "\n ab\nc \n\n\n" "\n") " ab\nc ")

(define (string-trim str sep)
  (string-trim-suffix (string-trim-prefix str sep) sep))

; ------------------------------------------------------------------------------
; String 1String -> String
; removes leading seps from str

(check-expect (string-trim-prefix "" " ") "")
(check-expect (string-trim-prefix "ab" " ") "ab")
(check-expect (string-trim-prefix "  \nab c \n" " ") "\nab c \n")
(check-expect (string-trim-prefix "\n\n ab\n" "\n") " ab\n")

(define (string-trim-prefix str sep)
  (string-dropf str (lambda (x) (string=? x sep))))

; ------------------------------------------------------------------------------
; String 1String -> String
; removes trailing seps from str

(check-expect (string-trim-suffix "" " ") "")
(check-expect (string-trim-suffix "ab" " ") "ab")
(check-expect (string-trim-suffix " \nab c \n\n" "\n") " \nab c ")
(check-expect (string-trim-suffix " ab c\n   " " ") " ab c\n")

(define (string-trim-suffix str sep)
  (string-dropf-right str (lambda (x) (string=? x sep))))

; ------------------------------------------------------------------------------
; String [1String -> Boolean] -> String
; drops letters in str while they satisfy p?

(check-expect (string-dropf "" string-upper-case?) "")
(check-expect (string-dropf "a" string-upper-case?) "a")
(check-expect (string-dropf "A" string-upper-case?) "")
(check-expect (string-dropf "abc" string-upper-case?) "abc")
(check-expect (string-dropf "ABc" string-upper-case?) "c")

(define (string-dropf str p?)
  (implode (dropf (explode str) p?)))

; ------------------------------------------------------------------------------
; String [1String -> Boolean] -> String
; drops letters in str from the end while they satisfy p?
(check-expect (string-dropf-right "" string-upper-case?) "")
(check-expect (string-dropf-right "a" string-upper-case?) "a")
(check-expect (string-dropf-right "A" string-upper-case?) "")
(check-expect (string-dropf-right "cba" string-upper-case?) "cba")
(check-expect (string-dropf-right "cBA" string-upper-case?) "c")

(define (string-dropf-right str p?)
  (implode (dropf-right (explode str) p?)))

; ------------------------------------------------------------------------------
; String [1String -> Boolean] -> String
; takes letters in str while they satisfy p?
(check-expect (string-takef "" string-upper-case?) "")
(check-expect (string-takef "a" string-upper-case?) "")
(check-expect (string-takef "A" string-upper-case?) "A")
(check-expect (string-takef "abc" string-upper-case?) "")
(check-expect (string-takef "ABc" string-upper-case?) "AB")
 
(define (string-takef str p?)
  (implode (takef (explode str) p?)))

; ------------------------------------------------------------------------------
; [List-of X] [X -> Boolean] -> [List-of X]
; drops elements in lox while they satisfy p?
(check-expect (dropf '() even?) '())
(check-expect (dropf '(1) even?) '(1))
(check-expect (dropf '(2) even?) '())
(check-expect (dropf '(1 2 3) even?) '(1 2 3))
(check-expect (dropf '(2 4 1) even?) '(1))
(check-expect (dropf '("1" "2" "a") string-numeric?) '("a"))

(define (dropf lox p?)
  (cond
    [(empty? lox) '()]
    [else
     (if (p? (first lox))
         (dropf (rest lox) p?)
         lox)]))

; ------------------------------------------------------------------------------
; [List-of X] [X -> Boolean] -> [List-of X]
; drops elements in lox from the end while they satisfy p?
(check-expect (dropf-right '() even?) '())
(check-expect (dropf-right '(1) even?) '(1))
(check-expect (dropf-right '(2) even?) '())
(check-expect (dropf-right '(3 2 1) even?) '(3 2 1))
(check-expect (dropf-right '(1 4 2) even?) '(1))
(check-expect (dropf-right '("a" "2" "1") string-numeric?) '("a"))

(define (dropf-right lox p?)
  (reverse (dropf (reverse lox) p?)))

; ------------------------------------------------------------------------------
; [List-of X] [X -> Boolean] -> [List-of X]
; takes elements in lox while they satisfy p?
(check-expect (takef '() even?) '())
(check-expect (takef '(1) even?) '())
(check-expect (takef '(2) even?) '(2))
(check-expect (takef '(1 2 3) even?) '())
(check-expect (takef '(2 4 1) even?) '(2 4))
(check-expect (takef '("1" "2" "a") string-numeric?) '("1" "2"))

(define (takef lox p?)
  (cond
    [(empty? lox) '()]
    [else
     (if (p? (first lox))
         (cons (first lox) (takef (rest lox) p?))
         '())]))