#lang racket/base

;; =============================================================================
; ISBN extraction from pdf files

; REMARK:
; To improve performance when handling large pdf files, this module relies on
; the ISBN Standard, according to which the ISBN should appear on the title
; page(s) (= within the first pdf pages) and/or the backcover (= last pdf page)

(require racket/contract)

(provide
 (contract-out
  ; extracts all isbn's from the given pdf document
  ; raises exception when the pdf file does not exist
  ; produces '() if pdf couldn't be opened
  [extract-isbn-from-pdf/list (-> pdf-document? (listof isbn?))]

  ; extract the first isbn from the given pdf document, if any
  ; raises exception when the pdf file does not exist
  ; produces #f if pdf couldn't be opened
  [extract-isbn-from-pdf      (-> pdf-document? (or/c isbn? #f))]

  ; maximum number of initial pages on which to search for the isbn
  [current-pdf-max-page       (->* () (natural-number/c) parameter?)]))

(require (for-syntax syntax/parse racket/base)
         (only-in racket/list flatten range)
         (only-in racket/set set-add)
         pdf-read
         "isbn.rkt"
         (only-in "isbn-utils.rkt" isbn?))

(module+ test
  (require rackunit)
  
  ; EXAMPLES
  (define pdf-with-isbn "test-isbn-examples.pdf")
  (define pdf-with-isbn-10-only "test-with-isbn-10-only.pdf")
  (define pdf-without-isbn "test-without-isbn.pdf")
  (define pdf-with-image-only "test-with-image-only.pdf")
  (define pdf-unreadable "test-damaged.pdf"))

(module+ test
  (check-equal? (current-pdf-max-page) 10)
  (parameterize ([current-pdf-max-page 5])
    (check-equal? (current-pdf-max-page) 5)))

(define current-pdf-max-page (make-parameter 10))

(module+ test
  (check-equal?
   (extract-isbn-from-pdf-abs for/or isbn-find #f pdf-with-isbn)
   "0262062186")
  (check-equal?
   (extract-isbn-from-pdf-abs for/list isbn-find/list '() pdf-with-isbn)
   (list (list "0262062186" "026256114X" "1593274912"
               "9781593274917" "0201896834" "9780201896831"
               "0262062186" "026256114X" "0262062186" "0201896834"
               "9780201896831" "026256114X" "9780201896831")
         (list))))

(define-syntax (extract-isbn-from-pdf-abs stx)
  (syntax-parse stx
    [(_ iter extract fallback doc)
     #'(with-handlers ([exn:fail? (lambda (e) fallback)])
         (iter ([pg# (in-list (pages-to-search (pdf-count-pages doc)))])
               (extract (page-text (pdf-page doc pg#)))))]))

(module+ test
  (check-exn exn:fail? (lambda () (extract-isbn-from-pdf "hi.pdf")))
  (check-equal? (extract-isbn-from-pdf/list pdf-unreadable) '())
  (check-equal? (extract-isbn-from-pdf/list pdf-with-image-only) '())
  (check-equal? (extract-isbn-from-pdf/list pdf-without-isbn) '())
  
  (check-equal?
   (extract-isbn-from-pdf/list pdf-with-isbn)
   (list "0262062186" "026256114X" "1593274912"
         "9781593274917" "0201896834" "9780201896831"
         "0262062186" "026256114X" "0262062186" "0201896834"
         "9780201896831" "026256114X" "9780201896831")))

(define (extract-isbn-from-pdf/list doc)
  (check-file-exists doc 'extract-isbn-from-pdf/list)
  (flatten (extract-isbn-from-pdf-abs for/list isbn-find/list '() doc)))

(module+ test
  (check-exn exn:fail? (lambda () (extract-isbn-from-pdf "hi.pdf")))
  (check-false (extract-isbn-from-pdf pdf-unreadable))
  (check-equal? (extract-isbn-from-pdf pdf-with-isbn) "0262062186")
  (check-equal? (extract-isbn-from-pdf pdf-with-isbn-10-only) "0262062186")
  (check-false (extract-isbn-from-pdf pdf-with-image-only))
  (check-false (extract-isbn-from-pdf pdf-without-isbn)))
      
(define (extract-isbn-from-pdf doc)
  (check-file-exists doc 'extract-isbn-from-pdf)
  (extract-isbn-from-pdf-abs for/or isbn-find #f doc))

(module+ test
  (check-equal? (pages-to-search 11) (set-add (range (current-pdf-max-page)) 10))
  (check-equal? (pages-to-search 10) (range (current-pdf-max-page)))
  (check-equal? (pages-to-search 5) (range 5)))

(define (pages-to-search n)
  (set-add (range (min n (current-pdf-max-page))) (sub1 n)))

(module+ test
  (check-exn exn:fail? (lambda () (check-file-exists "not-avail.pdf"))) 
  (check-pred file-exists? (check-file-exists pdf-with-isbn 'fun)))

(define (check-file-exists f src)
  (unless (file-exists? f)
    (error src "~s not found" f))
  f)

