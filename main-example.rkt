;; =============================================================================
; Simple client for using book-info.rkt

#lang racket

(require "pdf-isbn.rkt"
         "book-info.rkt"
         "provider-ol.rkt")

; ----------------------------------------------------------
#;
(define my-books
  ; put here a list of the paths of your pdf books like this
  '("/home/luis/Ebooks/informit/Fowler_Refactoring.pdf"
    "/home/luis/Ebooks/informit/Gamma_Design-Patterns.pdf"
    "/home/luis/Ebooks/informit/Sedgewick_Algorithms_4e.pdf"
    "/home/luis/Ebooks/springer/Blythe_Funology2.pdf")
  )

; ----------------------------------------------------------

; File [Provider ol] -> [Maybe BookInfo]
; produces bibligraphic information for the given pdf file
; if the pdf doesn't have an isbn it produces #f
(define (get-book-info-from-pdf pdf #:provider [provider ol])
  (define isbn (extract-isbn-from-pdf pdf))
  (match isbn
    [#f #f]
    [_ (get-book-info isbn provider)]))

; [Maybe BookInfo] -> String
; produces a bibliographic entry from given bi
(define (book-info->biblio bi)
  (define (join l)
    (foldr string-append "" (add-between l "; ")))

  (match bi
    [#f ""]
    [(hash-table ('authors as) ('date d) ('title t) ('place pl) ('publisher pb))
     (string-append (join as) " "
                    (string-append "(" d ")") " "
                    (string-append " " t ".") " "
                    pl ": "
                    pb)]))

; Dir -> [Listof BookInfo]
; produces book info of pdfs in given dir
(define (get-book-info-from-dir dir #:provider [provider ol])
  (for/list ([f (in-directory dir)]
             #:when (equal? (filename-extension f) #"pdf"))
    (let* ([pdf (path->string f)]
           [bki (get-book-info-from-pdf pdf #:provider provider)])
      bki)))

#;
(for ([pdf (in-list my-books)])
  (displayln (book-info->biblio (get-book-info-from-pdf pdf))))

#|
RESULT for some of my books:

Martin Fowler (June 28, 1999)  Refactoring. : Addison-Wesley Professional
Erich Gamma; Richard Helm; Ralph Johnson; John Vlissides (January 15, 1995)  Design Patterns. : Addison-Wesley Professional
Robert Sedgewick (2011)  Algorithms. Upper Saddle River, NJ: Addison-Wesley
|#
    
