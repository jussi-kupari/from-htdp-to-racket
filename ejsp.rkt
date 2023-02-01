#lang racket/base

;; -----------------------------------------------------------------------------
; Convenient extensions to the json-pointer module

(require racket/contract)

(provide
 (contract-out
  ; produces the value in the given jsexpr pointed by the given json pointer
  ; produces #f if json-pointer-value raises an exception
  [json-pointer-value/false     (-> (or/c json-pointer? json-pointer-expression?)
                                    jsexpr?
                                    (or/c any/c #f))]
  
  ; produces the value in the given jsexpr pointed by the given json pointer
  ; produces empty list if json-pointer-value raises an exception
  [json-pointer-value/empty     (-> (or/c json-pointer? json-pointer-expression?)
                                    jsexpr?
                                    (or/c any/c '()))]
  
  ; produces the value in the given jsexpr pointed by the given json pointer
  ; produces empty string if json-pointer-value raises an exception
  [json-pointer-value/empty-str (-> (or/c json-pointer? json-pointer-expression?)
                                    jsexpr?
                                    (or/c any/c ""))]
  
  ; extracts from the given jsexpr all n values pointed by "/pre/n/post"
  [json-pointer-value/index     (-> json-pointer-expression?
                                    json-pointer-expression?
                                    jsexpr?
                                    (listof any/c))]))

;; -----------------------------------------------------------------------------

(require (only-in json jsexpr?)
         (only-in json-pointer
                  json-pointer?
                  json-pointer-expression?
                  json-pointer-value))

(module+ test
  (require rackunit))

(module+ test
  (check-equal?
   (json-pointer-value/fallback "/a" (hash 'a 1) #f)
   (json-pointer-value "/a" (hash 'a 1)))
  (check-false (json-pointer-value/fallback "/a" (hash 'b 2) #f))
  (check-equal? (json-pointer-value/fallback "/a" (hash 'b 1) "") ""))

(define (json-pointer-value/fallback jsp jse fallback)
  (with-handlers ([exn:fail? (lambda (e) fallback)])
    (json-pointer-value jsp jse)))

(module+ test
  (check-equal?
   (json-pointer-value/false "/a" (hash 'a 1))
   (json-pointer-value "/a" (hash 'a 1)))
  (check-false (json-pointer-value/false "/a" (hash 'b 2))))

(define (json-pointer-value/false jsp jse)
  (json-pointer-value/fallback jsp jse #f))

(module+ test
  (check-equal?
   (json-pointer-value/empty "/a" (hash 'a 1))
   (json-pointer-value "/a" (hash 'a 1)))
  (check-equal? (json-pointer-value/empty "/a" (hash 'b 2)) '()))

(define (json-pointer-value/empty jsp jse)
  (json-pointer-value/fallback jsp jse '()))

(module+ test
  (check-equal?
   (json-pointer-value/empty-str "/a" (hash 'a 1))
   (json-pointer-value "/a" (hash 'a 1)))
  (check-equal? (json-pointer-value/empty-str "/a" (hash 'b 2)) ""))

(define (json-pointer-value/empty-str jsp jse)
  (json-pointer-value/fallback jsp jse ""))

(module+ test
  ;jsexpr example
  (define jse-ex
    '#hasheq((a
              .
              #hasheq((b
                       .
                       (#hasheq((x . 1) (y . 2))
                        #hasheq((x . 3) (y . 4))))
                      (c . (#hasheq((y . 0))))))))
  
  (check-equal? (json-pointer-value/index '("a" "b") '("y") jse-ex) '(2 4))
  (check-equal? (json-pointer-value/index '("a" "c") '("y") jse-ex) '(0)))

(define (json-pointer-value/index pre post jse)
  (for*/list ([n (in-naturals)]
              [v (in-value
                  (json-pointer-value/false
                   (join-pointers pre `(,(number->string n)) post) jse))]
              #:break (not v))
    v))

(module+ test
  (check-equal? (join-pointers '("hi")) '("hi"))
  (check-equal? (join-pointers '("a") '("b") '("c")) '("a" "b" "c")))

(define (join-pointers . jsps) (apply append jsps))

