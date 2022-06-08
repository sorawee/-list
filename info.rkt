#lang info
(define collection "list-plus")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/list-plus.scrbl" ())))
(define pkg-desc "A form that collects values into a list while supporting internal-definitions")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
