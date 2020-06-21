#lang scribble/manual
@require[scribble/example
         @for-label[list-plus
                    racket/base]]

@(define evaluator (make-base-eval))
@(evaluator '(require list-plus (for-syntax racket/base)))

@title{list-plus}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[list-plus]

This library provides a form @racket[list+] which is like @racket[list], but allows @seclink["intdef-body" #:doc '(lib "scribblings/reference/reference.scrbl")]{internal definitions} inside it.

@defform[(list+ defn-or-expr ...)]{
  Supports a mixture of expressions and mutually recursive definitions.
  The result of the @racket[list+] form is a list of values produced by expressions
  in @racket[defn-or-expr].

  @examples[#:eval evaluator
    (define-syntax-rule (define-like x v) (define-values (x) v))
    (list+ 1
           (define x 2)
           (add1 x)
           (define-syntax (mac stx) #'5)
           (mac)
           (define-like y 7)
           y
           (begin -1 (code:comment @#,elem{Note that @racket[begin] is splicing.})
                  -2))
  ]
}
