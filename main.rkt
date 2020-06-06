#lang racket/base

(provide list+)
(require syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     syntax/intdef
                     syntax/stx))

(define-syntax-parser list+
  [(_ body ...)
   (define gs (gensym))
   (define ctx (syntax-local-make-definition-context))
   (define (do-expand e)
     (syntax-parse (local-expand e
                                 (list gs)
                                 (list #'begin
                                       #'define-syntaxes
                                       #'define-values)
                                 ctx)
       #:literals (begin define-syntaxes define-values)
       [(begin body ...) (append-map do-expand (attribute body))]
       [(define-values (x ...) e)
        #:with (x* ...) (map syntax-local-identifier-as-binding
                             (attribute x))
        (syntax-local-bind-syntaxes (attribute x) #f ctx)
        (list #'(define-values (x* ...) e))]
       [(define-syntaxes (x ...) e)
        #:with (x* ...) (map syntax-local-identifier-as-binding
                             (attribute x))
        (syntax-local-bind-syntaxes (attribute x) #'e ctx)
        (list #'(define-syntaxes (x* ...) e))]
       [e (list #'(define-values ()
                    (begin (set! acc (cons e acc))
                           (values))))]))

   (define-values (def def-stx)
     (partition (syntax-parser
                  [({~literal define-values} . _) #t]
                  [_ #f])
                (append-map do-expand (attribute body))))

   (internal-definition-context-track
    ctx
    #`(let ([acc '()])
        (letrec-syntaxes+values
            #,(map stx-cdr def-stx)
            #,(map stx-cdr def)
          (reverse acc))))])

(module+ test
  (require rackunit)
  (check-equal? (list+) '())
  (check-equal? (list+ 1 2 3) '(1 2 3))
  (check-equal? (let ([x 1]) (list+ x 2 3))
                '(1 2 3))
  (check-equal? (list+ 1
                       (define x 2)
                       (add1 x)
                       5)
                '(1 3 5))
  (check-equal? (list+ 1
                       (define-syntax (x stx) #'2)
                       (add1 x)
                       5)
                '(1 3 5))
  (check-equal? (let ([x 0])
                  (list+ (let () (set! x (+ x 1)) x)
                         (let () (set! x (+ x 10)) x)
                         (define y (let () (set! x (+ x 100)) x))
                         (define z (let () (set! x (+ x 1000)) x))
                         (let () (set! x (+ x 10000)) x)
                         (let () (set! x (+ x 100000)) x)
                         y
                         z))
                '(1 11 11111 111111 111 1111))
  (check-equal? (let ([x 1])
                  (list (list+ (define x 2)
                               x)
                        x))
                '((2) 1))
  (check-equal? (list+ (define (f x)
                         (g (add1 x)))
                       (define (g x)
                         (* x 2))
                       (f 10))
                '(22))
  (define-syntax-rule (define2 x v) (define-values (x) v))
  (check-equal? (list+ (define2 x 10)
                       x)
                '(10)))
