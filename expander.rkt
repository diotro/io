#lang br/quicklang

(provide (rename-out [io-module-begin #%module-begin])
         io-program
         io-expression
         io-message-sending-expression
         io-assignment
         io-identifier
         io-number
         io-string
         io-atomic-expression
         io-method-assignment
         io-method-declaration
         )

(module+ test
  (require rackunit))

(define-macro (io-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))


; An IoStatement is either an assignment or an expression, both of which take as input
; the environment and produce a new environment and possibly print,
; so:
; An IoStatement is a [IoEnv -> (list IoEnv [Maybe String])]

; An IoAssignment is a [IoEnv -> IoEnv]

; An IoExpression is a [IoEnv -> IoValue]

; An IoEnv is a [Map Symbol IoObject]
; mapping identifiers to the corresponding values

; An IoObject is a [ Map Symbol IoSlotValue ]
; mapping slot names to slow values


; An IoSlotValue is a [??? -> ???]

(define-macro (io-program STATEMENT ...)
  #'(begin
      ; The initial environment contains no data.
      ; This will have to be revisited to add Object and built-ins.
      (define initial-env (hash))

      ; Fold, starting from the initial environment, applying each statement
      ; (recall that statements are functions from environment to environment + string).
      (void
       (for/fold ([env initial-env])
                 ([func (list STATEMENT ...)]
                  #:when func)
         (match-define (list new-env print-output) (func env))
         ;(displayln new-env)
         ;(displayln print-output)
         (when print-output
           (displayln (hash-ref print-output 'value)))
         new-env))))

; io-expression : IoExpression -> IoStatement
; transforms the expression into a statement
(define (io-expression expression)
  (λ (env) (list env (expression env))))


; io-message-sending-expression : IoIdentifier IoMessage... -> IoStatement
(define (io-message-sending-expression receiver message-name)
 (λ (env)
   (second ((hash-ref (hash-ref env receiver) message-name) env))))


; io-literal-expression : literal -> IoExpression
; converts a literal (either an Identifier or a number/string) into an Expression
(define (io-atomic-expression literal)
  (match literal
    [(? number? n) (λ (env) (hash 'value n))]
    [(? string? s) (λ (env) (hash 'value s))]
    [(? symbol? ident) (λ (env) (hash-ref env ident))]))

; io-assignment : IoIdentifier IoExpression -> IoStatement
; An assignment sets the value of the identifier to the result of the expression
(define-syntax-rule (io-assignment identifier expression)
  (λ (env) (list (hash-set env identifier (second (expression env)))
                 #f)))


; io-method-assignment : IoIdentifier IoIdentifer IoMethodDecl -> IoStatement
(define-syntax-rule (io-method-assignment object-name slot method)
  (λ (env) (list (hash-update env object-name
                              (λ (object) (hash-set object slot method)))
                 #f)))

; -> IoMethodDecl
; TODO args
(define-syntax-rule (io-method-declaration identifiers ... body)
  (λ (env) (body env)))

(define-syntax-rule (io-string value) value)
(define-syntax-rule (io-number value) value)
(define-syntax-rule (io-identifier name) (string->symbol name))
















