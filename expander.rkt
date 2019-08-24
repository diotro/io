#lang br/quicklang

(provide (rename-out [io-module-begin #%module-begin])
         io-program
         io-statement
         io-expression
         io-message-sending-expression
         io-assignment
         io-identifier
         io-literal
         io-literal-expression
         io-terminator)

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

; An IoEnv is a [Map Symbol IoValue]
; mapping identifiers to the corresponding values

; An IoValue is one of:
; - Int
; - String
; (going to be extended as I implement more of the language, but for now only two literal types
; and no compound types)


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
         (when print-output
           (displayln print-output))
         new-env))))

; io-statement : produces an IoStatement
(define-syntax (io-statement stx)
  (syntax-case stx ()
    ; If there is a statement, it is handled at a lower level and already a valid IoStatement here
    [(_ statement) #'statement]
    ; If given no args, the statement is a no-op. This represents an empty statement, like `;`
    [_ #'#f]))


; io-expression : IoExpression -> IoStatement
; transforms the expression into a statement
(define (io-expression expression)
  (λ (env) (list env (expression env))))


; io-message-sending-expression : IoIdentifier IoMessage... -> IoExpression
; TODO only evaluates the identifier, doesn't pass it messages
(define (io-message-sending-expression identifier . messages)
  (λ (env) (hash-ref env identifier)))


; io-literal-expression : literal -> IoExpression
; converts a literal (either an Identifier or a number/string) into an Expression
(define (io-literal-expression literal)
  (match literal
    [(? number? n) (λ (env) n)]
    [(? string? s) (λ (env) s)]
    [(? symbol? ident) (λ (env) (hash-ref env ident))]))

; io-assignment : IoIdentifier _ IoExpression -> IoAssignment
; An assignment sets the value of the identifier to the result of the expression
(define-syntax-rule (io-assignment identifier _ expression)
  (λ (env) (list (hash-set env identifier (second (expression env))) #f)))


(define-syntax-rule (io-literal value) value)
(define-syntax-rule (io-identifier name) (string->symbol name))

; A terminator does nothing, obviously
(define-syntax-rule (io-terminator _) #f)



(module+ test
  (define-syntax-rule (check-program program expected) 
    (check-equal? (with-output-to-string (thunk program)) expected))
  
  )














