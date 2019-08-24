#lang br/quicklang

(provide (rename-out [io-module-begin #%module-begin])
         io-program
         io-statement
         io-expression
         io-message-sending-expression
         io-assignment
         io-identifier
         io-literal
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

; An IoEnv is a [Map String IoValue]
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

(define-syntax (io-statement stx)
  (syntax-case stx ()
    [(_ expr) #'expr]
    ; If given no args, the statement is a no-op
    [_ #'#f]))


; io-expression : Message... -> IoStatement
(define-syntax io-expression
  ; Expression always wraps a specific type of expression, so we can just unwrap it
  ; and let the data in the expression handle becoming a function
  (syntax-rules () [(_ expression-value) (λ (env) (list env (evaluate env expression-value)))]))

; evaluate : IoEnv IoExpr -> IoValue
; evaluates the given expression in the environment
(define (evaluate env expr)
  (match expr
    [(io-literal value) value]
    [(io-identifier name) (hash-ref env name)]
    [(? procedure?) (second (expr env))]))

(define-struct io-literal [value])
(define-struct io-identifier [name])

; A terminator does nothing, obviously
(define-syntax-rule (io-terminator _) #f)

; io-assignment : IoIdentifier _ IoExpression -> IoStatements
; An assignment sets the value of the identifier to the result of the expression
(define-syntax-rule (io-assignment identifier _ expression)
  (λ (env) (list (hash-set env (io-identifier-name identifier) (evaluate env expression))
                 #f)))

(define (literal->string value)
  (cond [(string? value) value]
        [(number? value) (number->string value)]))

; io-message-sending-expression : IoIdentifier IoMessage... -> IoStatement
(define (io-message-sending-expression identifier . messages)
  (λ (env) (list env (evaluate env identifier))))


(module+ test
  (define-syntax-rule (check-program program expected) 
    (check-equal? (with-output-to-string (thunk program)) expected))


  (check-program (io-program (io-statement (io-expression (io-literal 3)))) "3\n")
  (check-program (io-program (io-statement (io-assignment (io-identifier "a") #f (io-literal 123)))
                             (io-statement (io-message-sending-expression (io-identifier "a"))))
                 "123\n")
  (check-program (io-program (io-statement)
                             (io-terminator #f)
                             (io-statement
                              (io-assignment
                               (io-identifier "a") #f (io-expression (io-literal "test"))))
                             (io-terminator #f)
                             (io-statement
                              (io-expression
                               (io-message-sending-expression (io-identifier "a"))))
                             (io-terminator #f))
                 "\"test\"\n")
  )














