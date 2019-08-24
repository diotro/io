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

; An IoAssignment is a (assn [IoEnv -> IoEnv])
(struct assn [eval])

; An IoExpression is a (expr [IoEnv -> IoValue])
(struct expr [eval])

; IoExpr IoEnv -> IoValue
(define (eval expr env)
  ((expr-eval expr) env))

; [Either IoAssignment IoExpression] -> IoStatement
; Converts the assignment or expression into a statement
(define (assn-or-expr-to-statement val)
  (cond [(assn? val) (λ (env) (list ((assn-eval val) env) #f))]
        [(expr? val) (λ (env) (list env ((expr-eval val) env)))]
        [else (λ (env) (list env #f))]))

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
         (match-define (list new-env print-output) ((assn-or-expr-to-statement func) env))
         (when print-output
           (displayln print-output))
         new-env))))

(define-syntax (io-statement stx)
  (syntax-case stx ()
    [(_ expr) #'expr]
    ; If given no args, the statement is a no-op
    [_ #'#f]))


; io-expression : <io-expression in parser> -> IoExpression
(define-syntax-rule (io-expression expression) expression)

(define-syntax-rule (io-literal value) value)
(define-syntax-rule (io-identifier name) (string->symbol name))

; A terminator does nothing, obviously
(define-syntax-rule (io-terminator _) #f)

; io-assignment : IoIdentifier _ IoExpression -> IoAssignment
; An assignment sets the value of the identifier to the result of the expression
(define-syntax-rule (io-assignment identifier _ expression)
  (assn (λ (env) (hash-set env identifier (eval expression env)))))

; io-message-sending-expression : IoIdentifier IoMessage... -> IoStatement
(define (io-message-sending-expression identifier . messages)
  (expr (λ (env) (hash-ref env identifier))))

(define (io-literal-expression literal)
  (match literal
    [(? number? n) (expr (λ (env) n))]
    [(? string? s) (expr (λ (env) s))]
    [(? symbol? ident) (expr (λ (env) (hash-ref env ident)))]))

(module+ test
  (define-syntax-rule (check-program program expected) 
    (check-equal? (with-output-to-string (thunk program)) expected))
  
  )














