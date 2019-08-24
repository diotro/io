#lang br/quicklang

(provide (rename-out [io-module-begin #%module-begin])
         io-program
         io-statement
         io-expression
         io-literal
         io-terminator)

(define-macro (io-module-begin PARSE-TREE)
   #'(#%module-begin PARSE-TREE (displayln "Evluated Io")))


; An IoStatement is either an assignment or an expression, both of which take as input
; the environment and produce a new environment and possibly print,
; so:
; An IoStatement is a [IoEnv -> (list IoEnv [Maybe String])]

; An IoEnv is a [Map String IoValue]
; mapping identifiers to the corresponding values

; An IoValue is one of:
; - Int
; - String
; (going to be extended as I implement more of the language, but for now only two literal types
; and no compound types)


(define-macro (io-program STATEMENT ...)
  #`(begin
      ; The initial environment contains no data.
      ; This will have to be revisited to add Object and built-ins.
      (define initial-env (make-hash))

      ; Fold, starting from the initial environment, applying each statement
      ; (recall that statements are functions from environment to environment + string).
      (void
       (for/fold ([env initial-env])
                 ([func (list STATEMENT ...)])
         (match-define (list new-env print-output) (func env))
         (when print-output
           (println print-output))
         new-env))))


(define-syntax (io-statement stx)
  (syntax-case stx ()
    ; If given no args, the statement is a no-op
    [(_ expr) #'expr]
    ; But if given one arg, then we should use it as a function
    [_ #'(λ (env) (list env #f))]))


; expression : Message... -> [ IoEnv -> (list IoEnv [Maybe String]) ]
(define-syntax io-expression
  ; Expression always wraps a specific type of expression, so we can just unwrap it
  ; and let the data in the expression handle becoming a function
  (syntax-rules () [(_ expression-value) expression-value]))

; A literal expression returns itself
(define-syntax-rule (io-literal value) (λ (env) (list env value)))

; An identifier evaluates to it's value in the context, or an error
(define-syntax-rule (io-identifier ident) (λ (env) (list env (hash-ref env ident))))

; A terminator does nothing, obviously
(define-syntax-rule (io-terminator terminator) (λ (env) (list env #f)))

(define (literal->string value)
  (cond [(string? value) value]
        [(number? value) (number->string value)]))

(define (message-sending-expression identifier . messages)
  #f)




































