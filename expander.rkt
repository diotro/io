#lang br/quicklang

(provide (rename-out [io-module-begin #%module-begin])
         io-program
         io-expression
         io-message
         io-assignment
         io-identifier
         io-number 
         io-string
         )

(require "data.rkt")
(module+ test
  (require rackunit))


(define-macro (io-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))



(define-macro (io-program EXPRESSION ...)
  #'(begin
      (for-each (λ (message-list) (receive* (LOBBY) message-list)) (list EXPRESSION ...))))


; Evalutes to a (list SlotValue)
(define-macro (io-expression MESSAGE ...)
  ; We start out with the lobby as the receiver, and then receive messages in order
  #'(list MESSAGE ...))


; io-message transforms into an IoMessage
(define-macro (io-message VAL ARGS ...)
  #'(msg VAL (list ARGS ...)))

(define-macro (io-assignment NAME EXPR)
  #'(io-expression (msg ':= (cons (msg NAME '()) EXPR))))

; Could be functions!
(define-syntax-rule (io-identifier stx) (string->symbol stx))
(define-syntax-rule (io-number stx) (string->number stx))
(define-syntax-rule (io-string stx) stx)


(module+ test
  ; Simplest case
  (io-program (io-expression (io-message (io-number "0"))))
  ; Expression with two messages
  (io-program (io-expression (io-message (io-number "0"))
                             (io-message (io-string ""))))
  ; Program with two expressions
  (io-program (io-expression (io-message (io-string "\"hello\"")))
              (io-expression (io-message (io-string "3"))))

  ; Message with args
  (io-program (io-expression
               (io-message
                (io-string "\"\"")
                (io-expression (io-message (io-number "3"))
                               (io-message (io-identifier "println"))))))

  
  ; assignment transformer
  (io-program (io-assignment
               (io-identifier "a")
               (io-expression (io-message (io-number "3"))))
              (io-expression (io-message (io-identifier "a"))
                             (io-message (io-identifier "println"))))
  )


