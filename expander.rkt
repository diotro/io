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

(module+ test
  (require rackunit))

; ---------------------------------------------------------------------------------------------------
; Data Definitions

; An IoObj is a (obj [Maybe IoObj] [Hash Symbol IoMsg])
(struct obj [proto slots] #:transparent)

; A SlotValue is an [Either IoMsg Predef]

; A Predef is a (predef [ * -> IoObj ].
(struct predef [func])

; An IoVal is one of:
; - String (representing a literal string)
; - Number (representing a literal number)
; - Symbol (representing an identifier)
; - Predef (representing a predefined function)

; An IoMsg is a (msg IoVal [List IoVal])
(struct msg [val args] #:transparent)

; ---------------------------------------------------------------------------------------------------
; Expander
(define-macro (io-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))

(define-macro (io-program EXPRESSION ...)
  #'(begin
      (define LOBBY
        (obj #f
             (make-hash
              `((:= . ,(msg (predef (λ (eval k v) (hash-set! (obj-slots LOBBY) (msg-val k) (eval v)))) '()))
                (println . ,(msg (predef (λ (eval val) (writeln (eval val)))) '()))))))
      (for-each (λ (message-list) (receive* LOBBY message-list) (writeln LOBBY)) (list EXPRESSION ...))
      LOBBY))


; Evalutes to a (list SlotValue)
(define-macro (io-expression MESSAGE ...)
  ; We start out with the lobby as the receiver, and then receive messages in order
  #'(list MESSAGE ...))

; receive* : IoObj [List IoMsg] -> IoObj
(define (receive* receiver messages)
  (foldl receive receiver messages))

; receive : IoMsg IoObj -> IoObj
; Sends the given message to the object
(define (receive message receiver)
  (evaluate receiver message (msg-args message)))

; io-message transforms into an IoMessage
(define-macro (io-message VAL ARGS ...)
  #'(msg VAL (list ARGS ...)))

(define-macro (io-assignment NAME EXPR)
  #'(io-expression (msg ':= (cons (msg NAME '()) EXPR))))

; Could be functions!
(define-syntax-rule (io-identifier stx) (string->symbol stx))
(define-syntax-rule (io-number stx) (string->number stx))
(define-syntax-rule (io-string stx) stx)


; evaluate : IoObj IoMsg [List IoMsg] -> IoMsg
(define (evaluate obj message args)
  (match (msg-val message)
    [(? number? num) (msg num args)]
    [(? string? str) (msg str args)]
    [(? symbol? sym) (evaluate obj (hash-ref (obj-slots obj) sym) args)]
    [(? predef? fun)
     (msg (apply (predef-func fun) (cons (λ (m) (evaluate obj m args)) args)) '())]
    [else (error  (format "Invalid msg: ~a" message))]))

(module+ test
  ; Simplest case
  #;(io-program (io-expression (io-message (io-number "0"))))
  ; Expression with two messages
  #;(io-program (io-expression (io-message (io-number "0"))
                               (io-message (io-string ""))))
  ; Program with two expressions
  #;(io-program (io-expression (io-message (io-string "\"hello\"")))
                (io-expression (io-message (io-string "3"))))

  ; Message with args
  #;(io-program (io-expression
                 (io-message
                  (io-string "\"\"")
                  (io-expression (io-message (io-number "3"))))))

  ; assignment transformer
  (io-program (io-assignment
               (io-identifier "a")
               (io-expression (io-message (io-number "3"))))
              (io-expression (io-message (io-identifier "a"))
                             (io-message (io-identifier "println"))))
  )


