#lang racket
(provide make-tokenizer)

(require brag/support)
(module+ test
  (require rackunit))


; Port -> [ -> Token]
; Creates a tokenizer that reads tokens from the given port
(define (make-tokenizer port)
  (thunk (io-lexer port)))

; Port -> Token
(define io-lexer
  (lexer
   [(:+ (:or " " "\t")) (token 'WHITESPACE lexeme #:skip? #t)]
   [(:or "\n" ";") (token 'TERMINATOR)]
   [":=" (token 'ASSIGNMENT-OPERATOR)]
   [(:: "\"" (:& (complement "\"") any-string) "\"") (token 'STRING lexeme)]
   [(:: (:? "-") (:+ numeric)) (token 'NUMBER lexeme)]
   [(:+ alphabetic) (token 'SYMBOL lexeme)]
   [(from/stop-before "//" "\n") (token 'COMMENT #:skip? #t)]))

(module+ test
  ; Reads the first token out of a string
  (define (read-token string)
    (io-lexer (open-input-string string)))

  (define symbol-token (read-token "asdf"))
  (check-equal? (token-struct-type symbol-token) 'SYMBOL)
  (check-equal? (token-struct-val symbol-token) "asdf")

  (define string-token (read-token "\"str\""))
  (check-equal? (token-struct-type string-token) 'STRING)
  (check-equal? (token-struct-val string-token) "\"str\"")

  (define number-token (read-token "1234"))
  (check-equal? (token-struct-type number-token) 'NUMBER)
  (check-equal? (token-struct-val number-token) "1234")

  (define white-space-token (read-token " "))
  (check-equal? (token-struct-type white-space-token) 'WHITESPACE)
  (define white-space-token2 (read-token "   \n    "))
  (check-equal? (token-struct-type white-space-token2) 'WHITESPACE)
  )