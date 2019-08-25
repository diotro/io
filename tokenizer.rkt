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
  (lexer-srcloc
   ["(" (token 'OPEN-PAREN)]
   [")" (token 'CLOSE-PAREN)]
   ["," (token 'COMMA)]
   ["method" (token 'METHOD)]
   [":=" (token 'ASSIGNMENT-OPERATOR)]
   [(:+ (:or " " "\t")) (token 'WHITESPACE lexeme #:skip? #t)]
   [(:or "\n" ";") (token 'TERMINATOR)]
   [(from/to "\"" "\"") (token 'STRING lexeme)]
   [(:: (:? "-") (:+ numeric)) (token 'NUMBER lexeme)]
   [(:: alphabetic (:* (:or alphabetic numeric))) (token 'SYMBOL lexeme)]
   [(from/stop-before "//" "\n") (token 'COMMENT lexeme #:skip? #t)]
   ))

(module+ test
  
  (define-syntax (check-token stx)
    (syntax-case stx ()
      [(_ string type content)
       #`(begin
           (define tok (io-lexer (open-input-string string)))
           #,(syntax/loc stx (check-equal? (token-struct-type (srcloc-token-token tok)) type))
           #,(syntax/loc stx (check-equal? (token-struct-val (srcloc-token-token tok)) content)))]))
  
  (check-token "asdf" 'SYMBOL "asdf")
  (check-token "\"str\"" 'STRING "\"str\"")
  (check-token "1234" 'NUMBER "1234")
  
  (check-token " " 'WHITESPACE " ")
  (check-token "  " 'WHITESPACE "  ")

  (check-token "//asdf \n3" 'COMMENT "//asdf ")
  )