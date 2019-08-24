#lang br
(provide color-io)

(require brag/support syntax-color/racket-lexer)

(define io-color-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [":=" (values lexeme 'hash-colon-keyword #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to "//" "\n") (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
   [(from/to "\"" "\"") (values lexeme 'string #f (pos lexeme-start) (pos lexeme-end))]
   [(:: (:? "-") (:+ numeric)) (values lexeme 'number #f (pos lexeme-start) (pos lexeme-end))]
   [any-char (values lexeme 'symbol #f (pos lexeme-start) (pos lexeme-end))]
   ))

(define (color-io port)
  (define-values (str cat paren start end) (io-color-lexer port))
  (values str cat paren start end))