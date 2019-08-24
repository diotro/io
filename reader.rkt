#lang racket

(provide (rename-out [literal-read-syntax read-syntax]))

(require "tokenizer.rkt" "parser.rkt")

(define (literal-read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (pretty-write (syntax->datum parse-tree))
  (define module-datum
    `(module io-module io/expander
       ,parse-tree
       ))
  (datum->syntax #f module-datum))


(module+ test
  ; Honestly just hoping these don't error, not too worried about the output being correct yet
  (define (test-parse str)
    (syntax->datum (literal-read-syntax "test" (open-input-string (string-append str ";")))))

  (test-parse "a")
  (test-parse "3")
  (test-parse "\"hello\"")
  (test-parse "b := 3")
  (test-parse "b 3 4")
  (test-parse "b := 3\nb\n c := \"hi\" ")

  (test-parse ";")
  )