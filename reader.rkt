#lang racket

(provide (rename-out [literal-read-syntax read-syntax]))

(require "tokenizer.rkt" "parser.rkt")

(define (literal-read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum
    `(module io-module callisto/expander
       ,parse-tree))
  (datum->syntax #f module-datum))


(module+ test
  ; Honestly just hoping these don't error, not too worried about the output being correct yet
  (define (test-parse str)
    (literal-read-syntax "test" (open-input-string str)))

  (test-parse "a")
  (test-parse "3")
  (test-parse "\"hello\"")
  (test-parse "b := 3")
  (test-parse "b 3 4")
  (test-parse "b := 3\nb\nc:=\"hi\" ")
  )