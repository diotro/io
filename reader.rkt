#lang racket

(provide (rename-out [literal-read-syntax read-syntax]))

(require "tokenizer.rkt" "parser.rkt")

(define (literal-read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (println parse-tree)
  #;(define module-datum `(module callisto-module callisto/expander
                          ,parse-tree))
  #;(datum->syntax #f module-datum))


(literal-read-syntax "test" (open-input-string "asdrr; foo"))