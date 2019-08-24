#lang br/quicklang
(module reader br
  (require "reader.rkt")
  (provide read-syntax get-info)
  
  (define (get-info port src-mod src-line src-col src-pos)
    (λ (key default)
      (case key
        [(color-lexer) (dynamic-require 'io/colorer 'color-io)]
        [else default])))
  )