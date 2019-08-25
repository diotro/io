#lang racket
(provide LOBBY
         (struct-out obj)
         slot-ref
         slot-ref-index
         msg
         receive*)

; An IoObj is a (obj [Maybe IoObj] [Hash Symbol IoExpr])
(struct obj [proto slots] #:transparent)

; An IoExpr is an [Either IoObj IoVal Predef]

; An IoMsg is an IoObj where 'val leads to a IoVal and 'args leads to a [List IoExpr]


; An IoVal is a
; - String (representing a literal string)
; - Number (representing a literal number)
; - Symbol (representing an identifier)

; A Predef is a [ * -> IoObj ]



; msg : IoVal [List IoExpr] -> IoObj
(define (msg val args)
  (obj MSG-PROTO (make-hash `((val .,val) (args .,args)))))

; slot-ref : IoObj Symbol -> IoObj
; retrieves the value in the slot of the given object
(define (slot-ref obj slot-name)
  #;(displayln (list obj slot-name))
  (if (hash-has-key? (obj-slots obj) slot-name)
      (hash-ref (obj-slots obj) slot-name)
      (if (obj-proto obj)
          (slot-ref (obj-proto obj) slot-name)
          (error (format "Could not find slot ~a" slot-name)))))

; slot-ref-index : IoObj Symbol Int -> I
(define (slot-ref-index obj key index)
  (list-ref (slot-ref obj 'args) index))



; receive* : IoObj [List IoMsg] -> IoObj
(define (receive* receiver messages)
  (foldl (λ (m r) (evaluate r m)) receiver messages))

; evaluate : Obj IoExpr -> IoMsg
(define (evaluate obj expr)
  (match (slot-ref expr 'val)
    [(? obj? obj) obj]
    [(? string? str) (msg str '())]
    [(? number? num) (msg num '())]
    [(? symbol? sym) (evaluate obj (msg (slot-ref obj sym) (slot-ref expr 'args)))]
    [(? procedure? func) (msg (apply func (cons obj (slot-ref expr 'args))) '())]))
    
  
(define OBJ-PROTO
  (obj #f
       (make-hash
        `((:= . ,(λ (this . args)
                   (define val (evaluate this (second args)))
                   (hash-set! (obj-slots this) (slot-ref (first args) 'val) val)
                   val))
          (println . ,(λ (this . args) (writeln ((slot-ref this 'to-string) this))))))))

(define MSG-PROTO (obj OBJ-PROTO
                       (make-hash `((to-string . ,(λ (this . args) (slot-ref this 'val)))))))

(define LOBBY (make-parameter
               (obj OBJ-PROTO
                    (make-hash
                     `((:= . ,(λ (this . args)
                                (define val (evaluate this (second args)))
                                (hash-set! (obj-slots this) (slot-ref (first args) 'val) val)
                                val))
                       (println . ,(λ (this . args) (writeln this #;((slot-ref this 'to-string) this)))))))))

