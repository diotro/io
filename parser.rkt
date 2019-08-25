#lang brag

io-program : io-form?  ( /TERMINATOR io-form? )*
@io-form : io-expression | io-assignment

io-expression : io-message+ 
io-message : io-atom [ io-arguments ]
@io-arguments : /OPEN-PAREN [  io-message+ [ /COMMA  io-message+ ] ] /CLOSE-PAREN

io-assignment : io-identifier /ASSIGNMENT-OPERATOR io-expression

@io-atom : io-identifier | io-number | io-string
io-identifier : SYMBOL
io-number : NUMBER
io-string : STRING