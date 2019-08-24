#lang brag

io-program : ( io-statement? /TERMINATOR )* ( io-statement )?
@io-statement : ( io-assignment | io-method-assignment | io-expression )?
io-assignment : io-identifier /ASSIGNMENT-OPERATOR io-expression

io-method-assignment : io-identifier io-identifier /ASSIGNMENT-OPERATOR io-method-declaration
io-method-declaration : /METHOD /OPEN-PAREN ( io-identifier "," )* io-statement /CLOSE-PAREN

io-expression : io-message-sending-expression | io-atomic-expression

io-message-sending-expression : io-identifier io-identifier [ io-message-args ]
io-message-args :  io-expression (","  io-expression )*

io-atomic-expression : io-identifier | io-number | io-string
io-identifier : SYMBOL
io-number : NUMBER
io-string : STRING