#lang brag

io-program : ( io-statement io-terminator )*
io-statement : ( io-assignment | io-expression )?
io-assignment : io-identifier ASSIGNMENT-OPERATOR io-expression
io-expression : io-message-sending-expression | io-literal
io-message-sending-expression : io-identifier ( io-expression* )
io-message : io-identifier | io-literal
io-identifier : SYMBOL
io-literal : NUMBER | STRING
io-terminator : TERMINATOR