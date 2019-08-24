#lang brag

io-program : io-statement (TERMINATOR io-statement)*
io-statement : (assignment | expression)?
assignment : identifier ASSIGNMENT-OPERATOR expression
expression : message+ 
message : identifier | literal
identifier : SYMBOL
literal : NUMBER | STRING