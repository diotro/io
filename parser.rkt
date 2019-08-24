#lang brag

program : expr *
expr       : [ (WHITESPACE)* ]  message [ (WHITESPACE)* ] | TERMINATOR
message    : SYMBOL [ arguments ] | NUMBER | STRING
arguments  : "(" expr  ( "," expr )* ")"
