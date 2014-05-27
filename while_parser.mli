type token =
  | FOR
  | ASSERT
  | ENSURE
  | SKIP
  | WHILE
  | IF
  | ELSE
  | NOT
  | AND
  | OR
  | EOF
  | LP
  | RP
  | LB
  | RB
  | EQ
  | NEQ
  | LE
  | LT
  | GE
  | GT
  | PVL
  | VL
  | PT
  | PTPT
  | ASSIGN
  | UNKNOWN
  | ADD
  | SUB
  | MULT
  | IDENT of (string)
  | INT of (int)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Syntax.program 
