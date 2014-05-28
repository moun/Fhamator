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
  | MOD
  | IDENT of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 2 "while_parser.mly"
  
  let token_var = ref 0
  let token_pp = ref 0

  let find var_tab s = 
    try Hashtbl.find var_tab s 
    with Not_found -> 
      Hashtbl.add var_tab s !token_var;
      let p = !token_var in
	incr token_var;
	p
	  
  let new_pp () = 
    let p = !token_pp in
      incr token_pp;
      p

  let vars_used var_tab = Hashtbl.fold (fun s p l -> p::l) var_tab []
			    
# 59 "while_parser.ml"
let yytransl_const = [|
  257 (* FOR *);
  258 (* ASSERT *);
  259 (* ENSURE *);
  260 (* SKIP *);
  261 (* WHILE *);
  262 (* IF *);
  263 (* ELSE *);
  264 (* NOT *);
  265 (* AND *);
  266 (* OR *);
    0 (* EOF *);
  267 (* LP *);
  268 (* RP *);
  269 (* LB *);
  270 (* RB *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LE *);
  274 (* LT *);
  275 (* GE *);
  276 (* GT *);
  277 (* PVL *);
  278 (* VL *);
  279 (* PT *);
  280 (* PTPT *);
  281 (* ASSIGN *);
  282 (* UNKNOWN *);
  283 (* ADD *);
  284 (* SUB *);
  285 (* MULT *);
  286 (* MOD *);
    0|]

let yytransl_block = [|
  287 (* IDENT *);
  288 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\002\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\006\000\007\000\007\000\007\000\007\000\007\000\
\004\000\004\000\004\000\004\000\004\000\008\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\000\000\002\000\003\000\003\000\001\000\003\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\005\000\003\000\011\000\001\000\003\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\000\000\000\000\009\000\000\000\010\000\
\007\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\021\000\022\000\
\023\000\024\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\028\000\000\000\006\000\000\000\
\005\000\000\000\008\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\019\000\032\000\000\000\000\000\000\000\
\031\000\027\000\000\000\000\000\000\000\000\000\000\000\029\000"

let yydgoto = "\002\000\
\009\000\010\000\025\000\011\000\018\000\019\000\040\000\045\000"

let yysindex = "\014\000\
\077\255\000\000\007\255\000\000\073\255\073\255\077\255\000\255\
\000\000\026\000\006\255\005\255\073\255\000\000\084\255\000\000\
\000\000\132\255\066\255\066\255\034\255\084\255\000\000\077\255\
\000\000\019\255\105\255\030\255\084\255\240\254\000\000\000\000\
\000\000\000\000\000\000\084\255\084\255\084\255\084\255\084\255\
\073\255\073\255\015\255\000\000\000\000\042\255\000\000\124\255\
\000\000\084\255\000\000\000\000\112\255\240\254\240\254\020\255\
\124\255\124\255\000\000\000\000\000\000\038\255\087\255\137\255\
\000\000\000\000\073\255\002\255\084\255\116\255\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\000\
\000\000\000\000\000\000\000\000\000\000\058\000\086\000\001\000\
\114\000\056\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\000\000\240\255\251\255\252\255\000\000\236\255"

let yytablesize = 403
let yytable = "\046\000\
\014\000\020\000\044\000\044\000\021\000\003\000\002\000\027\000\
\028\000\030\000\041\000\042\000\038\000\039\000\001\000\003\000\
\048\000\012\000\004\000\005\000\006\000\049\000\069\000\053\000\
\022\000\023\000\024\000\007\000\061\000\011\000\054\000\055\000\
\056\000\057\000\058\000\026\000\059\000\060\000\041\000\042\000\
\062\000\052\000\066\000\050\000\064\000\008\000\044\000\047\000\
\063\000\039\000\072\000\065\000\000\000\000\000\044\000\000\000\
\017\000\012\000\000\000\017\000\017\000\017\000\068\000\070\000\
\017\000\017\000\003\000\017\000\017\000\004\000\005\000\006\000\
\000\000\000\000\041\000\042\000\017\000\003\000\043\000\000\000\
\004\000\005\000\006\000\013\000\000\000\013\000\017\000\003\000\
\000\000\007\000\004\000\005\000\006\000\000\000\029\000\000\000\
\008\000\000\000\014\000\043\000\015\000\000\000\000\000\016\000\
\017\000\000\000\000\000\008\000\000\000\014\000\000\000\015\000\
\000\000\015\000\016\000\017\000\051\000\008\000\000\000\031\000\
\032\000\033\000\034\000\051\000\000\000\025\000\000\000\071\000\
\000\000\035\000\000\000\036\000\037\000\038\000\039\000\000\000\
\000\000\000\000\036\000\037\000\038\000\039\000\036\000\037\000\
\038\000\039\000\031\000\032\000\033\000\034\000\036\000\037\000\
\038\000\039\000\000\000\000\000\035\000\067\000\036\000\037\000\
\038\000\039\000\000\000\036\000\037\000\038\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\000\000\000\000\014\000\014\000\014\000\014\000\
\000\000\014\000\014\000\000\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\003\000\002\000\014\000\000\000\000\000\
\000\000\014\000\000\000\014\000\014\000\014\000\011\000\014\000\
\000\000\011\000\011\000\011\000\011\000\000\000\011\000\011\000\
\000\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\000\000\000\000\011\000\000\000\000\000\000\000\011\000\000\000\
\011\000\011\000\012\000\000\000\011\000\012\000\012\000\012\000\
\012\000\000\000\012\000\012\000\000\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\000\000\000\000\012\000\000\000\
\000\000\000\000\012\000\000\000\012\000\012\000\013\000\000\000\
\012\000\013\000\013\000\013\000\013\000\000\000\013\000\013\000\
\000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\000\000\000\000\013\000\000\000\000\000\000\000\013\000\000\000\
\013\000\013\000\015\000\000\000\013\000\015\000\015\000\015\000\
\015\000\000\000\015\000\015\000\000\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\025\000\000\000\015\000\000\000\
\000\000\000\000\015\000\025\000\000\000\000\000\000\000\000\000\
\015\000\000\000\025\000"

let yycheck = "\020\000\
\000\000\006\000\019\000\020\000\007\000\000\000\000\000\013\000\
\013\000\015\000\009\001\010\001\029\001\030\001\001\000\001\001\
\022\000\011\001\004\001\005\001\006\001\024\000\021\001\029\000\
\025\001\000\000\021\001\013\001\014\001\000\000\036\000\037\000\
\038\000\039\000\040\000\031\001\041\000\042\000\009\001\010\001\
\043\000\012\001\063\000\025\001\050\000\031\001\063\000\014\001\
\007\001\030\001\071\000\014\001\255\255\255\255\071\000\255\255\
\001\001\000\000\255\255\004\001\005\001\006\001\067\000\069\000\
\009\001\010\001\001\001\012\001\013\001\004\001\005\001\006\001\
\255\255\255\255\009\001\010\001\021\001\001\001\013\001\255\255\
\004\001\005\001\006\001\011\001\255\255\000\000\031\001\001\001\
\255\255\013\001\004\001\005\001\006\001\255\255\011\001\255\255\
\031\001\255\255\026\001\013\001\028\001\255\255\255\255\031\001\
\032\001\255\255\255\255\031\001\255\255\026\001\255\255\028\001\
\255\255\000\000\031\001\032\001\012\001\031\001\255\255\015\001\
\016\001\017\001\018\001\012\001\255\255\000\000\255\255\012\001\
\255\255\025\001\255\255\027\001\028\001\029\001\030\001\255\255\
\255\255\255\255\027\001\028\001\029\001\030\001\027\001\028\001\
\029\001\030\001\015\001\016\001\017\001\018\001\027\001\028\001\
\029\001\030\001\255\255\255\255\025\001\021\001\027\001\028\001\
\029\001\030\001\255\255\027\001\028\001\029\001\030\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\014\001\014\001\021\001\255\255\255\255\
\255\255\025\001\255\255\027\001\028\001\029\001\001\001\031\001\
\255\255\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\021\001\255\255\255\255\255\255\025\001\255\255\
\027\001\028\001\001\001\255\255\031\001\004\001\005\001\006\001\
\007\001\255\255\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\255\255\
\255\255\255\255\025\001\255\255\027\001\028\001\001\001\255\255\
\031\001\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\021\001\255\255\255\255\255\255\025\001\255\255\
\027\001\028\001\001\001\255\255\031\001\004\001\005\001\006\001\
\007\001\255\255\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\007\001\255\255\021\001\255\255\
\255\255\255\255\025\001\014\001\255\255\255\255\255\255\255\255\
\031\001\255\255\021\001"

let yynames_const = "\
  FOR\000\
  ASSERT\000\
  ENSURE\000\
  SKIP\000\
  WHILE\000\
  IF\000\
  ELSE\000\
  NOT\000\
  AND\000\
  OR\000\
  EOF\000\
  LP\000\
  RP\000\
  LB\000\
  RB\000\
  EQ\000\
  NEQ\000\
  LE\000\
  LT\000\
  GE\000\
  GT\000\
  PVL\000\
  VL\000\
  PT\000\
  PTPT\000\
  ASSIGN\000\
  UNKNOWN\000\
  ADD\000\
  SUB\000\
  MULT\000\
  MOD\000\
  "

let yynames_block = "\
  IDENT\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 38 "while_parser.mly"
              ( _1, new_pp() )
# 305 "while_parser.ml"
               :  Syntax.program ))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "while_parser.mly"
       ()
# 311 "while_parser.ml"
               : 'option_pvl))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "while_parser.mly"
       ()
# 317 "while_parser.ml"
               : 'option_pvl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'option_pvl) in
    Obj.repr(
# 47 "while_parser.mly"
                     ( _1 )
# 325 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 48 "while_parser.mly"
                     ( Syntax.Seq (_1,_3) )
# 333 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 49 "while_parser.mly"
                 (_2 )
# 340 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "while_parser.mly"
                  ( Syntax.Const _1 )
# 347 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "while_parser.mly"
                  ( _2 )
# 354 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "while_parser.mly"
                  ( Syntax.Unknown )
# 360 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "while_parser.mly"
                  ( Syntax.Var _1)
# 367 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Sub,Syntax.Const 0,_2) )
# 374 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Add,_1,_3) )
# 382 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Sub,_1,_3) )
# 390 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Mult,_1,_3) )
# 398 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Mult,_1,_3) )
# 406 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'test) in
    Obj.repr(
# 65 "while_parser.mly"
                  ( _2 )
# 413 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "while_parser.mly"
                  ( Syntax.Comp (_2,_1,_3) )
# 422 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'test) in
    Obj.repr(
# 67 "while_parser.mly"
                  ( Syntax.And (_1,_3) )
# 430 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'test) in
    Obj.repr(
# 68 "while_parser.mly"
                  ( Syntax.Or (_1,_3) )
# 438 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "while_parser.mly"
          ( Syntax.Eq )
# 444 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "while_parser.mly"
       ( Syntax.Eq )
# 450 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "while_parser.mly"
       ( Syntax.Neq )
# 456 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "while_parser.mly"
       ( Syntax.Le )
# 462 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "while_parser.mly"
       ( Syntax.Lt )
# 468 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "while_parser.mly"
                            ( Syntax.Assign (new_pp (),_1,_3) )
# 476 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "while_parser.mly"
                            ( Syntax.Skip (new_pp ()))
# 482 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 83 "while_parser.mly"
                            ( Syntax.If (new_pp (),_2,_3,_5) )
# 491 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 85 "while_parser.mly"
                            ( Syntax.While (new_pp (),_2,_3) )
# 499 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'test) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _11 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 87 "while_parser.mly"
      (
	let init = Syntax.Assign (new_pp (),_3, _5) in
	let incr = Syntax.Assign (new_pp (),_3, _9) in	  
	let body = Syntax.While(new_pp (),_7,Syntax.Seq (incr,_11)) in	  
	  Syntax.Seq (init,body)
      )
# 515 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 96 "while_parser.mly"
                ( _1 )
# 522 "while_parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 97 "while_parser.mly"
                ( _2 )
# 529 "while_parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "while_parser.mly"
                ( Syntax.Skip (new_pp ()))
# 535 "while_parser.ml"
               : 'block))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.program )
;;
# 101 "while_parser.mly"

# 562 "while_parser.ml"
