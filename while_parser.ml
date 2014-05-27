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
			    
# 58 "while_parser.ml"
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
    0|]

let yytransl_block = [|
  286 (* IDENT *);
  287 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\002\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\006\000\006\000\007\000\007\000\007\000\007\000\007\000\004\000\
\004\000\004\000\004\000\004\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\001\000\000\000\002\000\003\000\003\000\001\000\003\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\003\000\
\001\000\005\000\003\000\011\000\001\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\009\000\000\000\010\000\
\007\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\020\000\021\000\
\022\000\023\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\027\000\000\000\006\000\000\000\005\000\
\000\000\008\000\015\000\000\000\000\000\000\000\014\000\000\000\
\017\000\018\000\031\000\000\000\000\000\000\000\030\000\026\000\
\000\000\000\000\000\000\000\000\000\000\028\000"

let yydgoto = "\002\000\
\009\000\010\000\025\000\011\000\018\000\019\000\039\000\044\000"

let yysindex = "\014\000\
\085\255\000\000\005\255\000\000\090\255\090\255\085\255\252\254\
\000\000\023\000\009\255\008\255\090\255\000\000\096\255\000\000\
\000\000\121\255\069\255\069\255\021\255\096\255\000\000\085\255\
\000\000\014\255\116\255\101\255\096\255\013\255\000\000\000\000\
\000\000\000\000\000\000\096\255\096\255\096\255\096\255\090\255\
\090\255\079\255\000\000\000\000\049\255\000\000\031\255\000\000\
\096\255\000\000\000\000\255\254\013\255\013\255\000\000\031\255\
\000\000\000\000\000\000\050\255\099\255\247\254\000\000\000\000\
\090\255\004\255\096\255\038\255\099\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\069\000\000\000\
\000\000\000\000\000\000\000\000\029\000\057\000\000\000\042\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\000\000\240\255\251\255\252\255\000\000\236\255"

let yytablesize = 346
let yytable = "\045\000\
\011\000\020\000\043\000\043\000\021\000\003\000\002\000\027\000\
\028\000\030\000\050\000\065\000\040\000\041\000\001\000\012\000\
\047\000\036\000\037\000\038\000\022\000\048\000\023\000\052\000\
\067\000\036\000\037\000\038\000\012\000\024\000\053\000\054\000\
\055\000\056\000\046\000\057\000\058\000\026\000\049\000\060\000\
\064\000\038\000\016\000\062\000\043\000\016\000\016\000\016\000\
\070\000\069\000\016\000\016\000\043\000\016\000\016\000\061\000\
\013\000\036\000\037\000\038\000\066\000\068\000\016\000\063\000\
\036\000\037\000\038\000\000\000\024\000\003\000\000\000\016\000\
\004\000\005\000\006\000\000\000\000\000\040\000\041\000\003\000\
\000\000\042\000\004\000\005\000\006\000\003\000\000\000\000\000\
\004\000\005\000\006\000\007\000\059\000\000\000\000\000\000\000\
\000\000\007\000\008\000\003\000\013\000\000\000\004\000\005\000\
\006\000\000\000\029\000\000\000\008\000\040\000\041\000\042\000\
\051\000\000\000\008\000\014\000\000\000\015\000\000\000\016\000\
\017\000\014\000\000\000\015\000\000\000\016\000\017\000\050\000\
\008\000\000\000\031\000\032\000\033\000\034\000\000\000\031\000\
\032\000\033\000\034\000\000\000\035\000\000\000\036\000\037\000\
\038\000\035\000\000\000\036\000\037\000\038\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\011\000\011\000\011\000\011\000\
\000\000\011\000\011\000\000\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\003\000\002\000\011\000\000\000\000\000\
\000\000\011\000\000\000\011\000\011\000\012\000\011\000\000\000\
\012\000\012\000\012\000\012\000\000\000\012\000\012\000\000\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000\
\000\000\012\000\000\000\000\000\000\000\012\000\000\000\012\000\
\012\000\013\000\012\000\000\000\013\000\013\000\013\000\013\000\
\000\000\013\000\013\000\000\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\024\000\000\000\013\000\000\000\000\000\
\000\000\013\000\024\000\013\000\013\000\000\000\013\000\000\000\
\000\000\024\000"

let yycheck = "\020\000\
\000\000\006\000\019\000\020\000\007\000\000\000\000\000\013\000\
\013\000\015\000\012\001\021\001\009\001\010\001\001\000\011\001\
\022\000\027\001\028\001\029\001\025\001\024\000\000\000\029\000\
\021\001\027\001\028\001\029\001\000\000\021\001\036\000\037\000\
\038\000\039\000\014\001\040\000\041\000\030\001\025\001\042\000\
\061\000\029\001\001\001\049\000\061\000\004\001\005\001\006\001\
\069\000\012\001\009\001\010\001\069\000\012\001\013\001\007\001\
\000\000\027\001\028\001\029\001\065\000\067\000\021\001\014\001\
\027\001\028\001\029\001\255\255\000\000\001\001\255\255\030\001\
\004\001\005\001\006\001\255\255\255\255\009\001\010\001\001\001\
\255\255\013\001\004\001\005\001\006\001\001\001\255\255\255\255\
\004\001\005\001\006\001\013\001\014\001\255\255\255\255\255\255\
\255\255\013\001\030\001\001\001\011\001\255\255\004\001\005\001\
\006\001\255\255\011\001\255\255\030\001\009\001\010\001\013\001\
\012\001\255\255\030\001\026\001\255\255\028\001\255\255\030\001\
\031\001\026\001\255\255\028\001\255\255\030\001\031\001\012\001\
\030\001\255\255\015\001\016\001\017\001\018\001\255\255\015\001\
\016\001\017\001\018\001\255\255\025\001\255\255\027\001\028\001\
\029\001\025\001\255\255\027\001\028\001\029\001\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\014\001\014\001\021\001\255\255\255\255\
\255\255\025\001\255\255\027\001\028\001\001\001\030\001\255\255\
\004\001\005\001\006\001\007\001\255\255\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\021\001\255\255\255\255\255\255\025\001\255\255\027\001\
\028\001\001\001\030\001\255\255\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\007\001\255\255\021\001\255\255\255\255\
\255\255\025\001\014\001\027\001\028\001\255\255\030\001\255\255\
\255\255\021\001"

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
# 286 "while_parser.ml"
               :  Syntax.program ))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "while_parser.mly"
       ()
# 292 "while_parser.ml"
               : 'option_pvl))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "while_parser.mly"
       ()
# 298 "while_parser.ml"
               : 'option_pvl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'option_pvl) in
    Obj.repr(
# 47 "while_parser.mly"
                     ( _1 )
# 306 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 48 "while_parser.mly"
                     ( Syntax.Seq (_1,_3) )
# 314 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 49 "while_parser.mly"
                 (_2 )
# 321 "while_parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "while_parser.mly"
                  ( Syntax.Const _1 )
# 328 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "while_parser.mly"
                  ( _2 )
# 335 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "while_parser.mly"
                  ( Syntax.Unknown )
# 341 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "while_parser.mly"
                  ( Syntax.Var _1)
# 348 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Sub,Syntax.Const 0,_2) )
# 355 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Add,_1,_3) )
# 363 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Sub,_1,_3) )
# 371 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "while_parser.mly"
                  ( Syntax.Binop (Syntax.Mult,_1,_3) )
# 379 "while_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'test) in
    Obj.repr(
# 64 "while_parser.mly"
                  ( _2 )
# 386 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "while_parser.mly"
                  ( Syntax.Comp (_2,_1,_3) )
# 395 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'test) in
    Obj.repr(
# 66 "while_parser.mly"
                  ( Syntax.And (_1,_3) )
# 403 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'test) in
    Obj.repr(
# 67 "while_parser.mly"
                  ( Syntax.Or (_1,_3) )
# 411 "while_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "while_parser.mly"
          ( Syntax.Eq )
# 417 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "while_parser.mly"
       ( Syntax.Eq )
# 423 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "while_parser.mly"
       ( Syntax.Neq )
# 429 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "while_parser.mly"
       ( Syntax.Le )
# 435 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "while_parser.mly"
       ( Syntax.Lt )
# 441 "while_parser.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "while_parser.mly"
                            ( Syntax.Assign (new_pp (),_1,_3) )
# 449 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "while_parser.mly"
                            ( Syntax.Skip (new_pp ()))
# 455 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 82 "while_parser.mly"
                            ( Syntax.If (new_pp (),_2,_3,_5) )
# 464 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 84 "while_parser.mly"
                            ( Syntax.While (new_pp (),_2,_3) )
# 472 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'test) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _11 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 86 "while_parser.mly"
      (
	let init = Syntax.Assign (new_pp (),_3, _5) in
	let incr = Syntax.Assign (new_pp (),_3, _9) in	  
	let body = Syntax.While(new_pp (),_7,Syntax.Seq (incr,_11)) in	  
	  Syntax.Seq (init,body)
      )
# 488 "while_parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 95 "while_parser.mly"
                ( _1 )
# 495 "while_parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 96 "while_parser.mly"
                ( _2 )
# 502 "while_parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "while_parser.mly"
                ( Syntax.Skip (new_pp ()))
# 508 "while_parser.ml"
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
# 100 "while_parser.mly"

# 535 "while_parser.ml"
