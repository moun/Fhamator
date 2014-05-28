# 21 "while_lexer.mll"
 
  open Lexing  (* il n'est ainsi pas necessaire de prefixer par "Lexing." 
                  la fonction lexeme *) 
  open While_parser  (* pour connaitre le type token *)

  exception Stop

  (*      table des mots cl�s     *)
  let kwd_tab = Hashtbl.create 23 

  let _ = (* initialisation de la table de hash *)
    Hashtbl.add kwd_tab "while" WHILE;
    Hashtbl.add kwd_tab "for" FOR;
    Hashtbl.add kwd_tab "skip" SKIP;
    Hashtbl.add kwd_tab "assert" ASSERT;
    Hashtbl.add kwd_tab "ensure" ENSURE;
    Hashtbl.add kwd_tab "if" IF;
    Hashtbl.add kwd_tab "else" ELSE;
    Hashtbl.add kwd_tab "not" NOT;
    Hashtbl.add kwd_tab "and" AND;
    Hashtbl.add kwd_tab "or" OR

  let id_or_kwd s = (* cherche le token associ� � s ou renvoie IDENT s *)  
    try Hashtbl.find kwd_tab s 
    with Not_found -> IDENT(s) 

  (* pour g�rer les commentaires imbriqu�s *)
  let level = ref 0

  let currentline = ref 1

# 34 "while_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\228\255\078\000\017\000\231\255\232\255\233\255\234\255\
    \237\255\238\255\239\255\240\255\030\000\031\000\033\000\247\255\
    \249\255\250\255\090\000\192\000\027\000\053\000\254\255\002\000\
    \253\255\252\255\013\001\246\255\244\255\245\255\242\255\230\255\
    \109\000\253\255\054\000\083\000\255\255\254\255\028\000\254\255\
    \255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\014\000\012\000\020\000\255\255\
    \255\255\255\255\004\000\004\000\255\255\007\000\255\255\000\000\
    \255\255\255\255\004\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\002\000\002\000\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\020\000\255\255\000\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \033\000\000\000\255\255\255\255\000\000\000\000\039\000\000\000\
    \000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\022\000\023\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\000\000\023\000\020\000\000\000\025\000\040\000\000\000\
    \021\000\015\000\008\000\010\000\007\000\009\000\005\000\003\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\004\000\011\000\013\000\014\000\012\000\006\000\
    \031\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\030\000\028\000\029\000\027\000\024\000\
    \037\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\019\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\017\000\036\000\016\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \018\000\000\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\000\000\034\000\000\000\035\000\
    \000\000\000\000\000\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\000\000\000\000\000\000\
    \000\000\018\000\000\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\255\255\255\255\000\000\000\000\018\000\
    \000\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\026\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \000\000\000\000\000\000\000\000\018\000\255\255\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\023\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\023\000\000\000\255\255\020\000\038\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\013\000\013\000\014\000\021\000\
    \034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\035\000\000\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \018\000\255\255\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\255\255\032\000\255\255\032\000\
    \255\255\255\255\255\255\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\018\000\255\255\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\019\000\255\255\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\020\000\038\000\255\255\255\255\019\000\
    \255\255\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\026\000\255\255\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \255\255\255\255\255\255\255\255\026\000\032\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec nexttoken lexbuf =
    __ocaml_lex_nexttoken_rec lexbuf 0
and __ocaml_lex_nexttoken_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 62 "while_lexer.mll"
            ( nexttoken lexbuf )
# 212 "while_lexer.ml"

  | 1 ->
# 63 "while_lexer.mll"
            ( incr currentline; nexttoken lexbuf )
# 217 "while_lexer.ml"

  | 2 ->
# 64 "while_lexer.mll"
            ( level := 1; comment lexbuf )
# 222 "while_lexer.ml"

  | 3 ->
# 65 "while_lexer.mll"
                      (nexttoken  lexbuf)
# 227 "while_lexer.ml"

  | 4 ->
# 67 "while_lexer.mll"
            ( id_or_kwd (lexeme lexbuf) )
# 232 "while_lexer.ml"

  | 5 ->
# 68 "while_lexer.mll"
            ( LB )
# 237 "while_lexer.ml"

  | 6 ->
# 69 "while_lexer.mll"
            ( RB )
# 242 "while_lexer.ml"

  | 7 ->
# 70 "while_lexer.mll"
            ( LP )
# 247 "while_lexer.ml"

  | 8 ->
# 71 "while_lexer.mll"
            ( RP )
# 252 "while_lexer.ml"

  | 9 ->
# 72 "while_lexer.mll"
             ( EQ )
# 257 "while_lexer.ml"

  | 10 ->
# 73 "while_lexer.mll"
            ( NEQ )
# 262 "while_lexer.ml"

  | 11 ->
# 74 "while_lexer.mll"
            ( LE )
# 267 "while_lexer.ml"

  | 12 ->
# 75 "while_lexer.mll"
            ( LT )
# 272 "while_lexer.ml"

  | 13 ->
# 76 "while_lexer.mll"
            ( GE )
# 277 "while_lexer.ml"

  | 14 ->
# 77 "while_lexer.mll"
            ( GT )
# 282 "while_lexer.ml"

  | 15 ->
# 78 "while_lexer.mll"
            ( PVL )
# 287 "while_lexer.ml"

  | 16 ->
# 79 "while_lexer.mll"
            ( ADD )
# 292 "while_lexer.ml"

  | 17 ->
# 80 "while_lexer.mll"
            ( SUB )
# 297 "while_lexer.ml"

  | 18 ->
# 81 "while_lexer.mll"
            ( MULT )
# 302 "while_lexer.ml"

  | 19 ->
# 82 "while_lexer.mll"
            ( MOD )
# 307 "while_lexer.ml"

  | 20 ->
# 83 "while_lexer.mll"
            ( ASSIGN )
# 312 "while_lexer.ml"

  | 21 ->
# 84 "while_lexer.mll"
            ( VL )
# 317 "while_lexer.ml"

  | 22 ->
# 85 "while_lexer.mll"
            ( UNKNOWN )
# 322 "while_lexer.ml"

  | 23 ->
# 86 "while_lexer.mll"
            ( PT )
# 327 "while_lexer.ml"

  | 24 ->
# 87 "while_lexer.mll"
            ( PTPT )
# 332 "while_lexer.ml"

  | 25 ->
# 88 "while_lexer.mll"
            ( comment' lexbuf )
# 337 "while_lexer.ml"

  | 26 ->
# 89 "while_lexer.mll"
            ( INT (int_of_string (lexeme lexbuf)) )
# 342 "while_lexer.ml"

  | 27 ->
# 90 "while_lexer.mll"
            ( EOF )
# 347 "while_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_nexttoken_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 32
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "while_lexer.mll"
            ( level := !level - 1; 
              if !level = 0 then nexttoken lexbuf 
	      else comment lexbuf )
# 360 "while_lexer.ml"

  | 1 ->
# 96 "while_lexer.mll"
            ( level := !level + 1; comment lexbuf )
# 365 "while_lexer.ml"

  | 2 ->
# 97 "while_lexer.mll"
            ( comment lexbuf )
# 370 "while_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and comment' lexbuf =
    __ocaml_lex_comment'_rec lexbuf 38
and __ocaml_lex_comment'_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 100 "while_lexer.mll"
            ( incr currentline; nexttoken lexbuf )
# 381 "while_lexer.ml"

  | 1 ->
# 101 "while_lexer.mll"
            ( comment' lexbuf )
# 386 "while_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment'_rec lexbuf __ocaml_lex_state

;;

# 103 "while_lexer.mll"
 
  (* pour faire des test sur le lexer *)
  let list_token file =
    if not (Sys.file_exists file) then
      failwith  ("le fichier "^file^" n'existe pas\n")
    else
      let buf = from_channel (open_in file) in
      let rec aux () =
	try
	  match (nexttoken buf) with
	      EOF -> []
	    | t -> t::(aux ())
	  with Failure _ -> 
	    print_string "aucun moyen de filtrer le token courant !"; []
      in aux ()


# 410 "while_lexer.ml"
