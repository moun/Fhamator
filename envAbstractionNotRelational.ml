(*
 *  This file is part of WhileAnalyser
 *  Copyright (c)2005-2008 INRIA Rennes - Bretagne Atlantique
 *  David Pichardie <first.last@irisa.fr>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Syntax

let reduction = ref false

module Make = 
  functor (AbNum:Abstraction.Num) ->
struct

  module L = EnvLattice.Make(AbNum.L)

  let init = L.init
  
  let init_env = L.top
    
  let contains_bot env =
    L.M.fold (fun _ ab b -> AbNum.L.order_dec ab (AbNum.L.bottom ()) || b) env false

  let reduce env = 
    if !reduction && contains_bot env then L.bottom ()
    else env

  let rec forward_expr ~l env = function
    | Const n -> AbNum.const ~l n
    | Unknown -> AbNum.inith ~l 
    | Initl -> AbNum.initl ~l 
    | Var x -> AbNum.forward_binop ~l Add (L.get env x) (AbNum.const ~l  0) (* TODO *)
    | Binop (op, e1, e2) ->
      AbNum.forward_binop ~l op (forward_expr ~l env e1) (forward_expr ~l env
  e2)
    | T test ->
      (match test with
	| Comp (c, e1, e2) ->
	  AbNum.forward_comp ~l c 
	    (forward_expr ~l env e1) 
	    (forward_expr ~l env e2)
	| Or (t1, t2) -> AbNum.L.top () (* todo *)
	| And (t1, t2) -> AbNum.L.top () (* todo *)
      )

  let assign ~l x e env =
    reduce (L.update env x (forward_expr ~l env e))
     
  let forward_if ~l env_guard t labels env1 env2 =
    let abval_t = forward_expr ~l  env_guard (T t) in
    (*(match l with
      | Some l ->
	Printf.printf "at label %d, %s \n" l (AbNum.L.to_string abval_t)
      | None ->
	Printf.printf "at label unknown, %s \n" (AbNum.L.to_string abval_t));*)
    try
      L.M.mapi
	(fun key v -> AbNum.forward_if ~l abval_t labels  v (L.M.find key env2))
	env1
    with Not_found -> assert false

  let forward_loop ~l env_guard t labels env_final =
    let abval_t = forward_expr ~l env_guard (T t) in
    try
      L.M.mapi
	(fun key v -> AbNum.forward_loop ~l abval_t labels v )
	env_final
    with Not_found -> assert false
   
  let rec backward_expr env e n =
    match e with
      | Const n0 ->
        (if AbNum.L.order_dec (AbNum.L.meet (AbNum.const ~l:None n0) n) (AbNum.L.bottom ()) 
	   then L.bottom ()
	   else env)
      | Unknown -> env
      | Initl -> env
      | Var x -> L.update env x (AbNum.L.meet n (L.get env x))
      | Binop (op, e1, e2) ->
          let (n1, n2) = 
	    AbNum.backward_binop op n (forward_expr ~l:None env e1)
	      (forward_expr ~l:None env e2)
          in
            L.meet (backward_expr env e1 n1) (backward_expr env e2 n2)
      | T t -> backward_test t env
		   
  and backward_test t env =
    reduce (
      match t with
	| Comp (c, e1, e2) ->
          let (n1, n2) = AbNum.backward_comp c (forward_expr ~l:None env e1)
	    (forward_expr ~l:None env e2) in
              L.meet (backward_expr env e1 n1) (backward_expr env e2 n2)
	| And (t1, t2) -> L.meet (backward_test t1 env) (backward_test t2 env)
	| Or (t1, t2) -> L.join (backward_test t1 env) (backward_test t2 env)
    )

end


