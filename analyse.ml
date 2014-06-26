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

module Make =
  functor (AbEnv:Abstraction.Env) ->
struct
  
  module M = Map.Make(struct type t = label let compare = compare end)
    
  let get default m k =
    try
      M.find k m 
    with
	Not_found -> default

  let get_equation = get []

  let make_eq_sys (constraints : (label * (AbEnv.L.t -> AbEnv.L.t) * label) list) =
    List.fold_left
      (fun sys (l1,f,l2) -> M.add l2 ((l1,f)::(get_equation sys l2)) sys)
      M.empty
      constraints

  let get_abenv s l = get (AbEnv.L.bottom ()) s l
  let rec apply_eq s = function
    | [] -> AbEnv.L.bottom ()
    | [l,f] ->  (*Printf.printf "1: %d : %s --> %s\n" l 
      (AbEnv.L.to_string (get_abenv s l))
      (AbEnv.L.to_string (f (get_abenv s l)));*)
      f (get_abenv s l)
    | (l,f)::q -> 
      (*Printf.printf "2: %d : %s --> %s\n" l (AbEnv.L.to_string (get_abenv s l))
	(AbEnv.L.to_string (f (get_abenv s l)));*)
      (* + Should be at most 2 equations.
	 + One of the equations returns AbEnv.L.bottom 
	 thanks to the treatment of conditional branches by iter; each branch
	 only knows about the state s concerning the current path
       *)
      AbEnv.L.join (f (get_abenv s l)) (apply_eq s q)

  let modify s k f =
    M.add k (f (get_abenv s k)) s
      
  type strategy =
    | Single of label
    | Seq of strategy * strategy
    | Branch of label * test * strategy * strategy
    | Loop of label * strategy 

  let rec gen_strategy = function 
    | Syntax.Skip l -> Single l
    | Syntax.Assign (l,x,e) -> Single l
    | Syntax.If (l,t,b1,b2) -> (* Seq (Single l, Seq (gen_strategy b1,
				  gen_strategy b2)) *)
      Branch(l, t, gen_strategy b1, gen_strategy b2)
    | Syntax.Fi (l,t,l') -> Single l
    | Syntax.While (l,t,b) -> Loop (l, gen_strategy b)
    | Syntax.Seq (i1,i2) -> Seq (gen_strategy i1, gen_strategy i2)
    | Syntax.Inputh (l,_lvars) -> Single l (* TODO *)
    | Syntax.Inputl (l, _lvars) -> Single l (* TODO *)

  let rec entry  = function
    | Single l -> l
    | Seq (strat1,strat2) -> entry strat1
    | Branch (l,t,strat1,strat2) -> l
    | Loop (l, strat) -> l

  let strategy (p,l) = 
    Seq (gen_strategy p,Single l)

  let rec iter l0 sys strat s end_label  =
    match strat with
      | Single l -> 
	  let new_val = 
	    if l=l0 
	    (* No equation is mapped to l0. Do not apply_eq blindly *)
	    then AbEnv.L.join (AbEnv.init_env ()) (apply_eq s (get_equation sys l))
	    else 
	    apply_eq s (get_equation sys l)
	  in M.add l new_val s
      | Seq (strat1,strat2) ->
	iter l0 sys strat2 (iter l0 sys strat1 s (entry strat2)) end_label
      | Branch (l,t,strat1,strat2) ->
	(* compute "?the AbEnv" at the entrance of the conditional then add it
	   to the AbEnv map *)
	let s0 = iter l0 sys (Single l) s l in
	(* Extract the state at the entrance of the conditional then use it to
	   iter over both branches with a newly created map of AbEnvs *)
	let e_init = get_abenv s0 l in
	let s_init () = M.add l e_init (M.empty) in
	let s1 = iter l0 sys (Seq(strat1, Single end_label)) (s_init ()) end_label in
	let s1' = M.remove l s1 in
	let s2 = iter l0 sys (Seq(strat2, Single end_label)) (s_init () ) end_label in
	let s2' = M.remove l s2 in
	let merge k xo yo = (match xo,yo with
	  (* an invariant for both maps :
	     - for all keys k, there is a binding in m1 or exclusively in m2,
	     unless k is an immediate postdominator of a branch *)
	  | Some x, None -> Some x
	  | None, Some y -> Some y
	  | Some x, Some y when (k = end_label) ->
	    (* Here should go the abstract semantics of conditionals *)
	    Some (AbEnv.forward_if ~l:(Some l) e_init t x y)
	  | _ -> 
	    raise (Failure "Two states to merge that are not the immediate postdom!"))
	in
	M.merge merge s0 (M.merge merge s1' s2')
      | Loop (l,strat) ->
	let s' = iter  l0 sys (Single l) s end_label in
	let rec loop_widen s =
	  let s' = iter l0 sys (Seq (strat,Single l)) s end_label in
	  if AbEnv.L.order_dec (get_abenv s' l) (get_abenv s l) then 
	    iter l0 sys strat s end_label
	  else loop_widen (modify s' l (AbEnv.L.widen (get_abenv s l)))
	in
	let rec loop_narrow s =
	  let s' = iter l0 sys (Seq (strat,Single l)) s l in
	  if AbEnv.L.order_dec  (get_abenv s l) (get_abenv s' l) then 
	    iter l0 sys strat s end_label
	  else loop_narrow (modify s' l (AbEnv.L.narrow (get_abenv s l)))
	in
	loop_narrow (loop_widen s')

  let gen_constraints p =
    List.map
      (function (l1,i,l2) ->
	match i with (* Check if should pass l1 or l2?*)
	  | Cfg.Assign (x,e) -> (l1,AbEnv.assign ~l:(Some l1) x e,l2)
	  | Cfg.Assert t -> (l1,AbEnv.backward_test t,l2)
	  | Cfg.Fi (t,l) -> (l1, AbEnv.L.join (AbEnv.L.bottom () ), l2)
	  | Cfg.Input (lvars, e) -> 
	    let f abenv = 
	      List.fold_left 
		(fun accu x -> AbEnv.assign ~l:(Some l1) x e accu)
		abenv lvars
	    in (l1, f, l2)
	    
      )
      (Cfg.build p)

  let check p res =
    List.iter
      (function (l1,i,l2) ->
	 if
	   (match i with  (* Check if should pass l1 or l2?*)
	     | Cfg.Assign (x,e) -> AbEnv.L.order_dec (AbEnv.assign ~l:(Some l1) x e (res l1)) (res l2)
	      | Cfg.Assert t -> AbEnv.L.order_dec (AbEnv.backward_test t (res l1)) (res l2)
	      | Cfg.Fi (t,l) -> true (* TODO  *)
	      | Cfg.Input (lvars, e) -> 
		AbEnv.L.order_dec 
		  (List.fold_left 
		     (fun accu x ->AbEnv.assign ~l:(Some l1) x e accu)
		     (res l1) lvars)
		  (res l2)
	   )
	 then ()
	 else failwith (Printf.sprintf "wrong postfixpoint in edges (%d -> %d)\n" l1 l2))
      (Cfg.build p)

  let solve p =
    AbEnv.init (Syntax.vars p);
    let l0 = Cfg.entry (fst p) in
    let s_init = (*M.add l0 (AbEnv.init_env () )*) M.empty in
    let f = get_abenv
      (iter l0 (make_eq_sys (gen_constraints p)) (strategy p)
	 s_init  (snd p)) in
      check p f; f

  let solve_and_print p =
	let f = solve p in
      function l -> AbEnv.L.to_string (f l)


end
