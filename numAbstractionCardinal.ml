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


module Make =
struct

  
  type label = Syntax.label
  module Label_Set = Set.Make 
    (struct 
      type t = label
      let compare = compare
     end)

  type ppset = 
  | Top
  | Set of Label_Set.t

  type cardinal = Z.t

  let cardinal_top = Z.succ (Z.sub (Z.of_int max_int) (Z.of_int min_int))

  let ppset_l = function
    | None -> Set Label_Set.empty
    | Some l -> Set (Label_Set.add l Label_Set.empty)

  module L =
  struct


    type t = ppset * cardinal
  
    let order_dec x y =
      let order_dec_labels ppsetx ppsety =
	match ppsetx with 
	| Top -> ppsety = Top
	| Set sx -> 
	  (match ppsety with
	  | Top -> false
	  | Set sy -> Label_Set.subset sx sy)
      in
      let order_dec_cardinals x y = Z.leq x y in
      match x with
      | ppsetx, cardinalx ->
	(match y with
	| ppsety, cardinaly ->
	  (order_dec_labels ppsetx ppsety)
	  && (order_dec_cardinals cardinalx cardinaly))
     
	
    let join x y =
      let join_labels ppsetx ppsety =
	match ppsetx with
	| Top -> Top
	| Set sx ->
	  (match ppsety with
	  | Top -> Top
	  | Set sy -> Set (Label_Set.union sx sy))
      in
      let join_cardinals cardx cardy = Z.max cardx cardy 
	(*let sum = (Z.add cardx cardy) in (*  TODO : this is not right! *)
	if (Z.gt sum cardinal_top) then
	  cardinal_top
	else
	  sum *)
      in (* This is the transfer function for conditionals *)
      match x,y with
	| (ppsetx,cardx),(ppsety,cardy) -> 
	  join_labels ppsetx ppsety,
	  join_cardinals cardx cardy
	  
	
    let meet x y =
      let meet_labels ppsetx ppsety =
	match ppsetx with
	| Top -> ppsety
	| Set sx ->
	  (match ppsety with
	  | Top -> ppsetx
	  | Set sy -> Set (Label_Set.inter sx sy))
      in
      let meet_cardinals cardx cardy =
	Z.min cardx cardy
      in
      match x,y with
      | (ppsetx, cardx), (ppsety,cardy) ->
	meet_labels ppsetx ppsety,
	meet_cardinals cardx cardy
    (* TODO : be quiet wary! the meet over sets of labels can be
      treacherous... e.g: conditional tests reduction! *)
	
    let widen = join
    let narrow x y = x (* TODO : Revet *)
      
    let bottom () = (Set Label_Set.empty, Z.zero)
      
    let top () = Top, cardinal_top

    let is_bottom = function
      | Top, _ -> false
      | Set s, card -> (Label_Set.is_empty s) && (Z.equal Z.zero card)

    
      
    let to_string = function
      | ppset, card ->
	let label_set_to_string s =
	  (match s with
	  | Top -> "Top"
	  | Set s ->
	    let start = ref true in
	    Label_Set.fold
	      (fun elt accu -> 
		let string_elt = 
		  if (!start) then
		    (start := false;
		    Printf.sprintf "%d" elt) (* %d : hmmmm!! TODO  *)
		  else
		     Printf.sprintf ", %d" elt
		in 
		accu ^ string_elt) s "")
	in
	Printf.sprintf "({ %s }, %s)" 
	  (label_set_to_string ppset) 
	  (Z.to_string card) 
	
  end 
    
  let backward_eq n1 n2 =
    ((L.meet n1 n2), (L.meet n1 n2))
      
  let backward_neq n1 n2 =
    (n1, n2)
   (* Only thing can say is if one of n1, n2 has only one value?!
     TODO !*)

  let backward_lt n1 n2 =
    (n1, n2)

  let backward_le n1 n2 =
    (n1, n2)

  let forward_binop_cardinal ~l n1 n2 =
    let label = ppset_l l in
    match n1 with
    | _, c1 ->
      (match n2 with
      | _, c2 ->
	let mult = (Z.mul c1 c2) in
	if (Z.gt mult cardinal_top) then
	  label,cardinal_top
	else
	  label,mult)

  let forward_add  =
    forward_binop_cardinal 
  
  let forward_sub =
    forward_binop_cardinal
  
  let forward_mult =
    forward_binop_cardinal

  let forward_rem ~l  n1 n2 = 
    let label = ppset_l l in
    match n1 with 
      | _, c1 ->
	(match n2 with
	  | _, c2 ->
	    label, c1) (* Need to refine by reduced prod interval analysis *)
    
  
  let backward_add n n1 n2 =
    n1, n2 (* TODO *)
  
  let backward_sub n n1 n2 =
   n1, n2
  
  let backward_mult n n1 n2 =
   n1, n2
    
  let backward_rem n n1 n2 =
    n1, n2 (* TODO *)
      

  let const ~l n =
    let label = ppset_l l in
    label, Z.one

  let backward_comp = function
    | Syntax.Eq -> backward_eq
    | Syntax.Neq -> backward_neq
    | Syntax.Lt -> backward_lt
    | Syntax.Le -> backward_le

  let forward_binop ~l op n1 n2  = match op with 
    | Syntax.Add -> forward_add ~l n1 n2
    | Syntax.Sub -> forward_sub ~l n1 n2
    | Syntax.Mult -> forward_mult ~l n1 n2
    | Syntax.Rem -> forward_rem ~l n1 n2

  let backward_binop = function
    | Syntax.Add -> backward_add
    | Syntax.Sub -> backward_sub
    | Syntax.Mult -> backward_mult
    | Syntax.Rem -> backward_rem

end
