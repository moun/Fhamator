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

(** Syntax of programs
*)
type label = int
type var = string
type binop = Add | Sub | Mult | Rem
type expr =
    Const of int
  | Unknown
  | Initl
  | Var of var
  | Binop of binop * expr * expr
  | T of test

and comp = Eq | Neq | Le | Lt
and test =
    Comp of comp * expr * expr
  | And of test * test
  | Or of test * test
type stmt =
    Assign of label * var * expr
  | Skip of label
  | If of label * test * stmt * stmt
  | Fi of label * test * label
  | While of label * test * stmt
  | Seq of stmt * stmt
  | Inputh of label * var list
  | Inputl of label * var list
type program = stmt * label
 
val vars : program -> var list
(** [vars p] returns of the list of variables present in the program [p] 
*)
