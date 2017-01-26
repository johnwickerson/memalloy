(*
MIT License

Copyright (c) 2017 by John Wickerson.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Format

type access_type =
  | WriteRead
  | Write
  | Read
  | Atomic
  | Plain

type set_or_rel = Set | Rel

let pp_typ = function Set -> "Set" | Rel -> "Rel"

type unop =
  | Set_to_rln
  | Star
  | Plus
  | Opt
  | Inv
  | Comp of set_or_rel
  | Select of access_type * access_type
  | Domain
  | Range
			      
type binop =
  | Seq
  | Union
  | Diff
  | Inter
  | Cross

type cat_expr =
  | Empty_rln
  | Var of string
  | App of string * cat_expr list
  | Op1 of unop * cat_expr
  | Op of binop * cat_expr list

let rec pp_expr oc = function
  | Empty_rln -> fprintf oc "0"
  | Var x -> fprintf oc "%s" x
  | App (f,es) -> fprintf oc "%s(%a)" f pp_exprs es
  | Op1 (o,e) -> fprintf oc "unop(%a)" pp_expr e
  | Op (Seq,es) -> fprintf oc "Seq(%a)" pp_exprs es
  | Op (Union,es) -> fprintf oc "Union(%a)" pp_exprs es
  | Op (Inter,es) -> fprintf oc "Inter(%a)" pp_exprs es
  | Op (Diff,es) -> fprintf oc "Diff(%a)" pp_exprs es
  | Op (Cross,es) -> fprintf oc "Cross(%a)" pp_exprs es
			    
and pp_exprs oc = function
  | [e] -> pp_expr oc e
  | e :: es -> fprintf oc "%a," pp_expr e; pp_exprs oc es
  | _ -> assert false				 

type test_type =
  | Acyclic
  | Irreflexive
  | IsEmpty

let pp_test_type oc = function
  | Acyclic -> fprintf oc "acyclic"
  | Irreflexive -> fprintf oc "irreflexive"
  | IsEmpty -> fprintf oc "empty"
  
type cat_instr =
  | Let of string * string list * cat_expr
  | LetRec of (string * cat_expr) list
  | Test of test_type * cat_expr * string
  | Include of string

let rec pp_morebinds oc = function
  | [] -> fprintf oc ""
  | (x,e) :: xes ->
     fprintf oc "and %s = %a" x pp_expr e;
     pp_morebinds oc xes

let rec pp_varlist oc = function
  | [] -> ()
  | [x] -> fprintf oc "%s" x
  | x :: xs -> fprintf oc "%s," x; pp_varlist oc xs
		  
let pp_instr oc = function
  | Let (x,args,e) ->
     fprintf oc "let %s%a = %a\n\n" x pp_varlist args pp_expr e
  | LetRec ((x,e) :: xes) ->
     fprintf oc "let %s = %a\n\n" x pp_expr e;
     pp_morebinds oc xes
  | LetRec [] -> assert false
  | Test (t,e,n) ->
     fprintf oc "%a(%a) as %s\n\n" pp_test_type t pp_expr e n
  | Include path ->
     fprintf oc "include %s\n\n" path

let pp_instrs oc = List.iter (pp_instr oc)
	     
type cat_model = cat_instr list
