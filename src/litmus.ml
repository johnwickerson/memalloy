(*
MIT License

Copyright (c) 2017 by John Wickerson and Tyler Sorensen.

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

(** Datatype for litmus tests *)

open Format
open General_purpose

type attribute = string
type address = Reg of Register.t | Loc of Location.t

let pp_addr oc = function
  | Reg r -> Register.pp oc r
  | Loc l -> Location.pp oc l

(** A language of expressions for creating fake dependencies *)
type 'a expr =
  | Just of 'a
  | Madd of 'a expr * Register.t

let rec pp_expr k oc = function
  | Just x -> k oc x
  | Madd (e,r) -> fprintf oc "%a + 0*%a" (pp_expr k) e Register.pp r

(** [mk_expr b [r1,...,rn]] yields the expression [b + 0*r1 + ... + 0*rn] *)
let mk_expr b rs =
  List.fold_left (fun e r -> Madd(e,r)) (Just b) rs

(** Instruction in a litmus test *)
type instruction =
  | Load of Register.t * Location.t expr
  | Store of Location.t expr * Value.t expr
  | Cas of Location.t expr * Value.t * Value.t expr
  | Fence

(** Simple pretty-printing of instructions (for debugging *)
let pp_instr oc = function
  | Load (r,le), attrs -> 
     fprintf oc "%a := load(%a%a)"
	     Register.pp r (pp_expr Location.pp) le
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Store (le,ve), attrs ->
     fprintf oc "store(%a,%a%a)"
	     (pp_expr Location.pp) le (pp_expr Value.pp) ve
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Cas (le,v,ve), attrs ->
     fprintf oc "cas(%a,%a,%a%a)"
	     (pp_expr Location.pp) le Value.pp v (pp_expr Value.pp) ve
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Fence, attrs ->
     fprintf oc "fence(%a)"
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
      
(** A component is either a single instruction, a collection of components in sequence, a collection of components that are unsequenced, or an if-statement *)
type 'a component =
  | Basic of 'a
  | Seq of 'a component list
  | Unseq of 'a component list
  | If of Register.t * Value.t * 'a component

(** [map_component f c] applies [f] to each instruction in [c] *)
let rec map_component f = function
  | Basic x -> Basic (f x)
  | Seq cs -> Seq (List.map (map_component f) cs)
  | Unseq cs -> Unseq (List.map (map_component f) cs)
  | If (r,v,c) -> If (r,v, map_component f c)

(** Simple pretty-printing of components *)     
let rec pp_component k oc = function
  | Basic b -> k oc b     
  | Seq [c] | Unseq [c] -> (pp_component k) oc c
  | Seq cs -> fparen (fprintf_iter "; " (pp_component k)) oc cs
  | Unseq cs -> fparen (fprintf_iter " + " (pp_component k)) oc cs
  | If (r,v,c) ->
     fprintf oc "if (%a==%a) %a" Register.pp r Value.pp v
	     (pp_component k) c
		     
(** A litmus test comprises a list of locations, a list of threads, and a postcondition *)		     
type litmus_test = {
    locs: Location.t list;
    thds: (instruction * attribute list) component list;
    post: (address, Value.t) map;
  }

(** Simple pretty-printing of litmus tests *)	   
let pp oc lt =
  fprintf oc "Locations: %a.\n\n" (fprintf_iter ", " Location.pp) lt.locs;
  let pp_thd tid = function
    | Seq cs ->
       fprintf oc "Thread %d:\n" tid;
       fprintf_iter ";\n" (pp_component pp_instr) oc cs;
       fprintf oc ";\n\n";
       tid+1
    | _ -> assert false
  in
  let _ = List.fold_left pp_thd 0 lt.thds in
  fprintf oc "Final: ";
  let pp_cnstrnt oc (a,v) = fprintf oc "%a==%d" pp_addr a v in
  fprintf_iter " && " pp_cnstrnt oc lt.post
  
