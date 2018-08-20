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

open! Format
open! General_purpose

type attribute = string

type address = Reg of Register.t | Loc of MyLocation.t

let pp_addr oc = function
  | Reg r -> Register.pp_full oc r
  | Loc l -> MyLocation.pp oc l

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

let is_fake_dependence_expr = function
  | Just _ -> false
  | Madd (_,_) -> true

type txn_outcome =
  | TxnAbort
  | TxnCommit

let pp_txn_outcome oc = function
  | TxnAbort -> fprintf oc "Abort"
  | TxnCommit -> fprintf oc "Commit"

(** Instruction in a litmus test *)
type instruction =
  | Load of Register.t * MyLocation.t expr
  | Store of MyLocation.t expr * Value.t expr
  | Cas of MyLocation.t expr * Value.t * Value.t expr
  | Fence
  | TxnBegin
  | TxnEnd of txn_outcome

(** Simple pretty-printing of instructions (for debugging *)
let pp_instr oc = function
  | Load (r,le), attrs -> 
     fprintf oc "%a := load(%a%a)"
	     Register.pp r (pp_expr MyLocation.pp) le
	     (MyList.pp_gen "" (fun oc -> fprintf oc ",%s")) attrs
  | Store (le,ve), attrs ->
     fprintf oc "store(%a,%a%a)"
	     (pp_expr MyLocation.pp) le (pp_expr Value.pp) ve
	     (MyList.pp_gen "" (fun oc -> fprintf oc ",%s")) attrs
  | Cas (le,v,ve), attrs ->
     fprintf oc "cas(%a,%a,%a%a)"
	     (pp_expr MyLocation.pp) le Value.pp v (pp_expr Value.pp) ve
	     (MyList.pp_gen "" (fun oc -> fprintf oc ",%s")) attrs
  | Fence, attrs ->
     fprintf oc "fence(%a)"
	     (MyList.pp_gen "" (fun oc -> fprintf oc ",%s")) attrs
  | TxnBegin, _ -> fprintf oc "txn_begin"
  | TxnEnd outcome, _ -> fprintf oc "txn_end %a" pp_txn_outcome outcome
      
(** A component is either a single instruction or an if-statement *)
type 'a component =
  | Basic of 'a
  | If of Register.t * Value.t * 'a component list

(** [map_component f c] applies [f] to each instruction in [c] *)
let rec map_component f = function
  | Basic x -> f x
  | If (r,v,cs) -> [If (r, v, map_components f cs)]
and map_components f cs = List.concat (List.map (map_component f) cs)

(** Simple pretty-printing of components *)     
let rec pp_component k oc = function
  | Basic b -> k oc b
  | If (r,v,cs) ->
     fprintf oc "if (%a==%a) { %a }"
       Register.pp r Value.pp v (pp_components k) cs
and pp_components k = MyList.pp_gen "; " (pp_component k)
		     
(** A litmus test comprises a list of locations, a list of threads, and a postcondition *)		     
type t = {
    locs: MyLocation.t list;
    thds: (instruction * attribute list) component list list;
    post: (address, Value.t) Assoc.t;
  }

(** Simple pretty-printing of litmus tests *)	   
let pp oc lt =
  fprintf oc "Locations: %a.\n\n"
    (MyList.pp_gen ", " MyLocation.pp) lt.locs;
  let pp_thd tid cs =
    fprintf oc "Thread %d:\n" tid;
    MyList.pp_gen ";\n" (pp_component pp_instr) oc cs;
    fprintf oc ";\n\n";
    tid+1
  in
  let _ = List.fold_left pp_thd 0 lt.thds in
  fprintf oc "Final: ";
  let pp_cnstrnt oc (a,v) = fprintf oc "%a==%d" pp_addr a v in
  MyList.pp_gen " && " pp_cnstrnt oc lt.post
    
