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

open Format
open General_purpose

(*****************************)
(* Datatype for litmus tests *)
(*****************************)

type attribute = string
type address = Reg of Register.t | Loc of Location.t

let pp_addr oc = function
  | Reg r -> Register.pp oc r
  | Loc l -> Location.pp oc l
       
type 'a with_fake_deps = 'a * Register.t list
		   
type instruction =
  | Load of Register.t * Location.t with_fake_deps
  | Store of Location.t with_fake_deps * Value.t with_fake_deps
  | Cas of Location.t with_fake_deps * Value.t * Value.t with_fake_deps
  | Fence
		   
type 'a component =
  | Basic of 'a
  | Seq of 'a component list
  | Unseq of 'a component list
  | If of Register.t * Value.t * 'a component
		       
type litmus_test = {
    locs: Location.t list;
    thds: (instruction * attribute list) component list;
    post: (address, Value.t) map;
  }

let pp_instr oc = function
  | Load (r,(l,_)), attrs -> 
     fprintf oc "%a := load(%a%a)" Register.pp r Location.pp l
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Store ((l,_),(v,_)), attrs ->
     fprintf oc "store(%a,%d%a)" Location.pp l v
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Cas ((l,_),v,(v',_)), attrs ->
     fprintf oc "cas(%a,%d,%d%a)" Location.pp l v v'
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
  | Fence, attrs ->
     fprintf oc "fence(%a)"
	     (fprintf_iter "" (fun oc -> fprintf oc ",%s")) attrs
	     
let rec pp_component k oc = function
  | Basic b -> k oc b     
  | Seq [c] | Unseq [c] -> (pp_component k) oc c
  | Seq cs -> fparen (fprintf_iter "; " (pp_component k)) oc cs
  | Unseq cs -> fparen (fprintf_iter " + " (pp_component k)) oc cs
  | If (r,v,c) ->
     fprintf oc "if (%a==%d) %a" Register.pp r v (pp_component k) c
		     
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
  
