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

(****************************************) 
(* Datatype for representing executions *)
(****************************************)

type event = string
       
type execution = {
    sets : (string * event list) list;
    rels : (string * (event * event) list) list;
  }

let rec pp_set oc = function
  | [] -> ()
  | [e] -> fprintf oc "%s" e
  | e :: es -> fprintf oc "%s,%a" e pp_set es
		       
let rec pp_rel oc = function
  | [] -> ()
  | [(e,e')] -> fprintf oc "(%s,%s)" e e'
  | (e,e') :: es -> fprintf oc "(%s,%s),%a" e e' pp_rel es

let pp_exec oc exec =
  List.iter (fun (name,tuples) ->
	     fprintf oc "Set: %s={%a}\n" name pp_set tuples) exec.sets;
  List.iter (fun (name,tuples) ->
	     fprintf oc "Rel: %s={%a}\n" name pp_rel tuples) exec.rels
	    
let empty_exec = { sets = []; rels = [] }

let get_set x s =
  try List.assoc s x.sets
  with Not_found -> failwith (asprintf "Couldn't find set %s" s)

let get_sets x e =
  List.map fst (List.filter (fun (_,es) -> List.mem e es) x.sets)
	      
let get_rel x r =
  try List.assoc r x.rels
  with Not_found -> failwith (asprintf "Couldn't find relation %s" r)

(*******************************************)
(* Resolving locations, threads and values *)
(*******************************************)
			     
let loc_of_int = function
  | 0 -> "x"
  | 1 -> "y"
  | 2 -> "z"
  | 3 -> "w"
  | n -> sprintf "x%d" (n - 4)

let find_equiv_classes r dom =
  let rec find_equiv e = function
    | [] -> raise Not_found
    | (e',i) :: _ when List.mem (e,e') r -> i
    | _ :: class_map -> find_equiv e class_map
  in
  let f (i, class_map) e =
    try
      let i' = find_equiv e class_map in (i, (e,i')::class_map)
    with Not_found ->
      (i+1, (e,i)::class_map)
  in
  let _, class_map = List.fold_left f (0, []) dom in
  class_map
