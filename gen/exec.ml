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
			     
type execution_maps = {
    thd_map : (event, int) map;
    loc_map : (event, Location.t) map;
    wval_map : (event, Value.t) map;
    rval_map : (event, Value.t) map;
  }

let remove_transitive r_name x =
  let r = List.assoc r_name x.rels in
  let r = remove_transitive_edges r in
  { x with rels = (r_name, r) :: (remove_assocs [r_name] x.rels) }

let mk_wval_map loc_map co ws iws =
  let loc_map = List.filter (fun (e,_) -> List.mem e ws) loc_map in
  let loc_classes = val_list (invert_map loc_map) in
  let loc_classes = List.map (List.sort (compare co)) loc_classes in
  printf "loc_classes: [";
  List.iter (fun c ->
	     printf "[";
	     List.iter (fun e -> printf "%s;" e) c;
	     printf "];";
	    ) loc_classes;
  printf "]";
  let rec mk_val (i,res) e = match i, List.mem e iws with
    | 0, false -> mk_val (1,res) e
    | _ -> (i+1, (e,i)::res)
  in
  let tag_with_indices es =
    snd (List.fold_left mk_val (0,[]) es)
  in
  List.concat (List.map tag_with_indices loc_classes)

let mk_rval_map wval_map rf =
  let find_src e = try
      let w = List.assoc e (invert_rel rf) in
      try List.assoc w wval_map
      with Not_found -> failwith "Expected write to have a value"
    with Not_found -> 0
  in
  List.map (fun e -> (e, find_src e))

let resolve_exec x =
  let iws = get_set x "IW" in
  let rs = get_set x "R" in
  let ws = get_set x "W" in
  let rw = union rs ws in
  let nI = diff (get_set x "ev") iws in
  let thd_map = partition true (get_rel x "sthd") nI in
  let loc_map = partition true (get_rel x "sloc") rw in
  let wval_map = mk_wval_map loc_map (get_rel x "co") ws iws in
  let rval_map = mk_rval_map wval_map (get_rel x "rf") rs in
  { thd_map = thd_map; loc_map = loc_map;
    wval_map = wval_map; rval_map = rval_map }
