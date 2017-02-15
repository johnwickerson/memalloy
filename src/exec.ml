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

(** Datatype for representing executions *)

open Format
open General_purpose

type event = string

(** Remove dollar signs from event names *)
let pp_event_name oc e =
  fprintf oc "%s" (Str.global_replace (Str.regexp_string "$") "" e)

(** An execution is a list of named event sets and a list of named event relations *)
type execution = {
    sets : (string * event list) list;
    rels : (string * (event * event) list) list;
  }

(** Basic pretty-printing of executions *)
let pp_exec oc exec =
  let pp_set (name,tuples) =
    fprintf oc "Set: %s={%a}\n" name (fprintf_iter "," pp_str) tuples
  in
  let pp_rel (name,tuples) =
    fprintf oc "Rel: %s={%a}\n" name (fprintf_iter "," pp_pair) tuples
  in
  List.iter pp_set exec.sets;
  List.iter pp_rel exec.rels

(** The empty execution *)
let empty_exec = { sets = []; rels = [] }

let get_set x s =
  try List.assoc s x.sets
  with Not_found -> failwith "Couldn't find set %s" s

let get_sets x e =
  List.map fst (List.filter (fun (_,es) -> List.mem e es) x.sets)
	      
let get_rel x r =
  try List.assoc r x.rels
  with Not_found -> failwith "Couldn't find relation %s" r

(** {2 Resolving locations, threads and values } *)

(** A collection of maps that identify the thread, location, value written, and value read (where applicable) of each event *)
type execution_maps = {
    thd_map : (event, int) map;
    loc_map : (event, Location.t) map;
    wval_map : (event, Value.t) map;
    rval_map : (event, Value.t) map;
  }

(** [remove_transitive r_name x] returns a new execution in which the relation named [r_name] has been replaced with its intransitive core *)
let remove_transitive r_name x =
  let r = List.assoc r_name x.rels in
  let r = remove_transitive_edges r in
  { x with rels = (r_name, r) :: (remove_assocs [r_name] x.rels) }

(** Build a map from each write event to the value that it writes. Initial writes, if present, write zero; all other writes write positive integers. The value written by each write event corresponds to its position in the coherence order. *)
let mk_wval_map loc_map co ws iws =
  let loc_map = List.filter (fun (e,_) -> List.mem e ws) loc_map in
  let loc_classes = val_list (invert_map loc_map) in
  let loc_classes = List.map (List.sort (compare co)) loc_classes in
  let rec mk_val (i,res) e = match i, List.mem e iws with
    | 0, false -> mk_val (1,res) e
    | _ -> (i+1, (e,i)::res)
  in
  let tag_with_indices es =
    snd (List.fold_left mk_val (0,[]) es)
  in
  List.concat (List.map tag_with_indices loc_classes)

(** Build a map from each read event to the value that it reads. If the event has an incoming reads-from edge, then this value is the value written by the source of that edge. If the event has no incoming reads-from edge, then this value is 0. *)
let mk_rval_map wval_map rf =
  let find_src e = try
      let w = List.assoc e (invert_rel rf) in
      try List.assoc w wval_map
      with Not_found -> failwith "Expected write to have a value"
    with Not_found -> 0
  in
  List.map (fun e -> (e, find_src e))

(** Create maps that identify threads, locations and values for each event in the given execution *)
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
