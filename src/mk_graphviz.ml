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

(** Converting executions into Graphviz graphs *)

open Format
open General_purpose
open Exec
open Graphviz

(** Convert a single event into a Graphviz node description *)
let dot_of_event x maps e =
  let dir,vals,bgcolor =
    match List.mem e (get_set x "R"),
	  List.mem e (get_set x "W"),
	  List.mem e (get_set x "F")
    with
    | true, true, false ->
       let rval = List.assoc e maps.rval_map in
       let wval = List.assoc e maps.wval_map in
       "RMW", sprintf "=%d>%d" rval wval, "plum"
    | true, false, false ->
       let rval = List.assoc e maps.rval_map in
       "R", sprintf "=%d" rval, "lightpink1"
    | false, true, false ->
       let wval = List.assoc e maps.wval_map in
       "W", sprintf "=%d" wval, "lightblue1"
    | false, false, true -> "F", "", "peachpuff1"
    | false, false, false -> "Nop", "", "azure3"
    | r,w,f -> failwith "Found event with (R,W,F)=(%b,%b,%b)" r w f
  in
  let ignored_attrs = ["ev";"R";"W";"F";"IW"] in
  let attrs = MySet.diff (get_sets x e) ignored_attrs in
  let loc =
    try asprintf "%a" Location.pp (List.assoc e maps.loc_map)
    with Not_found -> ""
  in
  let e = asprintf "%a" Event.pp e in
  let label = asprintf "%s: %s[%a]%s%s" e dir
		       (MyList.pp "," pp_str) attrs loc vals
  in
  let attrs =
    ["label", label;
     "shape", "box";
     "color", "white";
     "style", "filled";
     "fillcolor", bgcolor]
  in
  Single (e, attrs)
     
(** Convert a relation into a set of Graphviz edge descriptions *)
let dot_of_rel (name, tuples) =
  let edge_color = function
    | "co" -> "cornflowerblue"
    | "rf" -> "crimson"
    | "sb" | "ad" | "cd" | "dd" -> "black"
    | "pi" -> "red"
    | _ -> "black"
  in
  let gv_attrs =
    if List.mem name ["sb"] then [] else [("constraint", "false")]
  in
  let gv_attrs = ("fontcolor", edge_color name) :: gv_attrs in
  let gv_attrs = ("color", edge_color name) :: gv_attrs in
  let gv_attrs = ("label", name) :: gv_attrs in
  let dot_of_pair (e,e') =
    let e = asprintf "%a" Event.pp e in
    let e' = asprintf "%a" Event.pp e' in
    (e,e',gv_attrs)
  in
  List.map dot_of_pair tuples

let dot_of_execution' resolved_x y =
  let y = remove_transitive "sb" y in
  let y = remove_transitive "co" y in
  let maps = resolve_exec y in
  let maps = match resolved_x with
    | None -> maps
    | Some (x,xmaps,pi) -> rectify_maps (x,xmaps) (y,maps) pi
  in
  let thds = Assoc.val_list (Assoc.invert_map maps.thd_map) in
  let mk_cluster ns =
    Cluster (ns, ["color", "azure4"; "style", "dashed"])
  in
  let nodes =
    let doe = dot_of_event y maps in
    List.map doe (get_set y "IW") @
      List.map (fun thd -> mk_cluster (List.map doe thd)) thds
  in
  let visible_rels = Assoc.remove_assocs ["sloc";"sthd"] y.rels in
  let edges = List.concat (List.map dot_of_rel visible_rels) in
  maps, {nodes = nodes; edges = edges}
	   
(** Convert an execution into a complete Graphviz file *)
let dot_of_execution x =
  snd (dot_of_execution' None x)

(** Convert a pair of executions into a complete Graphviz file *)
let dot_of_execution_pair x y pi =
  let xmaps, gx = dot_of_execution' None x in
  let _, gy = dot_of_execution' (Some (x,xmaps,pi)) y in
  let pi_edges = dot_of_rel ("pi", pi) in
  let nodes = gx.nodes @ gy.nodes in
  let edges = gx.edges @ gy.edges @ pi_edges in
  {nodes = nodes; edges = edges}
