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

(** Converting an execution to Graphviz *)

open Format
open General_purpose
open Exec
		 
let edge_color = function
  | "co" -> "cornflowerblue"
  | "rf" -> "crimson"
  | "sb" | "ad" | "cd" | "dd" -> "black"
  | _ -> "black"

(** Pretty-printing a list of Graphviz attributes *)
let pp_gv_attrs oc attrs =
  let pp_gv_attr oc (k,v) = fprintf oc "%s=\"%s\"" k v in
  fprintf_iter "," pp_gv_attr oc attrs

(** Convert a single event into a Graphviz node description *)
let dot_of_event x maps oc e =
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
  let attrs = diff (get_sets x e) ignored_attrs in
  let loc =
    try asprintf "%a" Location.pp (List.assoc e maps.loc_map)
    with Not_found -> ""
  in
  let thd =
    try Some (List.assoc e maps.thd_map)
    with Not_found -> None
  in 
  let gv_attrs =
    [("label",
      asprintf "%a: %s[%a]%s%s"
	       pp_event_name e dir (fprintf_iter "," pp_str) attrs
	       loc vals);
     ("shape", "box");
     ("color", "white");
     ("style", "filled");
     ("fillcolor", bgcolor)]
  in
  let printnode () =
    fprintf oc "\"%a\" [%a]\n" pp_event_name e pp_gv_attrs gv_attrs
  in
  match thd with
  | Some tid ->
     fprintf oc "subgraph cluster_%d {\n" tid;
     fprintf oc "color=azure4;\n";
     fprintf oc "style=dashed;\n";
     printnode ();
     fprintf oc "}\n"
  | None ->
     printnode ()
     
(** Convert a relation into a set of Graphviz edge descriptions *)
let dot_of_rel oc (name, tuples) =
  let gv_attrs =
    if List.mem name ["sb"] then [] else [("constraint", "false")]
  in
  let gv_attrs = ("fontcolor", edge_color name) :: gv_attrs in
  let gv_attrs = ("color", edge_color name) :: gv_attrs in
  let gv_attrs = ("label", name) :: gv_attrs in
  let dot_of_pair (e,e') =
    fprintf oc "%a -> %a [%a]\n"
	    pp_event_name e pp_event_name e' pp_gv_attrs gv_attrs
  in
  List.iter dot_of_pair tuples

(** Convert an execution into a complete Graphviz file *)
let dot_of_execution oc x =
  let x = remove_transitive "sb" x in
  let x = remove_transitive "co" x in
  let maps = resolve_exec x in
  fprintf oc "digraph G {\n";
  fprintf oc "ranksep = 1.3;\n";
  fprintf oc "nodesep = 0.8;\n";
  List.iter (dot_of_event x maps oc) (get_set x "ev");
  let visible_rels = remove_assocs ["sloc";"sthd"] x.rels in
  List.iter (dot_of_rel oc) visible_rels;
  fprintf oc "}\n"
