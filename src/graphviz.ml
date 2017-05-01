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

(** Representing and pretty-printing Graphviz graphs *)

open! Format
open! General_purpose

(** Graphviz attribute *)
type gv_attr = string * string
       
(** Graphviz node *)
type gv_node =
  | Single of string * gv_attr list
  | Cluster of gv_node list * gv_attr list

(** Graphviz edge *)
type gv_edge = string * string * gv_attr list

(** Graphviz graph *)
type graphviz = {
    nodes: gv_node list;
    edges: gv_edge list;
  }
    
(** Pretty-printing a Graphviz attribute *)
let pp_gv_attr oc (k,v) = fprintf oc "%s=\"%s\"" k v
	   
(** Pretty-printing a list of Graphviz attributes *)
let pp_gv_attrs oc attrs = MyList.pp_gen "," pp_gv_attr oc attrs

(** Pretty-print a graphviz node (single node or cluster node) *)
let rec pp_node oc cid = function
    | Single (name, attrs) ->
       fprintf oc "\"%s\" [%a]\n" name pp_gv_attrs attrs;
       cid
    | Cluster (nodes, attrs) ->
       fprintf oc "subgraph cluster_%d {\n" cid;
       List.iter (fprintf oc "%a;\n" pp_gv_attr) attrs;
       let cid = List.fold_left (pp_node oc) (cid+1) nodes in
       fprintf oc "}\n";
       cid

(** Pretty-print a graphviz edge *)
let pp_edge oc (e,e',attrs) =
  fprintf oc "%s -> %s [%a]\n" e e' pp_gv_attrs attrs

(** Pretty-print a graph *)
let pp_graph oc g =
  fprintf oc "digraph G {\n";
  fprintf oc "ranksep = 1.3;\n";
  fprintf oc "nodesep = 0.8;\n";
  fprintf oc "forcelabels = true;\n";
  ignore (List.fold_left (pp_node oc) 0 g.nodes);
  List.iter (pp_edge oc) g.edges;
  fprintf oc "}\n"
