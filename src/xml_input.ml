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

(** Parsing one or two executions from an XML file *)

open! Format
open! General_purpose

(** Either a single execution, or a pair of executions linked by some mapping relation *)
type input_type =
  | Single of Exec.t
  | Double of Exec.t * Exec.t * Event.t Rel.t

(** A field in an execution is either a set or a relation *)
type field =
  | Set of Event.t list
  | Rel of (Event.t * Event.t) list				   
    
let parse_file xml_path =
  let alloy_soln = Xml.parse_file xml_path in
  assert (Xml.tag alloy_soln = "alloy");
  let instance = List.hd (Xml.children alloy_soln) in
  assert (Xml.tag instance = "instance");
  let entities = Xml.children instance in
  let tag_is s e = Xml.tag e = s in
  let label_of e = Xml.attrib e "label" in
  let simp_label_of e =
    let l = label_of e in
    let l = MyStr.chop_prefix "$gp_" l in
    let l = MyStr.chop_suffix "&apos;" l in
    l
  in
  let label_is s e = label_of e = s in
  let fieldnodes = List.filter (tag_is "field") entities in
  let skolem_nodes = List.filter (tag_is "skolem") entities in
  let find_exec name =
    try
      let skolem_node = List.find (label_is name) skolem_nodes in
      let children = Xml.children skolem_node in
      let tuple_node =
	try MyList.the (List.filter (tag_is "tuple") children)
	with Not_found -> failwith "Expected a 'tuple' node"
      in
      let children = Xml.children tuple_node in
      let atom_node =
	try MyList.the (List.filter (tag_is "atom") children)
	with Not_found -> failwith "Expected an 'atom' node"
      in
      Some (label_of atom_node)
    with Not_found -> None
  in
  let build_set xo =
    let add_sing tuples tuple_node =
      match xo, Xml.children tuple_node with
      | Some x, [x';e] when label_of x' = x ->
	 (label_of e) :: tuples
      | Some _, [_;_] -> tuples
      | None, [e] ->
	 (label_of e) :: tuples
      | _ -> failwith "Expected a 1-tuple."
    in
    List.fold_left add_sing []
  in
  let build_rel xo =
    let add_pair tuples tuple_node =
      match xo, Xml.children tuple_node with
      | Some x, [x';e;e'] when label_of x' = x ->
	 (label_of e, label_of e') :: tuples
      | Some _, [_;_;_] -> tuples
      | None, [e;e'] ->
	 (label_of e, label_of e') :: tuples
      | _ -> failwith "Expected a 2-tuple."
    in
    List.fold_left add_pair []
  in
  let mk_field xo field_node =
    let field_children = Xml.children field_node in
    let arity =
      let type_node =
	try MyList.the (List.filter (tag_is "types") field_children)
	with Not_found -> failwith "Expected a single 'types' node" 
      in
      List.length (Xml.children type_node)
    in
    let tuple_nodes = List.filter (tag_is "tuple") field_children in
    match arity,xo with
    | 1, None | 2, Some _ -> Set (build_set xo tuple_nodes)
    | 2, None | 3, Some _ -> Rel (build_rel xo tuple_nodes)
    | _ -> failwith "Unexpected arity %d" arity
  in
  let add_field xo exec field_node =
    let field_name = simp_label_of field_node in
    match mk_field xo field_node with
    | Set tuples ->
       let sets = Assoc.add_assocs exec.Exec.sets (field_name,tuples) in
       { exec with Exec.sets = sets }
    | Rel tuples ->
       let rels = Assoc.add_assocs exec.Exec.rels (field_name,tuples) in
       { exec with Exec.rels = rels }
  in
  let x1 =
    match find_exec "$gp_X" with
    | None -> failwith "Could not find execution 'X'"
    | Some x1 -> x1
  in
  let exec1 = Exec.empty_exec in
  let exec1 = List.fold_left (add_field (Some x1)) exec1 fieldnodes in
  let is_primed e = MyStr.endswith (label_of e) "&apos;" in
  let reserved_rels = ["$gp_map"; "$gp_X"; "$gp_Y"; "$consistent_s"] in
  let is_reserved e = List.mem (label_of e) reserved_rels in
  let is_extra_rel1 e = not (is_reserved e) && not (is_primed e) in
  let extra_rels1 = List.filter is_extra_rel1 skolem_nodes in
  let extra_rels2 = List.filter is_primed skolem_nodes in
  let exec1 = List.fold_left (add_field None) exec1 extra_rels1 in
  match find_exec "$gp_Y" with
  | None -> Single exec1
  | Some x2 ->
     let exec2 = Exec.empty_exec in
     let exec2 = List.fold_left (add_field (Some x2)) exec2 fieldnodes in
     let exec2 = List.fold_left (add_field None) exec2 extra_rels2 in
     let pi_node = List.find (label_is "$gp_map") skolem_nodes in
     match mk_field None pi_node with
     | Rel pi -> Double (exec1, exec2, pi)
     | _ -> failwith "Ill-formed 'map' relation"
