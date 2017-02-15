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

(** Parsing execution from XML file *)

open Format
open General_purpose
open Exec

let parse_file xml_path =
  let alloy_soln = Xml.parse_file xml_path in
  assert (Xml.tag alloy_soln = "alloy");
  let instance = List.hd (Xml.children alloy_soln) in
  assert (Xml.tag instance = "instance");
  let entities = Xml.children instance in
  let tag_is s e = Xml.tag e = s in
  let label_of e = Xml.attrib e "label" in
  let field_nodes = List.filter (tag_is "field") entities in

  let exec =
    let add_field exec field_node =
      let field_name = label_of field_node in
      let field_children = Xml.children field_node in
      let arity =
	match List.filter (tag_is "types") field_children with
	| [type_node] -> List.length (Xml.children type_node) - 1
	| _ -> failwith "Expected a `types` node."
      in
      let add_sing tuples tuple_node =
	match Xml.children tuple_node with
	| [_;e] -> (label_of e) :: tuples
	| es -> failwith "Expected a 1-tuple."
      in
      let add_pair tuples tuple_node =
	match Xml.children tuple_node with
	| [_;e;e'] -> (label_of e, label_of e') :: tuples
	| _ -> failwith "Expected a 2-tuple."
      in
      let tuple_nodes = List.filter (tag_is "tuple") field_children in
      match arity with
      | 1 -> let tuples = List.fold_left add_sing [] tuple_nodes in
	     { exec with sets = (field_name, tuples) :: exec.sets }
      | 2 -> let tuples = List.fold_left add_pair [] tuple_nodes in
	     { exec with rels = (field_name, tuples) :: exec.rels }
      | _ -> failwith "Unexpected arity %d." arity
    in
    List.fold_left add_field empty_exec field_nodes
  in
  ("C", exec) (* TODO: architecture here? *)
