(*
MIT License

Copyright (c) 2017 by John Wickerson, Nathan Chong, and Tyler Sorensen.

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


open! Format
open! General_purpose

let get_args () =
  let xml_path : string list ref = ref [] in
  let out_path : string list ref = ref [] in
  let speclist = [
      ("-o", Arg.String (set_list_ref out_path),
       "Output directory (mandatory)");
    ] in
  let usage_msg = "Usage: `weaken [options] [xml_file.xml | xml_dir]`.\n\
                   Options available:"
  in
  let speclist = Global_options.speclist @ speclist in
  Arg.parse speclist (set_list_ref xml_path) usage_msg;
  let bad_arg () =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  let xml_path =
    try MyList.the !xml_path with Not_found -> bad_arg ()
  in
  let out_path =
    try MyList.the !out_path with Not_found -> bad_arg ()
  in
  xml_path, out_path

let hashes_seen = ref []
  
let write_xml_into_dir out_dir origin xml_root =
  let file_contents = Xml.to_string_fmt xml_root in
  Filename.set_temp_dir_name out_dir;
  let _, oc = Filename.open_temp_file "test_" ".xml" in
  let fmtr = formatter_of_out_channel oc in
  fprintf fmtr "<!-- weakened from : %s -->\n" origin;
  fprintf fmtr "%s" file_contents;
  close_out oc

let tag_is s e = Xml.tag e = s

let label_of e = Xml.attrib e "label"

let label_is s e = label_of e = s
  
let rec xml_map f = function
  | Xml.Element (name, attrs, children) ->
     let name', attrs', children' = f (name, attrs, children) in
     Xml.Element (name', attrs', List.map (xml_map f) children')
  | Xml.PCData str -> Xml.PCData str

let rec get_rel rname = function
  | [Xml.Element ("alloy", _, [instance])] ->
     get_rel rname [instance]
  | [Xml.Element ("instance", _, entities)] ->
     get_rel rname entities
  | Xml.Element ("field", _, children) as elt :: _
       when label_of elt = rname ->
     get_rel rname children
  | Xml.Element ("tuple", _, atoms) :: elts ->
     get_rel rname atoms @ get_rel rname elts
  | [_; Xml.Element ("atom",_,_) as e1; Xml.Element ("atom",_,_) as e2] ->
     [label_of e1, label_of e2]
  | _ :: elts -> get_rel rname elts
  | [] -> []

let rec get_set sname = function
  | [Xml.Element ("alloy", _, [instance])] ->
     get_set sname [instance]
  | [Xml.Element ("instance", _, entities)] ->
     get_set sname entities
  | Xml.Element ("field", _, children) as elt :: _
       when label_of elt = sname ->
     get_set sname children
  | Xml.Element ("tuple", _, atoms) :: elts ->
     get_set sname atoms @ get_set sname elts
  | [_; Xml.Element ("atom",_,_) as e] ->
     [label_of e]
  | _ :: elts -> get_set sname elts
  | [] -> []
                    
let rm_EV e ((name, attrs, children) as elt) = match name with
  | "field" ->
     let doesnt_contain_e tuple =
       match Xml.tag tuple with
       | "tuple" -> not (List.exists (label_is e) (Xml.children tuple))
       | _ -> true
     in 
     let children = List.filter doesnt_contain_e children in
     (name, attrs, children)
  | _ -> elt

let rm_from_dom rname e ((name, attrs, children) as elt) = match name with
  | "field" when label_of (Xml.Element elt) = rname ->
     let doesnt_start_at_e tuple =
       match Xml.tag tuple, Xml.children tuple with
       | "tuple", [_;e1;_] -> label_of e1 <> e
       | _ -> true
     in 
     let children = List.filter doesnt_start_at_e children in
     (name, attrs, children)
  | _ -> elt

let rm_from_dom_and_rng rname e ((name, attrs, children) as elt) = match name with
  | "field" when label_of (Xml.Element elt) = rname ->
     let doesnt_touch_e tuple =
       match Xml.tag tuple, Xml.children tuple with
       | "tuple", [_;e1;e2] ->
          label_of e1 <> e && label_of e2 <> e
       | _ -> true
     in 
     let children = List.filter doesnt_touch_e children in
     (name, attrs, children)
  | _ -> elt

let rm_from_set rname e ((name, attrs, children) as elt) = match name with
  | "field" when label_of (Xml.Element elt) = rname ->
     let isnt_e tuple =
       match Xml.tag tuple, Xml.children tuple with
       | "tuple", [_;e1] -> label_of e1 <> e
       | _ -> true
     in 
     let children = List.filter isnt_e children in
     (name, attrs, children)
  | _ -> elt

let not_already_seen soln =
  let hash = fprintf_to_string (Mk_hash.hash_xml soln) in
  if List.mem hash !hashes_seen then false
  else (hashes_seen := hash :: !hashes_seen; true)
       
let run_single out_dir xml_path =
  let soln = Xml.parse_file xml_path in
  let set_EV = get_set "EV" [soln] in
  let rm_edge (r,except) =
    Rel.dom (MySet.diff (get_rel r [soln]) except), rm_from_dom r
  in
  let rm_evt (s,except) =
    MySet.diff (get_set s [soln]) except, rm_from_set s
  in
  let edges_to_reduce = [
      "ad", [];
      "cd", [];
      "dd", [];
      "atom", [];
      "mfence", [];
      "pfence", [];
      "psync", [];
      "sync", [];
      "lwsync", get_rel "sync" [soln];
      "isync", [];
      "dmb", [];
      "dmbst", get_rel "dmb" [soln];
      "dmbld", get_rel "dmb" [soln];
      "isb", [];
      "membar_cta", get_rel "membar_gl" [soln];
      "membar_gl", get_rel "membar_cta" [soln];
      "membar_sys", [];
    ] in
  let evts_to_reduce = [
      "A", MySet.union (get_set "ACQ" [soln]) (get_set "REL" [soln]);
      "ACQ", get_set "SC" [soln];
      "REL", get_set "SC" [soln];
      "SC", [];
      "SCREL", [];
      "SCACQ", [];
      "WG", get_set "DV" [soln]; (* remove this line too? *)
      "DV", get_set "SY" [soln];
      "SY", [];
      "SYNC", [];
      "DMBST", MySet.diff set_EV (get_set "DMBLD" [soln]);
      "DMBLD", MySet.diff set_EV (get_set "DMBST" [soln]);
      "MEMBAR_GL", get_set "MEMBAR_SYS" [soln];
      "MEMBAR_SYS", [];
    ] in
  let perturbations =
    [set_EV, rm_EV]
    @ List.map rm_edge edges_to_reduce
    @ List.map rm_evt evts_to_reduce
  in
  let apply_perturbation (dom, perturb) =
    let solns = List.map (fun e -> xml_map (perturb e) soln) dom in
    let not_empty_exec soln = get_set "EV" [soln] <> [] in
    let solns = List.filter not_empty_exec solns in
    let solns = List.filter not_already_seen solns in
    List.iter (write_xml_into_dir out_dir xml_path) solns
  in
  List.iter apply_perturbation perturbations

let run out_dir xml_path =
  if Sys.is_directory xml_path then
    Array.iter (fun f ->
        let xml_path = MyFilename.concat [xml_path; f] in
        run_single out_dir xml_path)
      (Sys.readdir xml_path)
  else
    run_single out_dir xml_path
  
let main () =
  let xml_path, out_dir = get_args () in
  if not (Sys.file_exists xml_path) then
    failwith "Couldn't find `%s`" xml_path;
  hashes_seen := [];
  run out_dir xml_path;
  exit 0

let _ =
  if MyStr.endswith Sys.argv.(0) "weaken" then
    main ()
