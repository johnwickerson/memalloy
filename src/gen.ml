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

(** Command-line tool for processing executions and generating litmus tests *)

open Format
open General_purpose
       
type output_type = Dot | Als | Lit
       
let get_args () =
  let xml_path : string list ref = ref [] in
  let out_path : string list ref = ref [] in
  let output_dot = ref false in
  let output_als = ref false in
  let output_lit = ref false in
  let arch = ref None in
  let speclist = [
      ("-Tdot", Arg.Set output_dot, "Produce .dot output");
      ("-Tals", Arg.Set output_als, "Produce .als constraints");
      ("-Tlit", Arg.Set output_lit, "Produce litmus test");
      ("-arch", Arg.String (set_option_ref arch),
       "Optional: execution type");
      ("-o", Arg.String (set_list_ref out_path),
       "Output file (mandatory)");
    ] in
  let usage_msg =
    "Processing executions and generating litmus tests.\nUsage: `gen [options] <xml_file.xml>`.\nOptions available:"
  in
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
  let out_type = match !output_dot, !output_als, !output_lit with
    | true, false, false -> Dot
    | false, true, false -> Als
    | false, false, true -> Lit
    | _ -> bad_arg ()
  in
  let arch = match !arch with
    | Some arch -> Archs.parse_arch arch
    | None -> Archs.Basic
  in
  xml_path, out_path, out_type, arch

let check_args (xml_path, out_path, out_type) =
  assert (Filename.check_suffix xml_path ".xml")
		  
let main () =
  let xml_path, out_path, out_type, arch = get_args () in
  check_args (xml_path, out_path, out_type);
  let exec = Xml_input.parse_file xml_path in
  let oc = open_out out_path in
  let fmtr = formatter_of_out_channel oc in
  begin
    match out_type with
    | Dot ->
       assert (Filename.check_suffix out_path ".dot");
       begin
	 match exec with
	 | Xml_input.Single x ->
	    let g = Mk_graphviz.dot_of_execution x in
	    fprintf fmtr "%a\n" Graphviz.pp_graph g
	 | Xml_input.Double (x,y,pi) ->
	    let g = Mk_graphviz.dot_of_execution_pair x y pi in
	    fprintf fmtr "%a\n" Graphviz.pp_graph g
       end
    | Als ->
       assert (Filename.check_suffix out_path ".als");
       begin
	 match exec with
	 | Xml_input.Single x ->      
	    fprintf fmtr "%a\n" Alsbackend.als_of_execution x
	 | Xml_input.Double (x,y,pi) ->
	    fprintf fmtr "%a\n" Alsbackend.als_of_execution x;
	    fprintf fmtr "%a\n" Alsbackend.als_of_execution y;
	    fprintf fmtr "%a\n" Alsbackend.als_of_rel ("pi", pi)
       end
    | Lit ->
       assert (Filename.check_suffix out_path ".litmus");
       begin
	 match exec with
	 | Xml_input.Single x ->
	    let lt = Mk_litmus.litmus_of_execution x in
	    (match arch with
	     | Archs.Arm8 ->
		let name =
		  Filename.chop_extension (Filename.basename out_path)
		in
		let arm8_lt = Mk_arm8.arm8_of_lit name lt in
		fprintf fmtr "%a\n" Litmus_arm8.pp arm8_lt
	     | _ -> fprintf fmtr "%a\n" Litmus.pp lt)
	 | Xml_input.Double (x,y,pi) ->
	    let lt_src,lt =
	      Mk_litmus.litmus_of_execution_pair x y pi
	    in
	    fprintf fmtr "%a\n" Litmus.pp lt_src;
	    fprintf fmtr "\n";
	    (match arch with
	     | Archs.Arm8 ->
		let name =
		  Filename.chop_extension (Filename.basename out_path)
		in
		let arm8_lt = Mk_arm8.arm8_of_lit name lt in
		fprintf fmtr "%a\n" Litmus_arm8.pp arm8_lt
	     | _ -> fprintf fmtr "%a\n" Litmus.pp lt)
       end
  end;
  close_out oc;
  exit 0
	    
    let _ = main ()     
