(*
MIT License

Copyright (c) 2018 by John Wickerson.

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

(** Converting a solution found by Alloy into a hash. Isomorphic solutions should have the same hash, which means that the hash can be used to quickly discard duplicate solutions. *)

open! Format
open! General_purpose

let get_args () =
  let xml_path : string list ref = ref [] in
  let speclist = [] in
  let usage_msg = "Usage: `mk_hash [options] xml_file.xml`.\n\
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
  xml_path

let run xml_path oc =
  let soln =
    try Xml_input.parse_file xml_path
    with Xml.File_not_found _ ->
      failwith "ERROR: Couldn't make hash from %s; file not found." xml_path
  in
  Soln.hash_soln oc soln
  
let main () =
  let xml_path = get_args () in
  let oc = formatter_of_out_channel stdout in
  run xml_path oc;
  exit 0

let _ =
  if MyStr.endswith Sys.argv.(0) "mk_hash" then
    main ()
