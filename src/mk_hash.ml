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
  let usage_msg = "Usage: `weaken [options] xml_file.xml`.\n\
                   Options available:"
  in
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
  let soln = Xml_input.parse_file xml_path in
  Xml_input.hash_soln oc soln
  
let main () =
  let xml_path = get_args () in
  if not (Sys.file_exists xml_path) then
    failwith "Couldn't find `%s`" xml_path;
  let oc = formatter_of_out_channel stdout in
  run xml_path oc;
  exit 0

let _ =
  if not !Sys.interactive then main ()
