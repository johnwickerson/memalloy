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

(** Converting an .xml file into an .soln file *)

open Format
open General_purpose

let get_args () =
  let out_path = ref None in
  let xml_path = ref None in
  let speclist = [
      ("-o", Arg.String (set_option_ref out_path),
       "Output file (optional; defaults to stdout)");
    ]
  in
  let usage_msg =
    "Converting an xml file into an execution (or an execution pair).\nUsage: `xml2exec [options] <xml_file.xml>`.\nOptions available:"
  in
  Arg.parse speclist (set_option_ref xml_path) usage_msg;
  let out_channel = match !out_path with
    | None -> stdout
    | Some path -> open_out path
  in
  let oc = formatter_of_out_channel out_channel in
  let xml_path = match !xml_path with
    | None -> failwith "Expected one .xml file"
    | Some path -> path
  in
  xml_path, oc
                      
let main () =
  let xml_path, oc = get_args () in
  let soln = Xml_input.parse_file xml_path in
  Soln.pp oc soln;
  exit 0  

let _ = main ()
