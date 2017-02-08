(*
MIT License

Copyright (c) 2017 by John Wickerson.

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

open Format
open General_purpose

(*****************************************************************)
(* Generates an Alloy file that can be run to compare two models *)
(*****************************************************************)

let withinit = ref false
let relacq = ref false
let simplepost = ref false
let normws = ref false
let totalsb = ref false
let eventcount = ref 0
let noalloy = ref false
		  
let get_args () =
  let model_paths : string list ref = ref [] in
  let speclist = [
      ("-noalloy", Arg.Set noalloy, "Only generate comparator.als");
      ("-withinit", Arg.Set withinit, "Explicit initial writes");
      ("-relacq", Arg.Set relacq, "Only release/acquire fragment");
      ("-simplepost", Arg.Set simplepost, "Postcondition need not read shared locations");
      ("-normws", Arg.Set normws, "Avoid RMW events");
      ("-totalsb", Arg.Set totalsb, "Total sb per thread");
      ("-events", Arg.Set_int eventcount, "Max number of events");
    ] in
  let usage_msg =
    "Generating an Alloy file that can be run to compare two models.\nUsage: `comparator [options] <model1.als> <model2.als>`.\nOptions available:"
  in
  Arg.parse speclist (set_list_ref model_paths) usage_msg;
  let bad_arg () =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  let model2_path, model1_path =
    get_only_two_elements bad_arg !model_paths
  in
  model1_path, model2_path

let check_args (model1_path, model2_path) =
  assert (Filename.check_suffix model1_path ".als");
  assert (Filename.check_suffix model2_path ".als")
		  
let pp_comparator oc model1_path model2_path arch =
  fprintf oc "open ../%s[E] as M1\n" (Filename.chop_extension model1_path);
  fprintf oc "open ../%s[E] as M2\n\n" (Filename.chop_extension model2_path);
  fprintf oc "sig E {}\n\n";
  fprintf oc "pred gp [X:%s] {\n\n" arch;
  if !withinit then
    fprintf oc "  withinit[X]\n\n"
  else
    fprintf oc "  withoutinit[X]\n\n"; 
  fprintf oc "  not(M1/consistent[none,X])\n";
  fprintf oc "  M1/dead[none,X]\n";
  fprintf oc "  M2/consistent[none,X]\n\n";
  if !relacq then (
    fprintf oc "  // Stay within the rel/acq fragment\n";
    fprintf oc "  R[none,X] in acq[none,X]\n";
    fprintf oc "  W[none,X] in rel[none,X]\n";
    fprintf oc "  no sc[none,X]\n"
  );
  if !normws then (
    fprintf oc "  // Avoid RMW events\n";
    fprintf oc "  no_RMWs[none,X]\n"
  );
  if !totalsb then (
    fprintf oc "  // Total sb per thread\n";
    fprintf oc "  total_sb[none,X]\n"
  );
  if !simplepost then (
    fprintf oc "  // The postcondition need not read shared locations\n";
    fprintf oc "  co[none,X] in (rc[rf[none,X]]) . (rc[(sb[none,X]) . (rc[~(rf[none,X])])])\n"
  );
  fprintf oc "}\n\n";
  fprintf oc "run gp for 1 Exec, %d E, 3 Int\n" !eventcount

let main () =
  let model1_path, model2_path = get_args () in
  check_args (model1_path, model2_path);
  if not (Sys.file_exists "alloystar") then
    failwith "Please run from the top-level directory of the repo";
  let read_arch path =
    let first_line =
      try input_line (open_in path)
      with
      | Sys_error _ -> failwith (sprintf "Couldn't open %s" path)
      | End_of_file -> failwith (sprintf "File %s is empty" path)
    in
    let regex = Str.regexp "//[ \t]+\\([A-Za-z0-9_]+\\)" in
    try
      let _ = Str.string_match regex first_line 0 in
      Str.matched_group 1 first_line
    with Not_found ->
      failwith (sprintf "Missing architecture in %s" path)
  in
  let arch =
    let arch1 = read_arch model1_path in
    let arch2 = read_arch model2_path in
    if arch1=arch2 then arch1 else
      failwith (sprintf "Mismatch between %s and %s" arch1 arch2)
  in
  let comparator_als = "comparator/comparator.als" in
  let oc = open_out comparator_als in
  pp_comparator (formatter_of_out_channel oc) model1_path model2_path arch;
  close_out oc;
  if !noalloy then exit 0;
  let stamp =
    let open Unix in
    let t = localtime (time ()) in
    sprintf "%02d%02d%02d-%02d%02d%02d"
	    ((t.tm_year + 1900) mod 100) (t.tm_mon + 1) t.tm_mday
	    t.tm_hour t.tm_min t.tm_sec
  in
  let mk_fresh_dir_in s =
    if not (Sys.file_exists s) then
      Unix.mkdir s 0o755;
    Sys.chdir s;
    Unix.mkdir stamp 0o755;
    Sys.remove "_latest";
    Unix.symlink stamp "_latest";
    Sys.chdir "..";
  in
  mk_fresh_dir_in "xml";
  let alloy_cmd =
    sprintf "cd alloystar; export SOLVER=glucose; ./runalloy_once.sh ../%s 0 ../xml/_latest"
	    comparator_als
  in
  printf "Alloy started at %s.\n" (now ());
  let alloy_exit_code = Sys.command alloy_cmd in
  printf "Alloy finished at %s.\n" (now ());
  if alloy_exit_code != 0 then (
    printf "Alloy was unsuccessful.\n";
    exit 0
  );
  mk_fresh_dir_in "dot";
  mk_fresh_dir_in "png";
  let rec convert_solns_from i =
    let xml_file = sprintf "xml/_latest/test_%d.xml" i in
    if Sys.file_exists xml_file then
      let dot_file = sprintf "dot/_latest/test_%d.dot" i in
      let png_file = sprintf "png/_latest/test_%d.png" i in
      let gen_cmd = sprintf "gen/gen -Tdot -o %s %s" dot_file xml_file in
      let dot_cmd = sprintf "dot -Tpng -o %s %s" png_file dot_file in
      let _ = Sys.command gen_cmd in
      let _ = Sys.command dot_cmd in
      convert_solns_from (i+1)
    else i
  in
  let sol_count = convert_solns_from 0 in
  begin match Sys.os_type, sol_count with
  | "Unix", 1 -> ignore (Sys.command "open png/_latest/test_0.png")
  | "Unix", _ -> ignore (Sys.command "open png/_latest")
  | _, _ -> printf "Solution(s) are in png/_latest."
  end;
  exit 0
       
let _ = main ()
