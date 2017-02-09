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
let description = ref ""
let iter = ref false
		  
let pp_comparator model1_path model2_path arch oc =
  if !description != "" then fprintf oc "/* %s */\n" !description;
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

let get_args () =
  let model_paths : string list ref = ref [] in
  let speclist = [
      ("-desc", Arg.Set_string description, "Textual description");
      ("-iter", Arg.Set iter, "Find all solutions, not just one");
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

let pp_description () =
  printf "\n";
  printf "\n";
  printf "==============================\n";
  printf "%s\n" !description;
  printf "------------------------------\n"

let find_arch model1_path model2_path =
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
  let arch1 = read_arch model1_path in
  let arch2 = read_arch model2_path in
  if arch1=arch2 then arch1 else
    failwith (sprintf "Mismatch between %s and %s" arch1 arch2)

let write_file als_file pp =
  let oc = open_out als_file in
  pp (formatter_of_out_channel oc);
  close_out oc

let make_stamp () = 
  let open Unix in
  let t = localtime (time ()) in
  let base = sprintf "%02d%02d%02d-%02d%02d%02d"
		     ((t.tm_year + 1900) mod 100) (t.tm_mon + 1)
		     t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  let rec mk_stamp i =
    let stamp = if i=0 then base else sprintf "%s-%d" base i in
    if Sys.file_exists ("xml/" ^ stamp) then mk_stamp (i+1) else stamp
  in
  mk_stamp 0

let mk_fresh_dir_in s stamp =
  if not (Sys.file_exists s) then Unix.mkdir s 0o755;
  Sys.chdir s;
  Unix.mkdir stamp 0o755;
  Sys.remove "_latest";
  Unix.symlink stamp "_latest";
  Sys.chdir ".."

let count p =
  let rec count_helper i = if p i then count_helper (i+1) else i in
  count_helper 0

let run_alloy comparator_als stamp =
  let alloy_cmd =
    sprintf "cd alloystar; ./runalloy_%s.sh"
	    (if !iter then "iter" else "once")
  in
  let solver = "glucose" in
  printf "Alloy started at %s.\n" (now ());
  flush stdout;
  let alloy_exit_code =
    Sys.command (sprintf "export SOLVER=%s; %s ../%s 0 ../xml/%s"
			 solver alloy_cmd comparator_als stamp)
  in
  printf "Alloy finished at %s.\n" (now ());
  if alloy_exit_code != 0 then (
    printf "Alloy was unsuccessful.\n";
    exit 0
  );
  flush stdout;
  let num_solns =
    count (fun i ->
	   Sys.file_exists (sprintf "xml/%s/test_%d.xml" stamp i))
  in
  printf "Alloy found %d solutions.\n" num_solns;
  flush stdout

let remove_dups stamp =
  printf "Removing duplicate solutions.\n";
  flush stdout;
  let py_cmd =
    sprintf "python comparator/partition.py %d xml/%s" !eventcount stamp
  in
  let _ = Sys.command py_cmd in
  flush stdout;
  let num_solns =
    count (fun i ->
	   Sys.file_exists (sprintf "xml/%s/%d_unique" stamp i))
  in
  printf "Reduced to %d unique solutions.\n" num_solns;
  flush stdout;
  num_solns

let xml_to_dot stamp i =
  let xml_dir = sprintf "xml/%s/%d_unique" stamp i in
  let xml_files = Sys.readdir xml_dir in
  assert (Array.length xml_files > 0);
  let xml_file = sprintf "%s/%s" xml_dir (xml_files.(0)) in 
  let dot_file = sprintf "dot/%s/test_%d.dot" stamp i in
  let gen_cmd = sprintf "gen/gen -Tdot -o %s %s" dot_file xml_file in
  ignore (Sys.command gen_cmd)

let dot_to_png stamp i =
  let dot_file = sprintf "dot/%s/test_%d.dot" stamp i in
  let png_file = sprintf "png/%s/test_%d.png" stamp i in
  let dot_cmd = sprintf "dot -Tpng -o %s %s" png_file dot_file in
  ignore (Sys.command dot_cmd)
	 
let main () =
  let model1_path, model2_path = get_args () in
  check_args (model1_path, model2_path);
  if not (Sys.file_exists "alloystar") then
    failwith "Please run me from the top-level directory of the repo";
  pp_description ();
  let arch = find_arch model1_path model2_path in
  let comparator_als = "comparator/comparator.als" in
  write_file comparator_als (pp_comparator model1_path model2_path arch);
  if !noalloy then exit 0;
  let stamp = make_stamp () in
  mk_fresh_dir_in "xml" stamp;
  run_alloy comparator_als stamp;
  let num_solns = remove_dups stamp in
  mk_fresh_dir_in "dot" stamp;
  mk_fresh_dir_in "png" stamp;
  for i = 0 to num_solns - 1 do
    xml_to_dot stamp i;
    dot_to_png stamp i;
    printf "Converted solution %d of %d.\n" i (num_solns - 1);
    flush stdout
  done;
  printf "Solution(s) are in png/%s.\n" stamp;
  begin match Sys.os_type, num_solns with
  | "Unix", 1 -> ignore (Sys.command "open png/_latest/test_0.png")
  | "Unix", _ -> ignore (Sys.command "open png/_latest")
  | _, _ -> ()
  end;
  exit 0
       
let _ = main ()
