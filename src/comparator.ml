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

(** Compares memory models *)

open Format
open General_purpose

let withinit = ref false
let minimal = ref false
let eventcount = ref 0
let description = ref ""
let iter = ref false
let expectation = ref None
let atleastnthreads = ref 0		      
let atleastnlocs = ref 0
			  
let relacq = ref false
let simplepost = ref false
let normws = ref false
let nofences = ref false
let totalsb = ref false
let nodeps = ref false
let noscrelacq = ref false

(** [range i j] returns [i, i+1, ..., j] *)
let rec range i j = if i > j then [] else i :: (range (i+1) j)

(** [min_classes n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the existence of [n] distinct events in [dom], none of which are related by [r]*)
let min_classes n r dom oc =
  let es = List.map (sprintf "e%d") (range 1 n) in
  fprintf oc "  some disj %a : E {\n" (fprintf_iter ", " pp_str) es;
  fprintf oc "    %a in X.(%s)\n" (fprintf_iter "+" pp_str) es dom;
  fprintf oc "    no ((sq[%a]-iden) & %s[none,X])\n"
	  (fprintf_iter "+" pp_str) es r;
  fprintf oc "  }\n"

(** [pp_comparator (succ_paths, fail_paths) arch oc] generates an Alloy file (sent to [oc]) that can be used to find an execution of type [arch] that satisfies all the models in [succ_paths] and violates all the models in [fail_paths]. *)
let pp_comparator (succ_paths, fail_paths) arch oc =
  if !description != "" then fprintf oc "/* %s */\n" !description;
  for i = 1 to List.length succ_paths do
    let model = Filename.chop_extension (List.nth succ_paths (i-1)) in
    fprintf oc "open models/%s[E] as M%d\n" model i
  done;
  for i = 1 to List.length fail_paths do
    let model = Filename.chop_extension (List.nth fail_paths (i-1)) in
    fprintf oc "open models/%s[E] as N%d\n" model i
  done;
  fprintf oc "sig E {}\n\n";
  fprintf oc "pred gp [X:%a] {\n\n" Archs.pp_Arch arch;
  fprintf oc "  // Every event is a read, write or a fence\n";
  fprintf oc "  E in R[none,X] + W[none,X] + F[none,X]\n\n";
  if !withinit then
    fprintf oc "  withinit[X]\n\n"
  else
    fprintf oc "  withoutinit[X]\n\n";
  for i = 1 to List.length fail_paths do
    fprintf oc "  not(N%d/consistent[none,X])\n" i;
    fprintf oc "  N%d/dead[none,X]\n" i
  done;
  fprintf oc "\n";
  for i = 1 to List.length succ_paths do
    fprintf oc "  M%d/consistent[none,X]\n" i
  done;
  if !minimal then (
    fprintf oc "  not (some e : X.ev {\n";
    for i = 1 to List.length fail_paths do
      fprintf oc "    not(N%d/consistent[e,X])\n" i;
      fprintf oc "    N%d/dead[e,X]\n" i
    done;
    for i = 1 to List.length succ_paths do
      fprintf oc "    M%d/consistent[e,X]\n" i
    done;
    fprintf oc "  })\n"
  );
  if !relacq then (
    fprintf oc "  // Stay within the rel/acq fragment\n";
    fprintf oc "  R[none,X] in acq[none,X]\n";
    fprintf oc "  W[none,X] in rel[none,X]\n";
    fprintf oc "  no sc[none,X]\n"
  );
  if !noscrelacq then (
    fprintf oc "  // Avoid screl and scacq events\n";
    fprintf oc "  no (screl[none,X] + scacq[none,X])\n"
  );
  if !normws then (
    fprintf oc "  // Avoid RMW events (single and dual events)\n";
    fprintf oc "  no_RMWs[none,X]\n";
    fprintf oc "  no atom[none,X]\n"
  );
  if !nofences then (
    fprintf oc "  // Avoid fences\n";
    fprintf oc "  no F[none,X]\n";
  );
  if !totalsb then (
    fprintf oc "  // Total sb per thread\n";
    fprintf oc "  total_sb[none,X]\n"
  );
  if !nodeps then (
    fprintf oc "  // Avoid dependencies\n";
    fprintf oc "  no (ad[none,X] + cd[none,X] + dd[none,X])\n"
  );
  if !simplepost then (
    fprintf oc "  // The postcondition need not read shared locations\n";
    fprintf oc "  co[none,X] in (rc[rf[none,X]]) . (rc[(sb[none,X]) . (rc[~(rf[none,X])])])\n"
  );
  if 0 < !atleastnthreads then (
    fprintf oc "  // At least %d threads\n" !atleastnthreads;
    min_classes !atleastnthreads "sthd" "ev - IW" oc
  );
  if 0 < !atleastnlocs then (
    fprintf oc "  // At least %d locations\n" !atleastnlocs;
    min_classes !atleastnlocs "sloc" "R + W" oc
  );
  fprintf oc "}\n\n";
  fprintf oc "run gp for 1 Exec, %s%d E, 3 Int\n"
	  (if !minimal then "exactly " else "")
	  !eventcount

let get_args () =
  let succ_paths : string list ref = ref [] in
  let fail_paths : string list ref = ref [] in
  let arch : string list ref = ref [] in
  let speclist = [
      ("-satisfies", Arg.String (set_list_ref succ_paths),
       "Execution should satisfy this model (repeatable)");
      ("-violates", Arg.String (set_list_ref fail_paths),
       "Execution should violate this model (repeatable)");
      ("-arch", Arg.String (set_list_ref arch),
       "Type of executions being compared (required)");
      ("-events", Arg.Set_int eventcount, "Max number of events");
      ("-expect", Arg.Int (fun i -> expectation := Some i),
       "Expect to find this many unique solutions (optional)");
      ("-desc", Arg.Set_string description,
       "Textual description (optional)");
      ("-atleastnthreads", Arg.Set_int atleastnthreads,
       "Find executions with at least N threads (default 0)");
      ("-atleastnlocs", Arg.Set_int atleastnlocs,
       "Find executions with at least N locations (default 0)");
      ("-iter", Arg.Set iter, "Option: find all solutions");
      ("-minimal", Arg.Set minimal, "Option: find minimal executions");
      ("-withinit", Arg.Set withinit,
       "Option: explicit initial writes");
      ("-relacq", Arg.Set relacq,
       "Option: Only release/acquire fragment");
      ("-simplepost", Arg.Set simplepost,
       "Option: postcondition need not read shared locations");
      ("-normws", Arg.Set normws, "Option: avoid RMW events");
      ("-nofences", Arg.Set nofences, "Option: avoid fences");
      ("-noscrelacq", Arg.Set noscrelacq,
       "Option: avoid screl and scacq events");
      ("-totalsb", Arg.Set totalsb, "Option: total sb per thread");
      ("-nodeps", Arg.Set nodeps,
       "Option: avoid address/control/data dependencies");
    ] in
  let usage_msg =
    "Generating an Alloy file that can be run to compare two models.\nUsage: `comparator [options]`. There must be at least one -satisfies or -violates flag.\nOptions available:"
  in
  let bad_arg _ =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  Arg.parse speclist bad_arg usage_msg;
  let arch = get_only_element bad_arg !arch in
  let arch = Archs.parse_arch arch in
  !succ_paths, !fail_paths, arch

let check_args (succ_paths, fail_paths) =
  match succ_paths @ fail_paths with
  | [] -> failwith "Expected at least one model"
  | _ -> ()
(*| paths ->
     let check_ext p =
       if not (Filename.check_suffix p ".als") then
	 failwith "Expected file(s) ending with .als"
     in
     List.iter check_ext paths*)

(** Print a description of the comparison being undertaken *)
let pp_description () =
  printf "\n";
  printf "\n";
  printf "==============================\n";
  printf "%s\n" !description;
  printf "------------------------------\n"      

let write_file als_file pp =
  let oc = open_out als_file in
  pp (formatter_of_out_channel oc);
  close_out oc

(** Make a unique directory name for output files, based on the current time *)
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

(** [mk_fresh_dir_in s stamp] creates a new directory called [stamp] inside the directory [s], and points the symlink "_latest" to the newly-created directory *)
let mk_fresh_dir_in s stamp =
  if not (Sys.file_exists s) then Unix.mkdir s 0o755;
  Sys.chdir s;
  Unix.mkdir stamp 0o755;
  if (Sys.file_exists "_latest") then Sys.remove "_latest";
  Unix.symlink stamp "_latest";
  Sys.chdir ".."

(** [count p] returns the first non-negative integer that does not satisfy the predicate [p] *)
let count p =
  let rec count_helper i = if p i then count_helper (i+1) else i in
  count_helper 0

(** Run Alloy on the generated compator file *)
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
  flush stdout;
  num_solns

(** Invoke Tyler's python script to remove duplicate solutions found by Alloy *)
let remove_dups stamp =
  printf "Removing duplicate solutions.\n";
  flush stdout;
  let py_cmd =
    sprintf "python src/partition.py %d xml/%s" !eventcount stamp
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

(** Convert the XML files generated by Alloy into Graphviz format *)
let xml_to_dot stamp i =
  let xml_dir = sprintf "xml/%s/%d_unique" stamp i in
  let xml_files = Sys.readdir xml_dir in
  assert (Array.length xml_files > 0);
  let xml_file = sprintf "%s/%s" xml_dir (xml_files.(0)) in 
  let dot_file = sprintf "dot/%s/test_%d.dot" stamp i in
  let gen_cmd = sprintf "./gen -Tdot -o %s %s" dot_file xml_file in
  let exit_status = Sys.command gen_cmd in
  if exit_status <> 0 then
    failwith "Conversion from .xml to .dot failed"

(** Convert the Graphviz files into PNG format for easy viewing *)
let dot_to_png stamp i =
  let dot_file = sprintf "dot/%s/test_%d.dot" stamp i in
  let png_file = sprintf "png/%s/test_%d.png" stamp i in
  let dot_cmd = sprintf "dot -Tpng -o %s %s" png_file dot_file in
  let exit_status = Sys.command dot_cmd in
  if exit_status <> 0 then
    failwith "Conversion from .dot to .png failed"


 
let main () =
  let succ_paths, fail_paths, arch = get_args () in
  let unrolling_factor = 3 in
  let cat2als path =
    (* let path = Filename.concat Filename.parent_dir_name path in *)
    Cat2als.als_of_file false unrolling_factor path
  in
  begin match succ_paths @ fail_paths with
	| [] -> failwith "Expected at least one model"
	| paths -> List.iter cat2als paths
  end;
  if not (Sys.file_exists "alloystar") then
    failwith "Please run me from the top-level directory of the repo";
  pp_description ();
  let comparator_als = "comparator.als" in
  let pp = pp_comparator (succ_paths, fail_paths) arch in
  write_file comparator_als pp;
  let stamp = make_stamp () in
  mk_fresh_dir_in "xml" stamp;
  let num_solns_incl_dups = run_alloy comparator_als stamp in
  if num_solns_incl_dups = 0 then exit 0;
  let num_solns = remove_dups stamp in
  mk_fresh_dir_in "dot" stamp;
  mk_fresh_dir_in "png" stamp;
  for i = 0 to num_solns - 1 do
    xml_to_dot stamp i;
    dot_to_png stamp i;
    printf "Converted solution %d of %d.\n" (i + 1) num_solns;
    flush stdout
  done;
  printf "Solution(s) are in png/%s.\n" stamp;
  (match Sys.os_type, num_solns with
   | "Unix", 1 -> ignore (Sys.command "open png/_latest/test_0.png")
   | "Unix", _ -> ignore (Sys.command "open png/_latest")
   | _, _ -> ());
  (match !expectation with
   | Some i when i <> num_solns ->
      failwith "Expected %d unique solutions, found %d" i num_solns
   | _ -> ());
  exit 0
       
let _ = main ()
