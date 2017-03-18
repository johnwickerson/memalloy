(*
MIT License

Copyright (c) 2017 by John Wickerson and Nathan Chong.

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

let succ_paths = ref []
let fail_paths = ref []
let withinit = ref false
let minimal = ref false
let hint = ref None
let eventcount = ref 0
let eventcount2 = ref 0
let description = ref ""
let iter = ref false
let expectation = ref None
let solver = ref "glucose"
		      
let min_thds = ref 0
let max_thds = ref (-1)
let min_locs = ref 0
let max_locs = ref (-1)
let min_txns = ref 0
let max_txns = ref (-1)

(* 
   Got this on stackoverflow, is there a better way? 
   http://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml
*)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let pp_open_modules succ_sig fail_sig oc =
  for i = 1 to List.length !succ_paths do
    let model = Filename.chop_extension (List.nth !succ_paths (i-1)) in
    fprintf oc "open %s[%s] as M%d\n" model succ_sig i
  done;
  for i = 1 to List.length !fail_paths do
    let model = Filename.chop_extension (List.nth !fail_paths (i-1)) in
    fprintf oc "open %s[%s] as N%d\n" model fail_sig i
  done

let pp_all_events_used ev_sig oc =
  fprintf oc "  // Every event is a read, write or a fence\n";
  fprintf oc "  %s in R[none,X,ad,cd,dd] + W[none,X,ad,cd,dd] + F[none,X,ad,cd,dd]\n\n" ev_sig

let pp_violated_models exec_sig oc =
  for i = 1 to List.length !fail_paths do
    fprintf oc "  not(N%d/consistent[none,%s,ad,cd,dd])\n" i exec_sig;
    fprintf oc "  N%d/dead[none,%s,ad,cd,dd]\n\n" i exec_sig
  done

let pp_satisfied_models exec_sig oc =
  for i = 1 to List.length !succ_paths do
    fprintf oc "  M%d/consistent[none,%s,ad,cd,dd]\n\n" i exec_sig
  done
  
(** [min_classes ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the existence of [n] distinct objects of type [ev], all in [dom], and none of which are related by [r]*)
let min_classes ev n r dom oc =
  let es = List.map (sprintf "e%d") (range 1 n) in
  fprintf oc "  some disj %a : %s {\n"
	  (MyList.pp_gen ", " pp_str) es ev;
  fprintf oc "    %a in X.(%s)\n" (MyList.pp_gen "+" pp_str) es dom;
  fprintf oc "    no ((sq[%a]-iden) & %s[none,X,ad,cd,dd])\n"
	  (MyList.pp_gen "+" pp_str) es r;
  fprintf oc "  }\n"

(** [pp_min_classes name ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the existence of [n] distinct objects of type [ev], all in [dom], none of which are related by [r]. The [name] is for a descriptive comment.*)
let pp_min_classes name ev n r dom oc =
  if 0 < n then (
    fprintf oc "  // At least %d %s\n" n name;
    min_classes ev n r dom oc
  )
	  
(** [pp_max_classes name ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the non-existence of [n+1] distinct objects of type [ev], all in [dom], none of which are related by [r]. The [name] is for a descriptive comment.*)
let pp_max_classes name ev n r dom oc =
  if 0 <= n then (
    fprintf oc "  // At most %d %s\n" n name;
    fprintf oc "  not (\n";
    min_classes ev (n+1) r dom oc;
    fprintf oc "  )\n"
  )

let pp_file oc path =
  let ic = open_in path in
  try while true do fprintf oc "%s\n" (input_line ic) done
  with End_of_file -> close_in ic
			      
let pp_hint_predicate oc = match !hint with
  | None -> ()
  | Some hint_path -> pp_file oc hint_path

(** [pp_comparator arch oc] generates an Alloy file (sent to [oc]) that can be used to find an execution of type [arch] that satisfies all the models in [!succ_paths] and violates all the models in [!fail_paths]. *)
let pp_comparator arch oc =
  if !description != "" then fprintf oc "/* %s */\n" !description;
  pp_open_modules "E" "E" oc;
  fprintf oc "sig E {}\n\n";
  fprintf oc "pred gp [X:%a, ad,cd,dd:E->E] {\n\n" Archs.pp_Arch arch;
  pp_all_events_used "E" oc;
  if !withinit then
    fprintf oc "  withinit[X]\n\n"
  else
    fprintf oc "  withoutinit[X]\n\n";
  fprintf oc "  wf_%a[X,ad,cd,dd]\n\n" Archs.pp_Arch arch;
  pp_violated_models "X" oc;
  pp_satisfied_models "X" oc;
  if !hint <> None then
    fprintf oc "  hint[X,ad,cd,dd]\n\n";
  if !minimal then (
    fprintf oc "  not (some e : X.ev {\n";
    for i = 1 to List.length !fail_paths do
      fprintf oc "    not(N%d/consistent[e,X,ad,cd,dd])\n" i;
      fprintf oc "    N%d/dead[e,X,ad,cd,dd]\n" i
    done;
    for i = 1 to List.length !succ_paths do
      fprintf oc "    M%d/consistent[e,X,ad,cd,dd]\n" i
    done;
    fprintf oc "  })\n"
  );
  fprintf oc "  not (some e1, e2 : X.ev {\n";
  fprintf oc "    (e1 -> e2) in ad\n";
  for i = 1 to List.length !fail_paths do
    fprintf oc "    not(N%d/consistent[none,X,ad-(e1->e2),cd,dd])\n" i
  done;
  fprintf oc "  })\n";
  fprintf oc "  not (some e1, e2 : X.ev {\n";
  fprintf oc "    (e1 -> e2) in cd\n";
  for i = 1 to List.length !fail_paths do
    fprintf oc "    not(N%d/consistent[none,X,ad,cd-(e1->e2),dd])\n" i
  done;
  fprintf oc "  })\n";
  fprintf oc "  not (some e1, e2 : X.ev {\n";
  fprintf oc "    (e1 -> e2) in dd\n";
  for i = 1 to List.length !fail_paths do
    fprintf oc "    not(N%d/consistent[none,X,ad,cd,dd-(e1->e2)])\n" i
  done;
  fprintf oc "  })\n";
  pp_min_classes "threads" "E" !min_thds "sthd" "ev - IW" oc;
  pp_max_classes "threads" "E" !max_thds "sthd" "ev - IW" oc;
  pp_min_classes "locations" "E" !min_locs "sloc" "R + W" oc;
  pp_max_classes "locations" "E" !max_locs "sloc" "R + W" oc;
  fprintf oc "}\n\n";
  pp_hint_predicate oc;
  fprintf oc "run gp for 1 Exec, %s%d E, 3 Int\n"
	  (if !minimal then "exactly " else "")
	  !eventcount

(** [pp_comparator2 arch mapping_path arch2 oc] generates an Alloy file (sent to [oc]) that can be used to find an execution {i X} of type [arch] and an execution {i Y} of type [arch2] such that {i X} satisfies all the models in [!succ_paths], {i Y} violates all the models in [!fail_paths], and {i X} and {i Y} are related by the mapping in [mapping_path] *)
let pp_comparator2 arch mapping_path arch2 oc =
  if !withinit then
    failwith "Initial writes not supported in compiler mappings";
  if !minimal then
    failwith "Minimality not supported in compiler mappings";
  if !description != "" then fprintf oc "/* %s */\n" !description;
  pp_open_modules "HE" "SE" oc;
  let mapping = Filename.chop_extension mapping_path in
  fprintf oc "open %s[SE,HE] as mapping\n\n" mapping;
  fprintf oc "sig SE, HE {}\n\n";
  fprintf oc "pred gp [X:%a, Y:%a, map:SE->HE] {\n\n"
	  Archs.pp_Arch arch Archs.pp_Arch arch2;
  pp_all_events_used "SE" oc;
  fprintf oc "  withoutinit[X]\n";
  fprintf oc "  withoutinit[Y]\n\n";
  pp_violated_models "X" oc;
  pp_satisfied_models "Y" oc;
  if !hint <> None then
    fprintf oc "  hint[X]\n\n";
  fprintf oc "  // We have a valid application of the mapping\n";
  fprintf oc "  apply_map[X, Y, map]\n\n";
  pp_min_classes "threads" "SE" !min_thds "sthd" "ev - IW" oc;
  pp_max_classes "threads" "SE" !max_thds "sthd" "ev - IW" oc;
  pp_min_classes "locations" "SE" !min_locs "sloc" "R + W" oc;
  pp_max_classes "locations" "SE" !max_locs "sloc" "R + W" oc;
  fprintf oc "}\n\n";
  pp_hint_predicate oc;
  fprintf oc "run gp for exactly 1 M1/Exec, exactly 1 N1/Exec, %d SE, %d HE, 3 Int\n" !eventcount !eventcount2

let get_args () =
  let arch = ref None in
  let arch2 = ref None in
  let mapping_path = ref None in
  let speclist = [
      ("-satisfies", Arg.String (set_list_ref succ_paths),
       "Execution should satisfy this model (repeatable)");
      ("-violates", Arg.String (set_list_ref fail_paths),
       "Execution should violate this model (repeatable)");
      ("-arch", Arg.String (set_option_ref arch),
       "Type of executions being compared (required)");
      ("-events", Arg.Set_int eventcount, "Max number of events");
      ("-mapping", Arg.String (set_option_ref mapping_path),
       "An .als file representing a mapping between executions");
      ("-arch2", Arg.String (set_option_ref arch2),
       "Type of target execution (required iff -mapping is given)");
      ("-events2", Arg.Set_int eventcount2,
       "Max number of target events (required iff -mapping is given)");
      ("-expect", Arg.Int (set_option_ref expectation),
       "Expect to find this many unique solutions (optional)");
      ("-desc", Arg.Set_string description,
       "Textual description (optional)");
      ("-solver", Arg.Set_string solver,
       "Which SAT solver to use (optional). One of: sat4j, cryptominisat, glucose (default), plingeling, lingeling, minisatprover, or minisat.");
      ("-hint", Arg.String (set_option_ref hint),
       "An .als file containing a 'hint[X]' predicate (optional)");
      ("-minthreads", Arg.Set_int min_thds,
       "Find executions with at least N threads (default 0)");
      ("-maxthreads", Arg.Set_int max_thds,
       "Find executions with at most N threads");
      ("-threads",
       Arg.Int (fun i -> assert (0 < i); min_thds := i; max_thds := i),
       "Find executions with exactly N threads");
      ("-minlocations", Arg.Set_int min_locs,
       "Find executions with at least N locations (default 0)");
      ("-maxlocations", Arg.Set_int max_locs,
       "Find executions with at most N locations");
      ("-locations",
       Arg.Int (fun i -> assert (0 < i); min_locs := i; max_locs := i),
       "Find executions with exactly N locations");
      ("-iter", Arg.Set iter, "Option: find all solutions");
      ("-minimal", Arg.Set minimal, "Option: find minimal executions");
      ("-withinit", Arg.Set withinit,
       "Option: explicit initial writes");
    ] in
  let usage_msg =
    "Generating an Alloy file that can be run to compare two models.\nUsage: `comparator [options]`. There must be at least one -satisfies or -violates flag.\nOptions available:"
  in
  let bad_arg _ =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  Arg.parse speclist bad_arg usage_msg;
  let arch = match !arch with
    | Some arch -> Archs.parse_arch arch
    | None -> failwith "Expected one -arch"
  in
  let arch2 = match !arch2 with
    | Some arch -> Some (Archs.parse_arch arch)
    | None -> None
  in 
  arch, !mapping_path, arch2

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

(** Run Alloy on the generated compator file *)
let run_alloy comparator_als stamp =
  let alloy_cmd =
    sprintf "cd alloystar; ./runalloy_%s.sh"
	    (if !iter then "iter" else "once")
  in
  printf "Alloy started at %s.\n" (MyUnix.now ());
  flush stdout;
  let alloy_exit_code =
    Sys.command (sprintf "export SOLVER=%s; %s ../%s 0 ../xml/%s"
			 !solver alloy_cmd comparator_als stamp)
  in
  printf "Alloy finished at %s.\n" (MyUnix.now ());
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
    sprintf "python src/partition_hash.py %d xml/%s"
	    (!eventcount2 + !eventcount) stamp
  in
  let _ = Sys.command py_cmd in
  flush stdout;
  let dir = sprintf "xml/%s" stamp in
  let children = Sys.readdir dir in  
  (* subtracting one because of the hash file *)
  let num_solns = Array.length children - 1  
    (* count (fun i ->
	   Sys.file_exists (sprintf "xml/%s/%d_unique" stamp i)) *)
  in
  printf "Partitioned to %d unique solutions.\n" num_solns;
  flush stdout;
  num_solns

(** Invoke Tyler's python script to reduce stronger solutions found by Alloy *)
let reduce_tests stamp =
  printf "Removing duplicate solutions.\n";
  flush stdout;
  let py_cmd =
    sprintf "python src/reduce_tests.py %d xml/%s"
	    (!eventcount2 + !eventcount) stamp
  in
  let _ = Sys.command py_cmd in
  flush stdout;
  let dir = sprintf "xml/%s" stamp in
  let children = Sys.readdir dir in  
  (* subtracting one because of the hash file *)
  let num_solns = Array.length children - 1  
    (* count (fun i ->
	   Sys.file_exists (sprintf "xml/%s/%d_unique" stamp i)) *)
  in
  printf "Reduced to %d weakest solutions.\n" num_solns;
  flush stdout;
  num_solns


(** Convert the XML files generated by Alloy into Graphviz format *)
let xml_to_dot stamp i =
  let xml_dir = sprintf "xml/%s/%s_unique" stamp i in
  let xml_files = Sys.readdir xml_dir in
  assert (Array.length xml_files > 0);
  let xml_file = sprintf "%s/%s" xml_dir (xml_files.(0)) in 
  let dot_file = sprintf "dot/%s/test_%s.dot" stamp i in
  let gen_cmd = sprintf "./gen -Tdot -o %s %s" dot_file xml_file in
  let exit_status = Sys.command gen_cmd in
  if exit_status <> 0 then
    failwith "Conversion from .xml to .dot failed"

(** Convert the Graphviz files into PNG format for easy viewing *)
let dot_to_png stamp i =
  let dot_file = sprintf "dot/%s/test_%s.dot" stamp i in
  let png_file = sprintf "png/%s/test_%s.png" stamp i in
  let dot_cmd = sprintf "dot -Tpng -o %s %s" png_file dot_file in
  let exit_status = Sys.command dot_cmd in
  if exit_status <> 0 then
    failwith "Conversion from .dot to .png failed"

let main () =
  if not (Sys.file_exists "alloystar") then
    failwith "Please run me from the top-level directory of the repo";
  let arch, mapping_path, arch2 = get_args () in
  let unrolling_factor = 3 in
  let cat2als path =
    let path = Filename.concat Filename.parent_dir_name path in
    Cat2als.als_of_file false unrolling_factor path
  in
  begin match !succ_paths @ !fail_paths with
	| [] -> failwith "Expected at least one model"
	| paths -> List.iter cat2als paths
  end;
  pp_description ();
  let comparator_als = "comparator.als" in
  let pp =
    match mapping_path, arch2, !eventcount2 with
    | None, None, 0 -> pp_comparator arch
    | Some mapping_path, Some arch2, n when n>0 ->
       pp_comparator2 arch mapping_path arch2
    | _ ->
       failwith "Expected all or none of: -mapping, -arch2, -events2"
  in
  write_file comparator_als pp;
  let stamp = make_stamp () in
  mk_fresh_dir_in "xml" stamp;
  let num_solns_incl_dups = run_alloy comparator_als stamp in
  if num_solns_incl_dups = 0 then exit 0;
  let _num_unreduced_solns = remove_dups stamp in
  let num_solns = reduce_tests stamp in
  mk_fresh_dir_in "dot" stamp;
  mk_fresh_dir_in "png" stamp;
  let hash_file = sprintf "xml/%s/hashes.txt" stamp in
  let hashes = read_file hash_file in
  for i = 0 to num_solns - 1 do
    xml_to_dot stamp (List.nth hashes i);
    dot_to_png stamp (List.nth hashes i);
    printf "Converted solution %d of %d.\r" (i + 1) num_solns;
    flush stdout
  done;
  printf "\n";
  printf "Solution(s) are in png/%s.\n" stamp;
  let os =
    try Sys.getenv "OS"
    with Not_found ->
      failwith "Expected OS environment variable to be set"
  in
  (match os, num_solns with
   | "x86-mac", 1 -> ignore (Sys.command "open png/_latest/*.png")
   | "x86-mac", _ -> ignore (Sys.command "open png/_latest")
   | _, _ -> ());
  (match !expectation with
   | Some i when i <> num_solns ->
      failwith "Expected %d unique solutions, found %d" i num_solns
   | _ -> ());
  exit 0
       
let _ = main ()
