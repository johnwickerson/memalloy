(*
MIT License

Copyright (c) 2017 by John Wickerson, Nathan Chong and Tyler Sorensen

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

open! Format
open! General_purpose

let succ_paths = ref []
let also_succ_paths = ref []
let fail_paths = ref []
let withinit = ref false
let fencerels = ref false
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

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let write_file als_file pp =
  let oc = open_out als_file in
  pp (formatter_of_out_channel oc);
  close_out oc

(** Parse architecture from given .cat file *)
let arch_of cat_path =
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  let model_type,_,_ = Cat_parser.main Cat_lexer.token lexbuf in
  Archs.parse_arch model_type

let pp_open_modules succ_sig fail_sig oc =
  let chop_extn = Filename.chop_extension in
  for i = 1 to List.length !succ_paths do
    let model = chop_extn (List.nth !succ_paths (i-1)) in
    fprintf oc "open ../../%s[%s] as M%d\n" model succ_sig i
  done;
  for i = 1 to List.length !fail_paths do
    let model = chop_extn (List.nth !fail_paths (i-1)) in
    fprintf oc "open ../../%s[%s] as N%d\n" model fail_sig i
  done;
  for i = 1 to List.length !also_succ_paths do
    let model = chop_extn (List.nth !also_succ_paths (i-1)) in
    let i = i + List.length !succ_paths in
    fprintf oc "open ../../%s[%s] as M%d\n" model fail_sig i
  done

let pp_extra_rels oc arch =
  let extra_rels = Archs.arch_rels_min arch in
  List.iter (fprintf oc ",%s") extra_rels

let pp_extra_rels' oc arch =
  let extra_rels = Archs.arch_rels_min arch in
  List.iter (fprintf oc ",%s'") extra_rels

let pp_extra_rels_minus j oc rels =
  let pp_rel i rel =
    if i=j
    then fprintf oc ",%s - (e1 -> e2)" rel
    else fprintf oc ",%s" rel in
  MyList.iteri pp_rel rels

let pp_extra_rels_with_type ev_sig oc arch =
  let rec pp_extra_rels_with_type = function
    | [] -> fprintf oc ""
    | [rel] -> fprintf oc ",%s:%s->%s" rel ev_sig ev_sig
    | rel::rels -> fprintf oc ",%s" rel; pp_extra_rels_with_type rels
  in pp_extra_rels_with_type (Archs.arch_rels_min arch)

let pp_extra_rels_with_type' ev_sig oc arch =
  let rec pp_extra_rels_with_type = function
    | [] -> fprintf oc ""
    | [rel] -> fprintf oc ",%s':%s->%s" rel ev_sig ev_sig
    | rel::rels -> fprintf oc ",%s'" rel; pp_extra_rels_with_type rels
  in pp_extra_rels_with_type (Archs.arch_rels_min arch)
			     
let pp_all_events_used ev_sig oc =
  fprintf oc "  // Every event is a read, write or a fence\n";
  fprintf oc "  %s in R[none,X,ad,cd,dd] + W[none,X,ad,cd,dd] + F[none,X,ad,cd,dd]\n\n" ev_sig

let pp_violated_models exec_sig oc =
  let pp_model i path =
    let arch = arch_of path in
    fprintf oc "  not(N%d/consistent[none,%s%a])\n"
	    (i+1) exec_sig pp_extra_rels arch;
    fprintf oc "  N%d/dead[none,%s%a]\n\n"
	    (i+1) exec_sig pp_extra_rels arch
  in
  MyList.iteri pp_model !fail_paths

let pp_satisfied_models exec_sig oc =
  let pp_model i path =
    let arch = arch_of path in
    fprintf oc "  M%d/consistent[none,%s%a]\n\n"
	    (i+1) exec_sig pp_extra_rels arch
  in
  MyList.iteri pp_model !succ_paths

let pp_satisfied_models' exec_sig oc =
  let pp_model i path =
    let arch = arch_of path in
    fprintf oc "  M%d/consistent[none,%s%a]\n\n"
	    (i+1) exec_sig pp_extra_rels' arch
  in
  MyList.iteri pp_model !succ_paths
	       
let pp_also_satisfied_models exec_sig oc =
  let pp_model i path =
    let arch = arch_of path in
    let i = i + List.length !succ_paths in
    fprintf oc "  M%d/consistent[none,%s%a]\n\n"
	    (i+1) exec_sig pp_extra_rels arch
  in
  MyList.iteri pp_model !also_succ_paths
  
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
  fprintf oc "pred gp [X:%a%a] {\n\n"
	  Archs.pp_Arch arch
	  (pp_extra_rels_with_type "E") arch;
  pp_all_events_used "E" oc;
  if !withinit then
    fprintf oc "  withinit[X]\n\n"
  else
    fprintf oc "  withoutinit[X]\n\n";
  if !fencerels then fprintf oc "  wf_%a[X%a]\n\n"
	  Archs.pp_Arch arch pp_extra_rels arch;
  pp_violated_models "X" oc;
  pp_satisfied_models "X" oc;
  pp_also_satisfied_models "X" oc;
  if !hint <> None then
    fprintf oc "  hint[X%a]\n\n" pp_extra_rels arch;
  fprintf oc "  not (some e : X.ev {\n";
  MyList.iteri (
      fun i path ->
      let arch = arch_of path in
      fprintf oc "    not(N%d/consistent[e,X%a])\n"
	      (i+1) pp_extra_rels arch;
      fprintf oc "    N%d/dead[e,X%a]\n"
	      (i+1) pp_extra_rels arch
    ) !fail_paths;
  MyList.iteri (
      fun i path ->
      let arch = arch_of path in
      fprintf oc "    M%d/consistent[e,X%a]\n"
	      (i+1) pp_extra_rels arch
    ) !succ_paths;
  fprintf oc "  })\n";
  
  let min_rels = Archs.arch_rels_min arch in
  let pp_rel j rel =
    fprintf oc "  not (some e1, e2 : X.ev {\n";
    fprintf oc "    (e1 -> e2) in %s\n" rel;
    if !fencerels then fprintf oc "    wf_%a[X%a]\n"
	    Archs.pp_Arch arch (pp_extra_rels_minus j) min_rels;
    MyList.iteri (
	fun i path ->
	let arch = arch_of path in
	let min_rels = Archs.arch_rels_min arch in
	fprintf oc "    not(N%d/consistent[none,X%a])\n" (i+1)
		(pp_extra_rels_minus j) min_rels
      ) !fail_paths;
    fprintf oc "  })\n"
  in
  MyList.iteri pp_rel min_rels;
  pp_min_classes "threads" "E" !min_thds "sthd" "ev - IW" oc;
  pp_max_classes "threads" "E" !max_thds "sthd" "ev - IW" oc;
  pp_min_classes "locations" "E" !min_locs "sloc" "R + W" oc;
  pp_max_classes "locations" "E" !max_locs "sloc" "R + W" oc;
  fprintf oc "}\n\n";
  pp_hint_predicate oc;
  fprintf oc "run gp for 1 Exec, %d E, 3 Int\n" !eventcount

(** [pp_comparator2 arch mapping_path arch2 oc] generates an Alloy file (sent to [oc]) that can be used to find an execution {i X} of type [arch] and an execution {i Y} of type [arch2] such that {i X} satisfies all the models in [!succ_paths], {i Y} violates all the models in [!fail_paths], and {i X} and {i Y} are related by the mapping in [mapping_path] *)
let pp_comparator2 arch mapping_path arch2 oc =
  if !withinit then
    failwith "Initial writes not supported in compiler mappings";
  if !description != "" then fprintf oc "/* %s */\n" !description;
  pp_open_modules "HE" "SE" oc;
  let mapping = Filename.chop_extension mapping_path in
  fprintf oc "open %s[SE,HE] as mapping\n\n" mapping;
  fprintf oc "sig SE, HE {}\n\n";
  fprintf oc "pred gp [X:%a%a, Y:%a%a, map:SE->HE] {\n\n"
	  Archs.pp_Arch arch
	  (pp_extra_rels_with_type "SE") arch
	  Archs.pp_Arch arch2
	  (pp_extra_rels_with_type' "HE") arch2;
  pp_all_events_used "SE" oc;
  fprintf oc "  withoutinit[X]\n";
  fprintf oc "  withoutinit[Y]\n\n";
  if !fencerels then fprintf oc "  wf_%a[X%a]\n\n"
	  Archs.pp_Arch arch pp_extra_rels arch;
  if !fencerels then fprintf oc "  wf_%a[Y%a]\n\n"
	  Archs.pp_Arch arch2 pp_extra_rels' arch2;
  pp_violated_models "X" oc;
  pp_satisfied_models' "Y" oc;
  pp_also_satisfied_models "X" oc;
  if !hint <> None then
    fprintf oc "  hint[X]\n\n";
  fprintf oc "  not (some e : X.ev {\n";
  MyList.iteri (
      fun i path ->
      let arch = arch_of path in
      fprintf oc "    not(N%d/consistent[e,X%a])\n"
	      (i+1) pp_extra_rels arch;
    ) !fail_paths;
  fprintf oc "  })\n";
  fprintf oc "  // We have a valid application of the mapping\n";
  fprintf oc "  apply_map[X%a, Y%a, map]\n\n"
	  pp_extra_rels arch
	  pp_extra_rels' arch2;
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
  let comparator_als = ref None in
  let speclist = [
      ("-fencerels", Arg.Set fencerels,
       "Encode fences as relations rather than events");
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
      ("-alsosatisfies", Arg.String (set_list_ref also_succ_paths),
       "Execution should also satisfy this model (repeatable; always refers to the 'source' model when checking compilers)");
      ("-expect", Arg.Int (set_option_ref expectation),
       "Expect to find this many unique solutions (optional)");
      ("-desc", Arg.Set_string description,
       "Textual description (optional)");
      ("-solver", Arg.Set_string solver,
       "Which SAT solver to use (optional). One of: sat4j, cryptominisat, glucose (default), plingeling, lingeling, minisatprover, or minisat.");
      ("-o", Arg.String (set_option_ref comparator_als),
       "Output .als file (optional, default stdout)");
      ("-hint", Arg.String (set_option_ref hint),
       "An .als file containing a 'hint[X]' predicate (optional)");
      ("-minthreads", Arg.Set_int min_thds,
       "Find executions with at least N threads (default 0)");
      ("-maxthreads", Arg.Set_int max_thds,
       "Find executions with at most N threads");
      ("-threads",
       Arg.Int (fun i -> min_thds := i; max_thds := i),
       "Find executions with exactly N threads");
      ("-minlocations", Arg.Set_int min_locs,
       "Find executions with at least N locations (default 0)");
      ("-maxlocations", Arg.Set_int max_locs,
       "Find executions with at most N locations");
      ("-locations",
       Arg.Int (fun i -> min_locs := i; max_locs := i),
       "Find executions with exactly N locations");
      ("-iter", Arg.Set iter, "Option: find all solutions");
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
  let oc = match !comparator_als with
    | Some f -> open_out f
    | None -> stdout
  in
  arch, !mapping_path, arch2, oc

(** Print a description of the comparison being undertaken *)
let pp_description () =
  printf "\n";
  printf "\n";
  printf "==============================\n";
  printf "%s\n" !description;
  printf "------------------------------\n"      

let main () =
  let arch, mapping_path, arch2, oc = get_args () in
  if !succ_paths @ !fail_paths = [] then
    failwith "Expected at least one -satisfies or -violates flag";
  let pp =
    match mapping_path, arch2, !eventcount2 with
    | None, None, 0 -> pp_comparator arch
    | Some mapping_path, Some arch2, n when n>0 ->
       pp_comparator2 arch mapping_path arch2
    | _ ->
       failwith "Expected all or none of: -mapping, -arch2, -events2"
  in
  pp (formatter_of_out_channel oc);
  close_out oc;
  exit 0
       
let _ = main ()

