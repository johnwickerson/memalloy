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
		  
let get_args () =
  let model_paths : string list ref = ref [] in
  let speclist = [
      ("-withinit", Arg.Set withinit, "Explicit initial writes");
      ("-relacq", Arg.Set relacq, "Only release/acquire fragment");
      ("-simplepost", Arg.Set simplepost, "Postcondition need not read shared locations");
      ("-normws", Arg.Set normws, "Avoid RMW events");
      ("-totalsb", Arg.Set totalsb, "Total sb per thread");
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
		  
let main () =
  let model1_path, model2_path = get_args () in
  check_args (model1_path, model2_path);

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
  printf "open %s[E] as M1\n" (Filename.chop_extension model1_path);
  printf "open %s[E] as M2\n\n" (Filename.chop_extension model2_path);
  printf "sig E {}\n\n";
  printf "pred gp [X:%s] {\n\n" arch;
  if !withinit then
    printf "  withinit[X]\n\n"
  else
    printf "  withoutinit[X]\n\n"; 
  printf "  not(M1/consistent[none,X])\n";
  printf "  M1/dead[none,X]\n";
  printf "  M2/consistent[none,X]\n\n";
  if !relacq then (
    printf "  // Stay within the rel/acq fragment\n";
    printf "  R[none,X] in acq[none,X]\n";
    printf "  W[none,X] in rel[none,X]\n";
    printf "  no sc[none,X]\n"
  );
  if !normws then (
    printf "  // Avoid RMW events\n";
    printf "  no_RMWs[none,X]\n"
  );
  if !totalsb then (
    printf "  // Total sb per thread\n";
    printf "  total_sb[none,X]\n"
  );
  if !simplepost then (
    printf "  // The postcondition need not read shared locations\n";
    printf "  co[none,X] in (rc[rf[none,X]]) . (rc[(sb[none,X]) . (rc[~(rf[none,X])])])\n"
  );
  printf "}\n\n";
  printf "run gp for 1 Exec, 6 E, 3 Int\n";  
  exit 0
    
let _ = main ()
	    
(*

./comparator -relacq -simplepost -normws -totalsb ../models_als/c11_sra.als ../models_als/c11_simp.als > ../compare.als



 *)
