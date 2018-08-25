(*
MIT License

Copyright (c) 2018 by John Wickerson and Nathan Chong.

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

(** Top-level entry for memory model comparison *)

open! Format
open! General_purpose

let customscript = ref None
let expect = ref (-1)
let batch = ref false
let default_solver = "glucose"
let solver = ref default_solver
let iter = ref false

let legal_solvers =
  ["sat4j"; "cryptominisat"; "plingeling"; "lingeling";
   "minisatprover"; "minisat"; "glucose"]
                 
let speclist =
  ["-customscript", Arg.String (set_option_ref customscript),
   "Custom comparator script (optional)";

   "-expect", Arg.Set_int expect,
   "Expect to find this many unique solutions (optional)";

   "-batch", Arg.Set batch,
   "Suppress GUI (optional)";

   "-solver", Arg.Set_string solver,
   sprintf "Which SAT solver to use (optional, default=%s). One of: [%s]"
     default_solver (String.concat "|" legal_solvers);

   "-iter", Arg.Set iter,
   "Find all solutions (optional)";
  ]
                 
let get_args () =
  let usage_msg =
    "Top-level memalloy comparator.\nUsage: `comparator [options]`.\nOptions available:"
  in
  let bad_arg s =
    Arg.usage speclist usage_msg;
    failwith "Unexpected argument '%s'" s
  in
  let speclist = Global_options.speclist @ Pp_comparator.speclist @ speclist in
  Arg.parse speclist bad_arg usage_msg;
  if not (List.mem !solver (legal_solvers @ [""])) then
    failwith "The -solver flag must be set to one of [%s] (currently \"%s\")"
      (String.concat "|" legal_solvers) !solver

let running_osx () =
  let ic = Unix.open_process_in "uname -s" in
  let os_name = input_line ic in
  close_in ic;
  if os_name = "Darwin" then true else false

let run_alloy alloystar_dir xml_dir comparator_script =
  let java_heap_size = opt "3k" iden (Sys.getenv_opt "JAVA_HEAP_SIZE") in
  let os =
    try Sys.getenv "OS" with
      Not_found -> failwith "Environment variable 'OS' not set -- aborting."
  in
  let legal_oses =
    ["x86-freebsd"; "x86-linux"; "x86-mac"; "x86-windows"; "amd64-linux"]
  in
  if not (List.mem os legal_oses) then
    failwith "Environment variable 'OS' must be set to one of [%s]."
      (String.concat "|" legal_oses);
  let solver_dir = MyFilename.concat [alloystar_dir; os] in
  let cmd =
    String.concat " "
      ["java";
       "-Xmx" ^ java_heap_size;
       "-Djava.library.path=" ^ solver_dir;
       "-Dout=" ^ xml_dir;
       "-Dsolver=" ^ !solver;
       sprintf "-Dhigherorder=%b" (!solver = "glucose");
       "-Dcmd=0";
       sprintf "-Diter=%b" !iter;
       "-Dhash=../mk_hash";
       "edu/mit/csail/sdg/alloy4whole/RunAlloy";
       comparator_script]
  in
  Unix.chdir alloystar_dir;
  Sys.catch_break true;
  try
    let status = Unix.system cmd in
    if status <> Unix.WEXITED 0 then failwith "Alloy was unsuccessful."
  with Sys.Break ->
    printf "\nWARNING: Alloy was interrupted.\n"
            
let run_dot dot_file png_file =
  ignore (Sys.command (sprintf "dot -Tpng -o %s %s" png_file dot_file))
  
let main () =
  
  (* Stage 1: setup directory structure and generate comparator script *)
  let timer_start = MyTime.now_ms () in
  
  let memalloy_root = Filename.dirname Sys.executable_name in
  let top_results_dir = MyFilename.concat [memalloy_root; "results"] in
  if not (Sys.file_exists top_results_dir) then
    Unix.mkdir top_results_dir 0o755;
  
  let stamp = MyTime.timestamp () in  
  let results_dir = MyFilename.concat [top_results_dir; stamp] in
  let alloystar_dir = MyFilename.concat [memalloy_root; "alloystar"] in
  let xml_dir = MyFilename.concat [results_dir; "xml"] in
  let png_dir = MyFilename.concat [results_dir; "png"] in
  let dot_dir = MyFilename.concat [results_dir; "dot"] in
  let als_dir = MyFilename.concat [results_dir; "als"] in
  let lit_dir = MyFilename.concat [results_dir; "litmus"] in

  List.iter (fun p -> Unix.mkdir p 0o755)
    [results_dir; xml_dir; png_dir; dot_dir; lit_dir; als_dir];

  let latest_symlink = MyFilename.concat [memalloy_root; "results"; "_latest"] in
  if Sys.file_exists latest_symlink then
    Sys.remove latest_symlink;
  Unix.symlink results_dir latest_symlink;
  
  if !Pp_comparator.description <> "" then
    printf "\"%s\"\n" !Pp_comparator.description;
  
  let comparator_script = MyFilename.concat [results_dir; "comparator.als"] in

  begin match !customscript with
  | Some s ->
     if not (Sys.file_exists s) then
       failwith "ERROR: Custom comparator script '%s' not found.\n" s;
     ignore (Sys.command (sprintf "cp %s %s" s comparator_script))

  | None ->
     let all_models =
       !Pp_comparator.succ_paths @
         !Pp_comparator.also_succ_paths @
           !Pp_comparator.fail_paths
     in
     let convert_to_als model =
       if Filename.check_suffix model ".cat" then
         ignore (Cat2als.als_of_file false model)
       else if Filename.check_suffix model ".als" then
         ()
       else
         failwith "ERROR: Unrecognised model type '%s'.\n" model
     in
     List.iter convert_to_als all_models;

     Pp_comparator.comparator_als := Some comparator_script;
     Pp_comparator.main ()
  end;
  
  let timer_end = MyTime.now_ms () in
  let time_setup = timer_end - timer_start in
  
  (* Stage 2: Alloy solving *)
  let timer_start = MyTime.now_ms () in
  run_alloy alloystar_dir xml_dir comparator_script;

  let xml_files =
    let result_files = Array.to_list (Sys.readdir xml_dir) in
    let xml_filenames =
      List.filter (fun f -> Filename.check_suffix f ".xml") result_files
    in
    List.map (fun f -> MyFilename.concat [xml_dir; f]) xml_filenames
  in
  let nsolutions = List.length xml_files in
  let timer_end = MyTime.now_ms () in
  let time_alloy = timer_end - timer_start in
  printf "Alloy found %d solutions in %.2f sec.\n"
    nsolutions (float_of_int time_alloy /. 1000.0);

  if nsolutions = 0 then
    if !expect > 0 then
      failwith "ERROR: Expected %d solutions, found 0.\n" !expect
    else
      exit 0;
  
  
  (* TODO: implement -filter flag here. *)
  (* TODO: implement allow-set here. *)

  (* Stage 4: Generate litmus test output *)
  let timer_start = MyTime.now_ms () in

  let arch = !Pp_comparator.arch_string in

  MyList.foreach xml_files (fun xml_file ->
      let test_name = Filename.chop_extension (Filename.basename xml_file) in
      let als_file = MyFilename.concat [als_dir; test_name ^ ".als"] in
      Gen.run xml_file als_file Gen.Als (Some arch));
  
  MyList.foreach xml_files (fun xml_file ->
      let test_name = Filename.chop_extension (Filename.basename xml_file) in
      let dot_file = MyFilename.concat [dot_dir; test_name ^ ".dot"] in
      Gen.run xml_file dot_file Gen.Dot (Some arch);
      let png_file = MyFilename.concat [png_dir; test_name ^ ".png"] in
      run_dot dot_file png_file);
  
  MyList.foreach xml_files (fun xml_file ->
      let test_name = Filename.chop_extension (Filename.basename xml_file) in
      let lit_file = MyFilename.concat [lit_dir; test_name ^ ".litmus"] in
      Gen.run xml_file lit_file Gen.Lit (Some arch));
  
  let timer_end = MyTime.now_ms () in
  let time_dump = timer_end - timer_start in

  if not !batch then begin
    if nsolutions = 1 then begin
      let first_litmus_test = Array.get (Sys.readdir lit_dir) 0 in
      let first_litmus_path = MyFilename.concat [lit_dir; first_litmus_test] in
      ignore (Sys.command (sprintf "cat %s" first_litmus_path));
      if running_osx () then
        let first_png = Array.get (Sys.readdir png_dir) 0 in
        let first_png_path = MyFilename.concat [png_dir; first_png] in
        ignore (Sys.command (sprintf "open %s" first_png_path))
      end
    else begin
      if running_osx () then
        ignore (Sys.command (sprintf "open %s" png_dir))
      end
    end;

  printf "setup time: %d ms\n" time_setup;
  printf "alloy time: %d ms\n" time_alloy;
  printf "dump time:  %d ms\n" time_dump;

  exit 0

  

let _ =
  if MyStr.endswith Sys.argv.(0) "comparator" then begin
      get_args ();
      main ()
    end
