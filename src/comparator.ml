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

let running_osx () =
  let ic = Unix.open_process_in "uname -s" in
  let os_name = input_line ic in
  ignore (Unix.close_process_in ic);
  if os_name = "Darwin" then true else false
  
let run_alloy alloystar_dir xml_dir comparator_script iter solver quiet =
  let java_heap_size = opt "3g" iden (Sys.getenv_opt "JAVA_HEAP_SIZE") in
  let os =
    try Sys.getenv "OS" with Not_found ->
      if running_osx () then "x86-mac" else
        failwith
          "ERROR: Environment variable 'OS' not set. Try `source configure.sh`."
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
       sprintf "-Dquiet=%B" quiet;
       "-Dsolver=" ^ solver;
       sprintf "-Dhigherorder=%b" (solver = "glucose");
       "-Dcmd=0";
       sprintf "-Diter=%B" iter;
       "-Dhash=../mk_hash";
       "edu/mit/csail/sdg/alloy4whole/RunAlloy";
       comparator_script]
  in
  Unix.chdir alloystar_dir;
  Sys.catch_break true;
  begin try
    let status = Unix.system cmd in
    if status <> Unix.WEXITED 0 then failwith "Alloy was unsuccessful."
  with Sys.Break ->
    printf "\nWARNING: Alloy was interrupted.\n"
  end;
  Sys.catch_break false

let customscript = ref None
let expect = ref (-1)
let batch = ref false
let default_solver = "glucose"
let solver = ref default_solver
let iter = ref false
let allowset = ref false
let filter_path = ref ""
             
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

   "-allowset", Arg.Set allowset,
   "Generate the allow set too (optional)";

   "-filter", Arg.Set_string filter_path,
   "Only keep solutions that satisfy this model (optional)";
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
      (String.concat "|" legal_solvers) !solver;
  if !iter then Pp_comparator.minimal := true

let append_line_to_file filename str =
  let oc = open_out_gen [Open_append; Open_creat] 0o755 filename in
  output_string oc (str ^ "\n");
  close_out oc
            
let run_dot dot_file png_file =
  ignore (Sys.command (sprintf "dot -Tpng -o %s %s" png_file dot_file))
  
let main () =
  
  (* Stage 1: setup directory structure and generate comparator script *)
  MyTime.start_timer ("setup");

  let ppc_config = Pp_comparator.build_config () in
  
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

  let allow_dir = MyFilename.concat [results_dir; "allow"] in
  let allow_xml_dir = MyFilename.concat [allow_dir; "xml"] in
  let allow_png_dir = MyFilename.concat [allow_dir; "png"] in
  let allow_dot_dir = MyFilename.concat [allow_dir; "dot"] in
  let allow_als_dir = MyFilename.concat [allow_dir; "als"] in
  let allow_lit_dir = MyFilename.concat [allow_dir; "litmus"] in

  if !allowset then
    List.iter (fun p -> Unix.mkdir p 0o755)
      [allow_dir; allow_xml_dir; allow_png_dir;
       allow_dot_dir; allow_lit_dir; allow_als_dir];
  
  let latest_symlink =
    MyFilename.concat [memalloy_root; "results"; "_latest"]
  in
  if Sys.file_exists latest_symlink then
    Sys.remove latest_symlink;
  Unix.symlink results_dir latest_symlink;
  
  if ppc_config.description <> "" then
    printf "\"%s\"\n" ppc_config.description;
  
  let comparator_als = MyFilename.concat [results_dir; "comparator.als"] in

  begin match !customscript with
  | Some s ->
     if not (Sys.file_exists s) then
       failwith "ERROR: Custom comparator script '%s' not found.\n" s;
     ignore (Sys.command (sprintf "cp %s %s" s comparator_als))

  | None ->
     let all_models =
       ppc_config.satisfies_paths @
         ppc_config.also_satisfies_paths @
           ppc_config.violates_paths
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

     Pp_comparator.main ppc_config (Some comparator_als);
  end;
  
  MyTime.stop_timer();
  
  (* Stage 2: Alloy solving *)
  MyTime.start_timer("alloy");
  run_alloy alloystar_dir xml_dir comparator_als !iter !solver false;
  let nsolutions = Array.length (Sys.readdir xml_dir) in
  MyTime.stop_timer();
  printf "Alloy found %d solutions.\n" nsolutions;

  if nsolutions = 0 then
    if !expect > 0 then
      failwith "ERROR: Expected %d solutions, found 0.\n" !expect
    else
      exit 0;

  (* Stage 4: Generate als output *)
  MyTime.start_timer("dump als");

  printf "Generating als files.\n%!";
  let generate_als dir xml_file =
    let test_name = Filename.chop_extension (Filename.basename xml_file) in
    let als_file = MyFilename.concat [dir; "als"; test_name ^ ".als"] in
    Gen.als_name := test_name;
    Gen.run xml_file als_file Gen.Als ppc_config.arch
  in
  MyFilename.iter (generate_als results_dir) xml_dir;
  MyTime.stop_timer();

  (* Stage 3a: Filter executions to remove those that satisfy the model given in the -filter parameter *)
  if !filter_path <> "" then (
    MyTime.start_timer("filtering");
    printf "Removing executions that satisfy the -filter model";
    MyFilename.iter (fun xml_path ->
        let test_name =
          Filename.chop_extension (Filename.basename xml_path)
        in
        let hint_file =
          MyFilename.concat [als_dir; test_name ^ ".als"]
        in
        let comparator_als =
          MyFilename.concat [results_dir; "tmp_comparator.als"]
        in
        let new_config = {
            Pp_comparator.default_config with
            satisfies_paths = [!filter_path];
            hints = [hint_file];
            withinit = ppc_config.withinit;
            eventcount = ppc_config.eventcount;
            arch = ppc_config.arch;
          }
        in
        Pp_comparator.main new_config (Some comparator_als);
        let tmp_xml_file = MyFilename.concat [results_dir; "test_0.xml"] in
        if Sys.file_exists tmp_xml_file then
          Sys.remove tmp_xml_file;
        run_alloy alloystar_dir results_dir comparator_als false "sat4j" true;
        if Sys.file_exists tmp_xml_file then
          printf ".%!"
        else (
          printf "!%!";
          Sys.remove hint_file;
          Sys.remove xml_path
        )
      ) xml_dir;
    printf "\n";
    MyTime.stop_timer();
  );  
  
  (* Stage 3b: Generate allow-set *)
  if !allowset then (
    MyTime.start_timer("allowset");
    Weaken.hashes_seen := [];
    Weaken.run allow_xml_dir xml_dir;
    MyTime.stop_timer();
    let nsolutions = Array.length (Sys.readdir allow_xml_dir) in
    printf "Generated %d allowed solutions.\n%!" nsolutions;
    MyFilename.iter (generate_als allow_dir) allow_xml_dir
  );

  (* Stage 5: Check the allow-set *)
  if !allowset && List.length ppc_config.violates_paths = 1 then (
    MyTime.start_timer("check allow");
    printf "Checking validity of allow-tests";
    MyFilename.iter (fun allow_xml_path ->
        let test_name =
          Filename.chop_extension (Filename.basename allow_xml_path)
        in
        let violates_path = List.hd ppc_config.violates_paths in
        let hint_file = MyFilename.concat [allow_als_dir; test_name ^ ".als"] in
        let comparator_als =
          MyFilename.concat [results_dir; "allow_comparator.als"]
        in
        let new_config = {
            Pp_comparator.default_config with
            satisfies_paths = [violates_path];
            hints = [hint_file];
            withinit = ppc_config.withinit;
            eventcount = ppc_config.eventcount;
            arch = ppc_config.arch;
          }
        in
        Pp_comparator.main new_config (Some comparator_als);
        let tmp_xml_file = MyFilename.concat [allow_dir; "test_0.xml"] in
        if Sys.file_exists tmp_xml_file then
          Sys.remove tmp_xml_file;
        run_alloy alloystar_dir allow_dir comparator_als false "sat4j" true;
        if Sys.file_exists tmp_xml_file then
          printf ".%!"
        else (
          printf "!%!";
          Sys.remove hint_file;
          Sys.remove allow_xml_path
        )
      ) allow_xml_dir;
    MyTime.stop_timer()
  );

  MyTime.start_timer("dump png");
  printf "Generating dot and png files.\n%!";
  let generate_dot_png dir xml_file =
    let test_name = Filename.chop_extension (Filename.basename xml_file) in
    let dot_file = MyFilename.concat [dir; "dot"; test_name ^ ".dot"] in
    Gen.run xml_file dot_file Gen.Dot ppc_config.arch;
    let png_file = MyFilename.concat [dir; "png"; test_name ^ ".png"] in
    run_dot dot_file png_file
  in
  MyFilename.iter (generate_dot_png results_dir) xml_dir;
  if !allowset then begin
      printf "Generating dot and png files for allow-set.\n%!";
      MyFilename.iter (generate_dot_png allow_dir) allow_xml_dir;
    end;
  MyTime.stop_timer();

  MyTime.start_timer("dump litmus");
  printf "Generating litmus files.\n%!";
  let generate_lit dir xml_file =
    let all_file = MyFilename.concat [dir; "litmus"; "@all"] in
    let test_name = Filename.chop_extension (Filename.basename xml_file) in
    let lit_file = test_name ^ ".litmus" in
    let lit_path = MyFilename.concat [dir; "litmus"; lit_file] in
    append_line_to_file all_file lit_file;
    Gen.run xml_file lit_path Gen.Lit ppc_config.arch
  in
  MyFilename.iter (generate_lit results_dir) xml_dir;
  if !allowset then begin
      printf "Generating litmus files for allow-set.\n%!";
      MyFilename.iter (generate_lit allow_dir) allow_xml_dir 
    end;
  MyTime.stop_timer();
  
  if not !batch then
    if nsolutions = 1 then begin
        let first_litmus_test = Array.get (Sys.readdir lit_dir) 0 in
        let first_litmus_path = MyFilename.concat [lit_dir; first_litmus_test] in
        ignore (Sys.command (sprintf "cat %s" first_litmus_path));
        if running_osx () then
          let first_png = Array.get (Sys.readdir png_dir) 0 in
          let first_png_path = MyFilename.concat [png_dir; first_png] in
          ignore (Sys.command (sprintf "open %s" first_png_path))
      end
    else
      if running_osx () then
        ignore (Sys.command (sprintf "open %s" png_dir));
  
  if !expect >= 0 && !expect != nsolutions then
    failwith "ERROR: Expected %d solutions, found %d." !expect nsolutions;
  
  MyTime.report_timings ();

  exit 0

let _ =
  if MyStr.endswith Sys.argv.(0) "comparator" then begin
      get_args ();
      main ()
    end
