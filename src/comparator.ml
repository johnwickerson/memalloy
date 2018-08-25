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


open! Format
open! General_purpose

let customscript = ref None
let expect = ref (-1)
                 
let speclist =
  ["-customscript", Arg.String (set_option_ref customscript),
   "Custom comparator script (optional)";

   "-expect", Arg.Set_int expect,
   "Expect to find this many unique solutions (optional)";
  ]
                 
let get_args () =
  let usage_msg =
    "Top-level memalloy comparator.\nUsage: `comparator [options]`.\nOptions available:"
  in
  let bad_arg s =
    failwith "Unexpected argument '%s'" s
  in
  let speclist = Pp_comparator.speclist @ speclist in
  Arg.parse speclist bad_arg usage_msg

(* Arguments wanted:
- verbose
- expect
- desc
- batch
- customscript
- satisfies
- violates
- alsosatisfies
- arch
- events
- mapping
- arch2
- events2
- hint
- minthreads
- maxthreads
- threads
- minlocations
- maxlocations
- locations
- mintransactions
- maxtransactions
- minimal
- withinit
- exact
- emptytxns
- unroll
- timeout
- solver
- fencerels
- allowset
- filter 
 *)

let setup_result_dir () =
  (* TODO *)
  ""

let is_cat_file filename =
  Filename.check_suffix filename ".cat"
  
let main () =
  
  (* Stage 1: setup directory structure and generate comparator script *)
  let timer_start = MyTime.now () in
  
  let result_dir = setup_result_dir () in
  
  let xml_result_dir = Filename.concat result_dir "xml" in
  let dot_result_dir = Filename.concat result_dir "dot" in
  let png_result_dir = Filename.concat result_dir "png" in
  let lit_result_dir = Filename.concat result_dir "litmus" in
  let allow_result_dir = Filename.concat result_dir "allow" in
  let allow_xml_result_dir = Filename.concat allow_result_dir "xml" in
  let allow_dot_result_dir = Filename.concat allow_result_dir "dot" in
  let allow_png_result_dir = Filename.concat allow_result_dir "png" in
  let allow_lit_result_dir = Filename.concat allow_result_dir "litmus" in

  if !description <> "" then
    printf "\"%s\"\n" !description;
  
  let comparator_script = Filename.concat result_dir "comparator.als" in

  begin match !customscript with
  | Some s ->
     if not (fileexists s) then
       failwith "ERROR: Custom comparator script '%s' not found.\n" s;
     copyfile s comparator_script

  | None ->
     let all_models =
       !Pp_comparator.succ_paths @
         !Pp_comparator.also_succ_paths @
           !Pp_comparator.fail_paths
     in
     let convert_to_als model =
       if Filename.check_suffix model ".cat" then
         Cat2als.als_of_file false model
       else if Filename.check_suffix model ".als" then
         ()
       else failwith "ERROR: Unrecognised model type '%s'.\n" model
     in
     List.iter convert_to_als all_models;

     Pp_comparator.comparator_als := Some comparator_script;
     Pp_comparator.main ()
  end;
  
  let timer_end = MyTime.now () in
  let time_setup = timer_end - timer_start in
  
  (* Stage 2: Alloy solving *)
  let timer_start = MyTime.now () in
  run_alloy ();
  let xml_files =
    let result_files = Array.to_list (Sys.readdir xml_result_dir) in
    List.filter (fun f -> Filename.check_suffix f ".xml") result_files
  in
  let nsolutions = List.length xml_files in
  let timer_end = MyTime.now () in
  let time_alloy = timer_end - timer_start in
  printf "Alloy found %d solutions in %.2f sec.\n" nsolutions time_alloy;

  if nsolutions = 0 then
    if !expect > 0 then
      failwith "ERROR: Expected %d solutions, found 0.\n" !expect
    else
      exit 0;
  
  
  (* TODO: implement -filter flag here. *)
  (* TODO: implement allow-set here. *)

  (* Stage 4: Generate litmus test output *)
  let timer_start = MyTime.now () in

  let gen xml_file =
    Gen.run xml_file out_file Gen.als arch
  in
  List.iter gen xml_files;
  
     
     
  exit 0

let _ =
  if MyStr.endswith Sys.argv.(0) "comparator" then begin
      get_args ();
      main ()
    end
