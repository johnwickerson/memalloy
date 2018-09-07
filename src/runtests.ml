(*
MIT License

Copyright (c) 2018 by John Wickerson.

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

(** Run tests; report outcomes in a nice table. *)

open! Format
open! General_purpose


type test_input = {
    id : int;
    name : string;
    solver : string;
    events : int;
    events2 : int option;
    base_cmd : string;
    slow : bool;
  }

let default_test = {
    id = 0; name = ""; solver = "glucose"; events = -1;
    events2 = None; base_cmd = ""; slow = false;
  }
               
type test_result = {
    encoding_time : float;
    solve_time : float;
    found : int;
    passed : bool;
  }


let withslow = ref false
    
let run tests =
  let parse_test id t =
    let name = ref None in
    let events = ref None in
    let events2 = ref None in
    let solver = ref "glucose" in
    let slow = ref false in
    let rec parse_args = function
      | [] -> ()
      | "-desc" :: s :: args ->
         name := Some s; parse_args args
      | "-events" :: s :: args ->
         events := Some (int_of_string s); parse_args args
      | "-events2" :: s :: args ->
         events2 := Some (int_of_string s); parse_args args
      | "-solver" :: s :: args ->
         solver := s; parse_args args
      | "#slow" :: args ->
         slow := true; parse_args args
      | _ :: args -> parse_args args
    in
    parse_args (Str.split (Str.regexp "[ \t]+") t);
    let name = the !name in
    let events = the !events in
    let events2 = !events2 in
    let solver = !solver in
    let slow = !slow in
    { id; name; solver; events; events2; base_cmd = t; slow; }
  in
  let tests = List.mapi parse_test tests in
  let tests = List.filter (fun t -> not t.slow || !withslow) tests in
  let run_test t =
    let events2 = match t.events2 with
      | None -> ""
      | Some e2 -> sprintf " -events2 %d" e2
    in
    let cmd =
      sprintf "%s -events %d -solver %s%s -batch"
        t.base_cmd t.events t.solver events2
    in
    printf "%s: Running test %d (%s):\n%!" (MyTime.now ()) t.id t.name;
    printf "`%s`\n%!" cmd;
    let ic = Unix.open_process_in cmd in
    let encoding_time =
      let rec extract_encoding_time () =
        let line = input_line ic in
        try Scanf.sscanf line "%_s Translation took %fs" (fun i -> i)
        with Scanf.Scan_failure _ -> extract_encoding_time ()
      in extract_encoding_time ()
    in
    let solve_time =
      let rec extract_solve_time () =
        let line = input_line ic in
        try Scanf.sscanf line "Solving took %fs" (fun i -> i)
        with Scanf.Scan_failure _ -> extract_solve_time ()
      in extract_solve_time ()
    in
    let found =
      let rec extract_found () =
        let line = input_line ic in
        try Scanf.sscanf line "No solution" 0
        with Scanf.Scan_failure _ ->
          try Scanf.sscanf line "Alloy found %d solutions" (fun i -> i)
          with Scanf.Scan_failure _ ->
            extract_found ()
      in extract_found ()
    in
    (try while true do ignore (input_line ic) done
     with End_of_file -> ());
    let exitcode = Unix.close_process_in ic in
    let passed = (exitcode = Unix.WEXITED 0) in
    t, {encoding_time; solve_time; found; passed; }
  in
  let results = List.map run_test tests in
  let print_result (t,r) =
    let name = MyStr.pad_right 28 t.name in
    let solver = match t.solver with
      | "plingeling" -> "P"
      | "glucose" -> "G"
      | "minisat" -> "M"
      | _ -> failwith "Unrecognised solver"
    in
    let solver = MyStr.pad_right 7 solver in
    let events = match t.events2 with
      | None -> sprintf "%d" t.events
      | Some e2 -> sprintf "%d+%d" t.events e2
    in
    let events = MyStr.pad_right 7 events in
    let t_enc = sprintf "%.2f" r.encoding_time in
    let t_enc = MyStr.pad_right 8 t_enc in
    let t_sol = sprintf "%.2f" r.solve_time in
    let t_sol = MyStr.pad_right 8 t_sol in
    let passed = if r.passed then "" else "*" in
    printf "%2d | %s| %s| %s| %s| %s| %4d%s\n"
      t.id name solver events t_enc t_sol r.found passed
  in
  printf "Id | Name                        | Solver | Events | T_enc   | T_sol   | Found\n";
  printf "---+-----------------------------+--------+--------+---------+---------+------\n";
  List.iter print_result results
  
let main () =
  let tests_path = ref [] in
  let speclist = [
      ("-withslow", Arg.Set withslow, "Include slow tests");
    ] in
  let usage_msg = "Usage: `runtests [options]`.\n\
                   Options available:"
  in
  Arg.parse speclist (set_list_ref tests_path) usage_msg;
  let bad_arg () =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  let tests_path =
    try MyList.the !tests_path with Not_found -> bad_arg ()
  in
  let ic = open_in tests_path in
  let tests = ref [] in
  begin
    try while true do tests := input_line ic :: !tests done
    with End_of_file -> ()
  end;
  close_in ic;
  let tests = List.rev (!tests) in
  run tests
  
let _ =
  if MyStr.endswith Sys.argv.(0) "runtests" then
    main ()
