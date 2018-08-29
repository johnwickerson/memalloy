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

(** Printing a litmus test as a C program *)

open! Format
open! General_purpose

let mk_indent oc i = fprintf oc "%s" (String.make i ' ')
  
(** Pretty-printing of components *)     
let rec pp_component i oc = function
  | Litmus.Basic b ->
     fprintf oc "%a%a;\n" mk_indent i (pp_instr indent) b
  | Litmus.If (r,v,cs) ->
     fprintf oc "%af (%a == %a) {\n" mk_indent i
       Register.pp r Value.pp v;
     List.iter (pp_component (indent+2) oc) cs;
     fprintf oc "%a}\n" mk_indent i
  

let pp oc lt =

  (* Include standard headers. *)
  fprintf oc "#include <stdio.h>\n";
  fprintf oc "#include <pthread.h>\n";
  fprintf oc "\n";

  (* Declare global variables. *)
  List.iter (fprintf oc "atomic_int %a;\n" MyLocation.pp) lt.Litmus.locs;
  fprintf oc "\n";

  (* Print a function for each thread. *)
  let pp_thd tid cs =
    fprintf oc "// Thread %d" tid;
    fprintf oc "void *thread%d (void *unused) {\n" tid;
    List.iter (pp_component 2 oc) cs;
    fprintf oc "}\n";
    fprintf oc "\n";
  in
  List.iteri pp_thd lt.Litmus.thds;

  (* Begin main() routine. *)
  fprintf oc "int main() {\n";
  fprintf oc "\n";

  (* Declare thread-id variables. *)
  let tids = List.mapi (fun i _ -> i) thds in
  List.iter (fprintf oc "  pthread_t tid%d;\n") tids;
  fprintf oc "\n";

  (* Initialise global variables. *)
  List.iter (fprintf oc "  %a = 0;\n" MyLocation.pp) lt.Litmus.locs;
  fprintf oc "\n";

  (* Launch threads. *)
  List.iter (fun tid ->
      fprintf oc "  pthread_create(&tid%d, NULL, thread%d, NULL);\n" tid tid
    ) tids;
  fprintf oc "\n";

  (* Join threads. *)
  List.iter (fprintf oc "  pthread_join(tid%d, NULL);\n") tids;
  fprintf oc "\n";

  (* End main() routine. *)
  fprintf oc "  return 0;\n";
  fprintf oc "}\n"  
