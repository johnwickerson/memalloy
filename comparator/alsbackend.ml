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

(** Converting an execution into an Alloy predicate *)

open Format
open General_purpose
open Exec

(** Convert event set to Alloy constraint *)
let als_of_set oc (name, es) =
  fprintf oc "    X.%s = " name;
  if es = [] then fprintf oc "none"
  else fprintf_iter "+" pp_event_name oc es;
  fprintf oc "\n"

(** Convert event pair to Alloy expression *)
let als_of_pair oc (e,e') =
  fprintf oc "(%a->%a)" pp_event_name e pp_event_name e'

(** Convert event relation to Alloy constraint *)
let als_of_rel oc (name, ees) =
  fprintf oc "    X.%s = " name;
  if ees = [] then fprintf oc "none->none"
  else fprintf_iter "+" als_of_pair oc ees;
  fprintf oc "\n"	  

(** Convert execution to Alloy predicate *)
let als_of_execution oc x =
  let ev = get_set x "ev" in
  fprintf oc "pred hint[X:Exec] {\n";
  fprintf oc "  some disj %a : E {\n" (fprintf_iter "," pp_event_name) ev;
  List.iter (als_of_set oc) x.sets;
  List.iter (als_of_rel oc) x.rels;
  fprintf oc "  }\n";
  fprintf oc "}\n"
