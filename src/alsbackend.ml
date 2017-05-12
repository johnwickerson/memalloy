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

open! Format
open! General_purpose
open Exec

(** Equality constraint on sets *)
let als_of_set oc (name, es) =
  if es = [] then fprintf oc "    X.%s = none" name
  else fprintf oc "    X.%s = %a" name
         (MyList.pp_gen "+" Event.pp) es

(** Convert event pair to Alloy expression *)
let als_of_pair oc (e,e') =
  fprintf oc "(%a->%a)" Event.pp e Event.pp e'

(** Equality constraint on relations *)
let als_of_rel oc (name, ees) =
  if ees = [] then fprintf oc "    X.%s = none->none" name
  else fprintf oc "    X.%s = %a" name
         (MyList.pp_gen "+" als_of_pair) ees

(** [als_of_execution pred_name oc x] generates on [oc] a constraint called [pred_name] that holds when a given execution is isomorphic to [x] *)
let als_of_execution pred_name oc x =
  let ev = get_set x "EV" in
  fprintf oc "pred %s[X:Exec] {\n" pred_name;
  fprintf oc "  some disj %a : E {\n%a%a\n  }\n"
    (MyList.pp_gen "," Event.pp) ev
    (MyList.pp_gen "\n" als_of_set) x.sets
    (MyList.pp_gen "\n" als_of_rel) x.rels;
  fprintf oc "}\n"
  
(** Superset constraint on sets *)
let als_of_set_super oc (name, es) =
  let op = if List.mem name Archs.min_sets then "in" else "=" in
  if es = []
  then fprintf oc "    none %s X.%s" op name
  else fprintf oc "    %a %s X.%s"
         (MyList.pp_gen "+" Event.pp) es op name

(** Superset constraint on relations *)
let als_of_rel_super oc (name, ees) =
  let op = if List.mem name Archs.min_rels then "in" else "=" in
  if ees = []
  then fprintf oc "    none->none %s X.%s" op name
  else fprintf oc "    %a %s X.%s"
         (MyList.pp_gen "+" als_of_pair) ees op name


(** [als_of_execution_notsuper pred_name oc x] generates on [oc] a constraint called [pred_name] that holds when a given execution is not isomorphic to any super-execution of [x] *)
let als_of_execution_notsuper pred_name oc x =
  let ev = get_set x "EV" in
  fprintf oc "pred %s[X:Exec] {\n" pred_name;
  fprintf oc "  not some disj %a : E {\n%a%a\n }\n"
    (MyList.pp_gen "," Event.pp) ev
    (MyList.pp_gen "\n" als_of_set_super) x.sets
    (MyList.pp_gen "\n" als_of_rel_super) x.rels;
  fprintf oc "}\n"
  
(** Subset constraint on sets *)
let als_of_set_sub oc (name, es) =
  let op = if List.mem name Archs.min_sets then "in" else "=" in
  if es = []
  then fprintf oc "    X.%s %s none" name op
  else fprintf oc "    X.%s %s %a"
         name op (MyList.pp_gen "+" Event.pp) es

(** Subset constraint on relations *)
let als_of_rel_sub oc (name, ees) =
  let op = if List.mem name Archs.min_rels then "in" else "=" in
  if ees = []
  then fprintf oc "    X.%s %s none->none" name op
  else fprintf oc "    X.%s %s %a"
         name op (MyList.pp_gen "+" als_of_pair) ees

(** [als_of_execution_strictsub pred_name oc x] generates on [oc] a constraint called [pred_name] that holds when a given execution is not isomorphic to any super-execution of [x] *)
let als_of_execution_strictsub pred_name oc x =
  let ev = get_set x "EV" in
  fprintf oc "pred %s[X:Exec] {\n" pred_name;
  fprintf oc "  some disj %a : E {\n%a%a\n  not (%a && %a)\n  }\n"
    (MyList.pp_gen "," Event.pp) ev
    (MyList.pp_gen "\n" als_of_set_sub) x.sets
    (MyList.pp_gen "\n" als_of_rel_sub) x.rels
    (MyList.pp_gen " && " als_of_set) x.sets
    (MyList.pp_gen " && " als_of_rel) x.rels;
  fprintf oc "}\n"
