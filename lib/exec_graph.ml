(*
MIT License

Copyright (c) 2017 by John Wickerson and Tyler Sorensen.

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

(** Simple graph-based representation of executions *)

open Format
open General_purpose
	  
(** An execution is a set of named event sets and a set of named event relations *)
type t = {
    exec_sets : (string * string MySet.t) MySet.t;
    exec_rels : (string * string Rel.t) MySet.t;
  }

(** Basic pretty-printing of executions *)
let pp_exec oc exec =
  let pp_set (name,tuples) =
    fprintf oc "Set: %s={%a}\n" name
	    (MyList.pp_gen "," pp_str) tuples
  in
  let pp_rel (name,tuples) =
    fprintf oc "Rel: %s={%a}\n" name
	    (MyList.pp_gen "," (fparen (Pair.pp pp_str "," pp_str)))
	    tuples
  in
  List.iter pp_set exec.exec_sets;
  List.iter pp_rel exec.exec_rels

(** The empty execution *)
let empty = { exec_sets = []; exec_rels = [] }

(** Apply [f] to every event in every set of execution [x] *)
let map_exec_sets f x =
  List.map (fun (s_name,s) -> s_name, List.map f s) x.exec_sets

(** Apply [f] to both events in every pair in every relation of execution [x] *)
let map_exec_rels f x =
  List.map (fun (r_name,r) ->
      r_name, List.map (fun (e1,e2) -> f e1, f e2) r
    ) x.exec_rels
  

