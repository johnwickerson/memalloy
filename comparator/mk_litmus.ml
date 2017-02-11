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

(** Generating a litmus test from an execution *)

open Format
open General_purpose
open Exec
open Litmus

(** [mk_instr x maps reg_map e] returns an instruction corresponding to the event [e] in the execution [x], by looking up values, registers, and so on, in [maps] and [reg_map]. *)
let mk_instr x maps reg_map e =
  let ignored_attrs = ["ev";"R";"W";"F";"IW"] in
  let attrs = diff (get_sets x e) ignored_attrs in
  let ad = get_rel x "ad" in
  let dd = get_rel x "dd" in
  let ev = get_set x "ev" in
  let a_deps = List.filter (fun e' -> List.mem (e',e) ad) ev in
  let d_deps = List.filter (fun e' -> List.mem (e',e) dd) ev in
  let a_regs = List.map (fun e -> List.assoc e reg_map) a_deps in
  let d_regs = List.map (fun e -> List.assoc e reg_map) d_deps in
  match List.mem e (get_set x "R"),
	List.mem e (get_set x "W"),
	List.mem e (get_set x "F")
  with
  | true, true, false ->
     let loc = List.assoc e maps.loc_map in
     let rval = List.assoc e maps.rval_map in
     let wval = List.assoc e maps.wval_map in
     let loc_expr = mk_expr loc a_regs in
     let wval_expr = mk_expr wval d_regs in
     Cas (loc_expr, rval, wval_expr), attrs
  | true, false, false ->
     let loc = List.assoc e maps.loc_map in
     let reg = List.assoc e reg_map in
     let loc_expr = mk_expr loc a_regs in
     Load (reg, loc_expr), attrs
  | false, true, false ->
     let loc = List.assoc e maps.loc_map in
     let wval = List.assoc e maps.wval_map in
     let loc_expr = mk_expr loc a_regs in
     let wval_expr = mk_expr wval d_regs in
     Store (loc_expr, wval_expr), attrs
  | false, false, true ->
     Fence, attrs
  | _ -> assert false

(** If the list of events [es] comprises a single event, [partition_seq sb es] returns a basic component containing just that event. Otherwise, [partition_seq sb es] partitions the events in [es] into a sequence of components, such that whenever two events are in consecutive components, they are ordered by [sb]. *)
let rec partition_seq sb = function
  | [] -> assert false
  | [e] -> Basic e
  | es ->
     let map = partition false sb es in
     let classes = val_list (invert_map map) in
     let comparator es es' =
       if exists_pair (fun e e' -> List.mem (e,e') sb) es es'
       then -1 else 1
     in
     let classes = List.sort comparator classes in
     Seq (List.map (partition_par sb) classes)

(** [partition_par sb es] partitions the events in [es] into a collection of unsequenced components, such that whenever two events are in different components, they are unrelated (in either direction) by [sb]. *)
and partition_par sb = function
  | [] -> assert false
  | es ->
     let map = partition true sb es in
     let classes = val_list (invert_map map) in
     Unseq (List.map (partition_seq sb) classes)

let litmus_of_execution x =
  let maps = resolve_exec x in
  let locs = key_list (invert_map maps.loc_map) in
  let thd_classes = val_list (invert_map maps.thd_map) in
  let sb = get_rel x "sb" in
  let thds = List.map (partition_seq sb) thd_classes in
  let mk_reg_map (i,res) e = (i+1, (e,i)::res) in
  let reg_evts = diff (get_set x "R") (get_set x "W") in
  let _,reg_map = List.fold_left mk_reg_map (0,[]) reg_evts in
  let thds = List.map (map_component (mk_instr x maps reg_map)) thds in
  let find_reg_val e =
    try
      let e', _ = List.find (fun (_,e') -> e'=e) (get_rel x "rf") in
      strong_assoc maps.wval_map e'
    with Not_found -> 0
  in
  let find_reg e = Reg (strong_assoc reg_map e) in
  let reg_post =
    List.map (fun e -> (find_reg e, find_reg_val e)) (get_set x "R")
  in
  let final_wval (l,es) =
    let ws = inter (get_set x "W") es in
    let co_after e e' = List.mem (e,e') (get_rel x "co") in
    let co_maximal e = not (List.exists (co_after e) ws) in
    let wval =
      try strong_assoc maps.wval_map (List.find co_maximal ws)
      with Not_found -> 0
    in (Loc l, wval)
  in
  let loc_post = List.map final_wval (invert_map maps.loc_map) in
  {locs = locs; thds = thds; post = reg_post @ loc_post}
