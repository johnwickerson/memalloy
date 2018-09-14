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

open! Format
open! General_purpose

(** [mk_instr x maps reg_map e] returns a component corresponding to the event [e] in the execution [x], by looking up values, registers, and so on, in [maps] and [reg_map]. *)
let mk_instr x maps reg_map e =
  let ignored_attrs = ["EV";"R";"W";"F";"IW"] in
  let attrs = MySet.diff (Exec.get_sets x e) ignored_attrs in
  let atom = Exec.get_rel x "atom" in
  let atomic = MySet.union (Rel.dom atom) (Rel.rng atom) in
  let attrs = (if List.mem e atomic then ["X"] else []) @ attrs in
  let ev = Exec.get_set x "EV" in
  let r_ev = Exec.get_set x "R" in
  let src r_name e' = List.mem (e',e) (Exec.get_rel x r_name) in
  let reg_of e = List.assoc e reg_map in
  let reg_rval_of e = reg_of e, List.assoc e maps.Exec.rval_map in
  let a_regs = List.map reg_of (List.filter (src "ad") ev) in
  let c_regvals = List.map reg_rval_of (List.filter (src "cd") r_ev) in
  let d_regs = List.map reg_of (List.filter (src "dd") ev) in

  let ins =
    match List.mem e (Exec.get_set x "R"),
	  List.mem e (Exec.get_set x "W"),
	  List.mem e (Exec.get_set x "F")
    with
    | true, true, false ->
       (* If we're reading and writing at the same time,
          we have a (one-event) CAS. *)
       let loc = List.assoc e maps.Exec.loc_map in
       let loc_expr = Litmus.mk_expr loc a_regs in
       let rval = List.assoc e maps.Exec.rval_map in
       let wval = List.assoc e maps.Exec.wval_map in
       let wval_expr = Litmus.mk_expr wval d_regs in
       Litmus.Cas (loc_expr, rval, wval_expr)
    | true, false, false ->
       let reg = reg_of e in
       let loc = List.assoc e maps.Exec.loc_map in
       let loc_expr = Litmus.mk_expr loc a_regs in
       (* If this is the load-link instruction in a
          load-link/store-conditional pair, we attach the write value of
          the corresponding store-conditional event.  This lets us
          condense the pair into a single CAS when emitting, for example,
          C11 witnesses. *)
       if List.mem "X" attrs
       then
         let wev = List.assoc e atom in
         let rval = List.assoc e maps.Exec.rval_map in
         let wval = List.assoc wev maps.Exec.wval_map in
         let wval_expr = Litmus.mk_expr wval d_regs in
         Litmus.LoadLink (reg, loc_expr, rval, wval_expr)
       else Litmus.Load (reg, loc_expr)
    | false, true, false ->
       let loc = List.assoc e maps.Exec.loc_map in
       let wval = List.assoc e maps.Exec.wval_map in
       let loc_expr = Litmus.mk_expr loc a_regs in
       let wval_expr = Litmus.mk_expr wval d_regs in
       if List.mem "X" attrs
       then Litmus.StoreCnd (loc_expr, wval_expr)
       else Litmus.Store (loc_expr, wval_expr)
    | false, false, true ->
       Fence
    | _ -> assert false
  in
  let cs = [Litmus.Basic (ins, attrs)] in
  let mk_fence f cs =
    if List.mem e (Rel.rng (Exec.get_rel x f))
                (* FIXME: This currently inserts too many fences *)
    then Litmus.Basic (Litmus.Fence, [f]) :: cs else cs 
  in
  let cs = List.fold_right mk_fence Archs.all_fences cs in
  List.fold_left (fun c (r,v) -> [Litmus.If(r,v,c)]) cs c_regvals

(** [partition_seq sb es] orders the events in [es] into a sequence of events ordered by [sb]. *)
let partition_seq sb es : Evt.t list =
  List.sort (Rel.compare sb) es

let litmus_of_execution' x maps =
  let x = Exec.tidy_exec x in
  let locs = Assoc.key_list (Assoc.invert_map maps.Exec.loc_map) in
  let inv_thd_map = Assoc.invert_map maps.Exec.thd_map in
  let inv_thd_map =
    List.sort (fun (k,_) (k',_) -> compare k k') inv_thd_map
  in
  let thd_classes = Assoc.val_list inv_thd_map in
  let sb = Exec.get_rel x "sb" in
  let thds = List.map (partition_seq sb) thd_classes in
  (* let reg_evts = MySet.diff (Exec.get_set x "R") (Exec.get_set x "W") in *)
  let reg_evts = Exec.get_set x "R" in
  let mk_reg_map (i,res) e =
    let thd = Assoc.strong_assoc maps.Exec.thd_map e in
    (i+1, (e,(thd,i))::res)
  in
  let reg_map_thd reg_map thd =
    let evts = MySet.inter reg_evts thd in
    snd (List.fold_left mk_reg_map (0,reg_map) evts)
  in
  let reg_map = List.fold_left reg_map_thd [] thd_classes in
  let mk_thd t = List.concat (List.map (mk_instr x maps reg_map) t) in
  let thds = List.map mk_thd thds in
  let find_reg_val e =
    try
      let e', _ = List.find (fun (_,e') -> e'=e) (Exec.get_rel x "rf") in
      Assoc.strong_assoc maps.wval_map e'
    with Not_found -> 0
  in
  let find_reg e = try
      Litmus.Reg (List.assoc e reg_map)
    with Not_found ->
      failwith "Couldn't find register written by event %a." Evt.pp e
  in
  let reg_post =
    List.map (fun e -> (find_reg e, find_reg_val e)) reg_evts
  in
  let final_wval (l,es) =
    let ws = MySet.inter (Exec.get_set x "W") es in
    let co_after e e' = List.mem (e,e') (Exec.get_rel x "co") in
    let co_maximal e = not (List.exists (co_after e) ws) in
    let wval =
      try Assoc.strong_assoc maps.Exec.wval_map (List.find co_maximal ws)
      with Not_found -> 0
    in (Litmus.Loc l, wval)
  in
  let loc_post = List.map final_wval (Assoc.invert_map maps.loc_map) in
  {Litmus.locs = locs; thds = thds; post = reg_post @ loc_post}
    
let litmus_of_execution x =
  let maps = Exec.resolve_exec x in
  litmus_of_execution' x maps

let litmus_of_execution_pair x y pi =
  let xmaps = Exec.resolve_exec x in
  let ymaps = Exec.resolve_exec y in
  let ymaps = Exec.rectify_maps (x,xmaps) (y,ymaps) pi in
  let lit1 = litmus_of_execution' x xmaps in
  let lit2 = litmus_of_execution' y ymaps in
  lit1,lit2

