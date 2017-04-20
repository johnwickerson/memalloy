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

(** Converting a generic litmus test into a hardware litmus test *)

open Format
open General_purpose
open Litmus

(** [remove_Ifs r c] removes from the component [c] all if-statements that test the value of the register [r] *)
let rec remove_Ifs r = function
  | Basic b -> Basic b
  | Seq cs -> Seq (List.map (remove_Ifs r) cs)
  | Unseq cs -> Unseq (List.map (remove_Ifs r) cs)
  | If (r',v,c) ->
     let c' = remove_Ifs r c in
     if r=r' then c' else If(r',v,c')

(** [combine_Ifs c] seeks to combine consecutive if-statements with the same test. For instance, [combine_Ifs [If(r,v,c1); If(r,v,c2)] = [If(r, v, Seq [c1;c2])]]. This function assumes that "cd;sb \subseteq cd" holds, and that sb is total within a thread. *)
let rec combine_Ifs = function
  | [] -> []
  | Basic b :: cs -> Basic b :: combine_Ifs cs
  | If(r,v,c) :: cs ->
     [If(r, v, Seq (combine_Ifs (c :: List.map (remove_Ifs r) cs)))]
  | Seq cs :: cs' -> combine_Ifs (cs @ cs')
  | Unseq _ :: _ -> failwith "Program-order cannot be partial!"

(** Builds various flavours of load/store instructions *)
let mk_Access dir attrs (dst, src, off, sta) = 
  let a = {
      Litmus_HW.dir = dir;
      dst = dst; src = src; off = off; sta = sta;
      is_exclusive = List.mem "X" attrs;
      is_acq_rel = List.mem "scacq" attrs || List.mem "screl" attrs
    }
  in Litmus_HW.Access a

(** Builds a load instruction *)
let mk_LD attrs (dst, src, off) =
  mk_Access Litmus_HW.LD attrs (dst, src, off, None)

(** Builds a store instruction *)
let mk_ST attrs (src, dst, off, sta) =
  mk_Access Litmus_HW.ST attrs (dst, src, off, sta)

(** Builds a MOV or an ADD instruction, depending on whether or not there is a data-dependency *)
let mk_MOV_or_ADD (r_src, v) = function
  | None -> Litmus_HW.MOV (r_src, v)
  | Some r_off_d -> Litmus_HW.ADD (r_src, r_off_d, v)
	    
(** Builds fake dependencies using exclusive-or instructions. Currently an instruction can have an address or data dependency only on a single instruction, but there's no good reason not to generalise to any number of instructions if required. *)
let arm8_of_exp tid nr = function
  | Just n -> nr, [], n, None
  | Madd (Just n, r_dep) ->
     let r_off = tid,nr in
     let nr = nr + 1 in
     let il = [Litmus_HW.EOR (r_off, r_dep, r_dep)] in
     nr, il, n, Some r_off
  | _ -> failwith "Not yet implemented!"

(** [arm8_of_ins tid (locs, nr) ins] builds a sequence of ARM8 instructions from a single generic instruction [ins]. The current thread identifier is [tid], the correspondence between locations and registers is in [locs], and [nr] is the next register to use. *)
let hw_ins_of_ins use_status_reg mk_fence tid (locs,nr) = function
  | Load (r_dst, le), attrs ->
     let nr, il, l, r_off = arm8_of_exp tid nr le in
     let r_src = tid, nr in
     let nr = nr + 1 in
     let locs = (l, r_src) :: locs in
     let il = il @ [mk_LD attrs (r_dst, r_src, r_off)] in
     locs, nr, il
  | Store (le, Just v), attrs
       when List.mem "X" attrs && use_status_reg ->
     let nr, il, l, r_off = arm8_of_exp tid nr le in
     let r_src = tid,nr in
     let nr = nr + 1 in
     let r_dst = tid,nr in
     let nr = nr + 1 in
     let r_status = tid,nr in
     let nr = nr + 1 in
     let locs = (l, r_dst) :: locs in
     let il = il @ [
	   Litmus_HW.MOV (r_src, v);
	   mk_ST attrs (r_src, r_dst, r_off, Some r_status);
           Litmus_HW.CMP r_status; (* Branch to Fail if r_status!=0 *)
           Litmus_HW.BNZ (sprintf "Fail%d" tid)
	 ]
     in
     locs, nr, il
  | Store (le, Just v), attrs
       when List.mem "X" attrs && not use_status_reg ->
     let nr, il, l, r_off = arm8_of_exp tid nr le in
     let r_src = tid,nr in
     let nr = nr + 1 in
     let r_dst = tid,nr in
     let nr = nr + 1 in
     let locs = (l, r_dst) :: locs in
     let il = il @ [
	   Litmus_HW.MOV (r_src, v);
	   mk_ST attrs (r_src, r_dst, r_off, None);
           Litmus_HW.BNZ (sprintf "Fail%d" tid)
	 ]
     in
     locs, nr, il
  | Store (le, ve), attrs when not (List.mem "X" attrs) ->
     let nr, il1, l, r_off_a = arm8_of_exp tid nr le in
     let nr, il2, v, r_off_d = arm8_of_exp tid nr ve in
     let r_src = tid,nr in
     let nr = nr + 1 in
     let r_dst = tid,nr in
     let nr = nr + 1 in
     let locs = (l, r_dst) :: locs in
     let il = il1 @ il2 @ [
	   mk_MOV_or_ADD (r_src, v) r_off_d;
	   mk_ST attrs (r_src, r_dst, r_off_a, None)
	 ]
     in
     locs, nr, il
  | Cas _, _ -> failwith "No single-event RMWs in assembly!"
  | Fence, attrs ->
     let il = [Litmus_HW.HW_fence (mk_fence attrs)] 
     in locs, nr, il
  | _, _ -> failwith "Not yet implemented!"

(** A simpler version of the [Litmus.component] type that does not allow non-total sequencing within threads. *)
type 'a hw_component =
  | HW_Basic of 'a
  | HW_If of Register.t * Value.t * 'a hw_component list

(** Convert a generic litmus test component into an ARM8 litmus test component, by removing any unsequenced instructions. In fact, we expect there are already no unsequenced instructions, and fail if there are. *)
let rec flatten = function
  | Basic (ins,attrs) -> [HW_Basic (ins,attrs)]
  | Seq cs -> flatten_list cs
  | Unseq [c] -> flatten c
  | Unseq _ -> failwith "Program-order cannot be partial!"
  | If (r,v,c) -> [HW_If (r, v, flatten c)]
and flatten_list = function
  | [] -> []
  | c :: cs -> flatten c @ flatten_list cs

(** [can_fail il] holds iff the instruction list [il] contains an unconditional branch. Unconditional branches are only inserted to handle possibly-failing code (i.e. store-exclusives), so this function tests for the presence of possibly-failing code. *)
let can_fail il =
  let is_branch_to_Fail = function
    | Litmus_HW.BNZ str when String.sub str 0 4 = "Fail" -> true
    | _ -> false
  in
  List.exists is_branch_to_Fail il

(** [hw_ins_of_components tid (locs,nr,nl,il) cs] convert a list [cs] of ARM8 litmus test components into a list of ARM8 instructions. The current thread identifier is [tid], the correspondence between locations and registers is in [locs], [nr] is the next register to use, [nl] is the next label to use, and [il] is the list of instructions produced so far. *)
let rec hw_ins_of_components
          use_status_reg mk_fence tid (locs,nr,nl,il) = function
  | [] when can_fail il ->
     let r_zero = tid,nr in
     let nr = nr + 1 in
     let r_ok = tid,nr in
     let nr = nr + 1 in
     let il = il @ [
	   Litmus_HW.J (sprintf "Exit%d" tid);
	   Litmus_HW.LBL (sprintf "Fail%d" tid);
	   Litmus_HW.MOV (r_zero, 0);
	   mk_ST [] (r_zero, r_ok, None, None);
	   Litmus_HW.LBL (sprintf "Exit%d" tid)
	 ]
     in
     let locs = (-1, r_ok) :: locs in
     locs,nr,nl,il
  | [] -> locs,nr,nl,il
  | HW_Basic (ins,attrs) :: cs ->
     let locs,nr,il1 =
       hw_ins_of_ins use_status_reg mk_fence tid (locs,nr) (ins,attrs)
     in
     hw_ins_of_components
       use_status_reg mk_fence tid (locs,nr,nl,il@il1) cs
  | [HW_If (r,_,cs)] ->
     let lbl = sprintf "LC%02d" nl in
     let nl = nl + 1 in
     let il = il @ [
           Litmus_HW.CMP r;
	   Litmus_HW.BNZ lbl;
	   Litmus_HW.LBL lbl
         ]
     in
     hw_ins_of_components use_status_reg mk_fence tid (locs,nr,nl,il) cs
  | _ -> assert false

(** Calculate the first unused register in a thread *)
let rec next_reg n = function
  | [] -> n
  | HW_Basic (Load((_,r),_),_) :: cs ->
     next_reg (max (r+1) n) cs
  | _ :: cs -> next_reg n cs

(** [hw_thds_of_thds tid (locs,nl) thds] generates a list of hardware threads from a list [thds] of generic litmus test threads. The current thread identifier is [tid], the correspondence between locations and registers is in [locs], and [nl] is the next label to use. *)
let rec hw_thds_of_thds use_status_reg mk_fence tid (locs,nl) = function
  | [] -> locs, nl, []
  | thd :: thds ->
     let nr = next_reg 0 thd in
     let locs,_,nl,il1 =
       hw_ins_of_components
         use_status_reg mk_fence tid (locs,nr,nl,[]) thd
     in
     let locs,nl,il2 =
       hw_thds_of_thds
         use_status_reg mk_fence (tid+1) (locs,nl) thds
     in
     locs, nl, il1 :: il2

(** [hw_lit_of_lit name use_status_reg mk_fence lt] converts the generic litmus test [lt] into a hardware litmus test, named [name], using the [mk_fence] function to build architecture-specific fences, and using explicit status registers for store-conditionals iff the [use_status_reg] flag is set *)
let hw_lit_of_lit name use_status_reg mk_fence lt =
  let thds = List.map flatten lt.thds in 
  let locs, _, thds =
    hw_thds_of_thds use_status_reg mk_fence 0 ([], 0) thds
  in
  let locs = Assoc.group_map locs in
  {Litmus_HW.name = name; locs = locs; thds = thds; post = lt.post}
