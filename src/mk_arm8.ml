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

(** Converting a generic litmus test into an ARM8 test *)

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

let mk_Access dir attrs (dst, src, off) = 
  let a = { Litmus_arm8.dir = dir;
	    dst = dst; src = src; off = off;
	    is_exclusive = List.mem "X" attrs;
	    is_acq_rel = List.mem "scacq" attrs }
  in Litmus_arm8.Access a

let mk_LD attrs (dst, src, off) =
  mk_Access Litmus_arm8.LD attrs (dst, src, off)

let mk_ST attrs (src, dst, off) =
  mk_Access Litmus_arm8.ST attrs (dst, src, off)
			    	 
let rec arm8_of_ins tid (locs,nr) = function
  | Load (r_dst, Just l), attrs ->
     let r_src = tid, nr in
     let nr = nr + 1 in
     let locs = (l, r_src) :: locs in
     let il = [
	 mk_LD attrs (r_dst, r_src, None)
       ] in
     locs, nr, il
  | Load (r_dst, Madd (Just l,r_dep)), attrs ->
     let r_src = tid,nr in
     let nr = nr + 1 in
     let r_off = tid,nr in
     let nr = nr + 1 in
     let locs = (l, r_src) :: locs in
     let il = [
	 Litmus_arm8.EOR (r_off, r_dep, r_dep);
	 mk_LD attrs (r_dst, r_src, Some r_off)
       ] in
     locs, nr, il
  | Store (Just l, Just v), attrs ->
     let r_src = tid,nr in
     let nr = nr + 1 in
     let r_dst = tid,nr in
     let nr = nr + 1 in
     let locs = (l, r_dst) :: locs in
     let il = [
	 Litmus_arm8.MOV (r_src, v);
	 mk_ST attrs (r_src, r_dst, None)
       ] in
     locs, nr, il		 
  | Cas _, _ -> failwith "No single-event RMWs in assembly!"
  | Fence, attrs ->
     let il = match List.mem "dmbst" attrs,
		    List.mem "dmbld" attrs,
		    List.mem "isb" attrs with
       | true, true, false -> [Litmus_arm8.DMB None]
       | true, false, false -> [Litmus_arm8.DMB (Some Litmus_arm8.ST)]
       | false, true, false -> [Litmus_arm8.DMB (Some Litmus_arm8.LD)]
       | false, false, true -> [Litmus_arm8.ISB]
       | _ -> failwith "Invalid fence attributes!"
     in locs, nr, il
  | _, _ -> failwith "Not implemented yet!"

type 'a arm8_component =
  | Arm8_Basic of 'a
  | Arm8_If of Register.t * Value.t * 'a arm8_component list

let rec flatten = function
  | Basic (ins,attrs) -> [Arm8_Basic (ins,attrs)]
  | Seq cs -> flatten_list cs
  | Unseq [c] -> flatten c
  | Unseq _ -> failwith "Program-order cannot be partial!"
  | If (r,v,c) -> [Arm8_If (r, v, flatten c)]
and flatten_list = function
  | [] -> []
  | c :: cs -> flatten c @ flatten_list cs
			 					
let rec arm8_of_components tid (locs,nr,nl) = function
  | [] -> locs,nr,nl,[]
  | Arm8_Basic (ins,attrs) :: cs ->
     let locs,nr,il1 = arm8_of_ins tid (locs,nr) (ins,attrs) in
     let locs,nr,nl,il2 = arm8_of_components tid (locs,nr,nl) cs in
     locs, nr, nl, il1 @ il2
  | [Arm8_If (r,_,cs)] ->
     let lbl = nl in
     let nl = nl + 1 in
     let locs, nr, nl, il = arm8_of_components tid (locs,nr,nl) cs in
     let bnz_lbl = [Litmus_arm8.BNZ (r,lbl); Litmus_arm8.LBL lbl] in 
     locs, nr, nl, bnz_lbl @ il
  | _ -> assert false

let rec next_reg n = function
  | [] -> n
  | Arm8_Basic (Load((_,r),_),_) :: cs ->
     next_reg (max (r+1) n) cs
  | _ :: cs -> next_reg n cs
		
let rec arm8_of_thds tid (locs,nl) = function
  | [] -> locs, nl, []
  | thd :: thds ->
     let nr = next_reg 0 thd in
     let locs,_,nl,il1 = arm8_of_components tid (locs,nr,nl) thd in
     let locs,nl,il2 = arm8_of_thds (tid+1) (locs,nl) thds in
     locs, nl, il1 :: il2

let arm8_of_lit name lt =
  let thds = List.map flatten lt.thds in 
  let locs, _, thds = arm8_of_thds 0 ([], 0) thds in
  let locs = Assoc.group_map locs in
  {Litmus_arm8.name = name; locs = locs; thds = thds; post = lt.post}
