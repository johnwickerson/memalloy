(*
MIT License

Copyright (c) 2017 by John Wickerson and Nathan Chong

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

(** Printing a hardware litmus test as a PowerPC litmus test *)

open! Format
open! General_purpose

type fence = SYNC | LWSYNC | ISYNC

let mk_fence attrs =
  match (List.mem "sync" attrs || List.mem "SYNC" attrs),
        (List.mem "lwsync" attrs || List.mem "LWSYNC" attrs),
        (List.mem "isync" attrs || List.mem "ISYNC" attrs) with
  | true , _, false -> SYNC (* sync is also lwsync *)
  | false, true, false -> LWSYNC
  | false, false, true -> ISYNC
  | _ -> failwith "Invalid fence attributes!"

let mk_tstart reg lbl =
  [ Litmus_HW.TSTART (reg, lbl);
    Litmus_HW.BEQ lbl]

let mk_tabort reg imm =
  [ Litmus_HW.MOV (reg, imm);
    Litmus_HW.TABORT (reg, imm)]

let mk_tabort_handler reg _tstart_reg =
  let texasr = (fst reg, -1) in
  (* we shift to get the bottom word of the 64-bit texasr *)
  [ Litmus_HW.MOVREG (reg, texasr);
    Litmus_HW.SHIFT (Litmus_HW.LSR, reg, reg, 32) ]

let encode_sentinel imm8 =
  assert (0 <= imm8 && imm8 < 256);
  let abt_caused_by_tabort = 0x1 in
  (imm8 lsl 24) lor abt_caused_by_tabort
                                                                    
let ppc_specific_params = {
    Litmus_HW.use_status_reg=false;
    mk_fence; mk_tstart; mk_tabort; mk_tabort_handler;
    encode_sentinel;
}
   
(** Print a register *)
let pp_reg oc (_,r) = fprintf oc "r%d" r

(** Print a register qualified with thread identifier *)
let pp_reg_full oc (t,r) = fprintf oc "%d:r%d" t r

(** Print an instruction *)
let pp_ins oc = function
  | Litmus_HW.Access a ->
     (match a.dir, a.off, a.sta with
      | LD, None, None when not a.is_exclusive ->
	 fprintf oc "lwz %a, 0(%a)"
	   pp_reg a.dst pp_reg a.src
      | LD, Some off, None when not a.is_exclusive ->
	 fprintf oc "lwzx %a, %a, %a"
	   pp_reg a.dst pp_reg off pp_reg a.src
      | LD, None, None when a.is_exclusive ->
         fprintf oc "lwarx %a, 0, %a"
           pp_reg a.dst pp_reg a.src
      | LD, Some off, None when a.is_exclusive ->
         fprintf oc "lwarx %a, %a, %a"
           pp_reg a.dst pp_reg off pp_reg a.src
      | ST, None, None when not a.is_exclusive ->
	 fprintf oc "stw %a, 0(%a)"
	   pp_reg a.src pp_reg a.dst
      | ST, None, None when a.is_exclusive ->
         fprintf oc "stwcx. %a, 0, %a"
           pp_reg a.src pp_reg a.dst
      | ST, Some off, None ->
         fprintf oc "stw%sx%s %a, %a, %a"
           (if a.is_exclusive then "c" else "")
           (if a.is_exclusive then "." else "")
           pp_reg a.src pp_reg off pp_reg a.dst
      | _, _, _ -> assert false)
  | Litmus_HW.ADD (dst, src, v) ->
     fprintf oc "addi %a, %a, %d"
       pp_reg dst pp_reg src v
  | Litmus_HW.ADDREG (dst, src1, src2) ->
     fprintf oc "add %a, %a, %a"
       pp_reg dst pp_reg src1 pp_reg src2
  | Litmus_HW.EOR (dst, src1, src2) ->
     fprintf oc "xor %a, %a, %a"
       pp_reg dst pp_reg src1 pp_reg src2
  | Litmus_HW.SHIFT (kind, dst, src, v) ->
     (match kind with
     | Litmus_HW.LSL ->
       fprintf oc "sldi %a, %a, %d"
         pp_reg dst pp_reg src v
     | Litmus_HW.LSR ->
       fprintf oc "srdi %a, %a, %d"
         pp_reg dst pp_reg src v)
  | Litmus_HW.MOV (dst, v) ->
     fprintf oc "li %a, %d" pp_reg dst v
  | Litmus_HW.MOVREG (dst, src) ->
     (match src with
     | (_,-1) ->
       fprintf oc "mftexasr %a" pp_reg dst
     | _ -> failwith "MOVREG only expected from TABORT sequence")
  | Litmus_HW.HW_fence SYNC -> fprintf oc "sync"
  | Litmus_HW.HW_fence LWSYNC -> fprintf oc "lwsync"
  | Litmus_HW.HW_fence ISYNC -> fprintf oc "isync"         
  | Litmus_HW.CMPIMM (src, imm) -> fprintf oc "cmpwi %a, %d" pp_reg src imm
  | Litmus_HW.CMP src -> fprintf oc "cmpwi %a, 0" pp_reg src
  | Litmus_HW.BEQ lbl -> fprintf oc "beq %s" lbl
  | Litmus_HW.BNZ lbl -> fprintf oc "bne %s" lbl
  | Litmus_HW.J lbl -> fprintf oc "b %s" lbl
  | Litmus_HW.LBL lbl -> fprintf oc "%s:" lbl
  | Litmus_HW.TSTART (_, _) -> fprintf oc "tbegin." (* ignore reg parameter *)
  | Litmus_HW.TCOMMIT -> fprintf oc "tend."
  | Litmus_HW.TABORT (src, _) -> fprintf oc "tabort. %a" pp_reg src

let ppc_of_lit name lt =
  Mk_litmus_HW.hw_lit_of_lit name ppc_specific_params lt
             
let pp oc lt = Litmus_HW.pp "PPC" pp_reg_full pp_ins oc lt
