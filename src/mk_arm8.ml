(*
MIT License

Copyright (c) 2017 by John Wickerson and Nathan Chong.

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

(** Printing a hardware litmus test as an ARM8 litmus test *)

open! Format
open! General_purpose

type fence = DMB | DMBLD | DMBST | ISB

let mk_fence attrs =
  let has_attrs = List.map (fun x -> List.mem x attrs) in
  match has_attrs ["dmb"; "dmbst"; "dmbld"; "isb"],
        has_attrs ["DMBST"; "DMBLD"; "ISB"] with
  | [true; _; _; false], _         | _, [true; true; false]  -> DMB
  | [false; true; false; false], _ | _, [true; false; false] -> DMBST
  | [false; false; true; false], _ | _, [false; true; false] -> DMBLD
  | [false; false; false; true], _ | _, [false; false; true] -> ISB
  | _ -> failwith "Invalid fence attributes: %a!" (MyList.pp pp_str) attrs

let arm8_specific_params = {
    Litmus_HW.use_status_reg=true; mk_fence
}

(** Print a register in 64-bit mode *)
let pp_64reg oc (_,r) = fprintf oc "X%d" r

(** Print a register in 32-bit mode *)
let pp_32reg oc (_,r) = fprintf oc "W%d" r

(** Print a register qualified with thread identifier *)
let pp_reg_full oc (t,r) = fprintf oc "%d:X%d" t r

(** Print an instruction *)
let pp_ins oc = function
  | Litmus_HW.Access a ->
     (match a.dir, a.off, a.sta with
      | Litmus_HW.LD, None, None ->
	 fprintf oc "LD%s%sR %a, [%a]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.dst pp_64reg a.src
      | Litmus_HW.LD, Some off, None ->
	 if a.is_acq_rel then failwith "LDAR with offset not supported";
	 if a.is_exclusive then failwith "LDXR with offset not supported";
	 fprintf oc "LD%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.dst pp_64reg a.src pp_32reg off
      | Litmus_HW.ST, None, None ->
	 fprintf oc "ST%s%sR %a, [%a]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.src pp_64reg a.dst
      | Litmus_HW.ST, None, Some sta ->
	 fprintf oc "ST%s%sR %a, %a, [%a]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg sta pp_32reg a.src pp_64reg a.dst
      | Litmus_HW.ST, Some off, None ->
	 if a.is_acq_rel then failwith "STLR with offset not supported";
	 if a.is_exclusive then failwith "STXR with offset not supported";
	 fprintf oc "ST%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.src pp_64reg a.dst pp_32reg off
      | Litmus_HW.ST, Some off, Some sta ->
	 if a.is_acq_rel then failwith "STLR with offset not supported";
	 if a.is_exclusive then failwith "STXR with offset not supported";
	 fprintf oc "ST%s%sR %a, %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg sta pp_32reg a.src pp_64reg a.dst pp_32reg off
      | _, _, _ -> assert false)
  | Litmus_HW.ADD (dst, src, v) ->
     fprintf oc "ADD %a, %a, #%d"
       pp_32reg dst pp_32reg src v
  | Litmus_HW.ADDREG (dst, src1, src2) ->
     fprintf oc "ADD %a, %a, %a"
	     pp_32reg dst pp_32reg src1 pp_32reg src2
  | Litmus_HW.EOR (dst, src1, src2) ->
     fprintf oc "EOR %a, %a, %a"
	     pp_32reg dst pp_32reg src1 pp_32reg src2
  | Litmus_HW.SHIFT (kind, dst, src, v) ->
     (match kind with
     | Litmus_HW.LSL ->
       fprintf oc "LSL %a, %a, #%d"
         pp_32reg dst pp_32reg src v
     | Litmus_HW.LSR ->
       fprintf oc "LSR %a, %a, #%d"
         pp_32reg dst pp_32reg src v)
  | Litmus_HW.MOV (dst, v) ->
     fprintf oc "MOV %a, #%d" pp_32reg dst v
  | Litmus_HW.MOVREG (dst, src) ->
     fprintf oc "MOV %a, %a" pp_32reg dst pp_32reg src
  | Litmus_HW.HW_fence DMB -> fprintf oc "DMB SY"
  | Litmus_HW.HW_fence DMBLD -> fprintf oc "DMB LD"
  | Litmus_HW.HW_fence DMBST -> fprintf oc "DMB ST"
  | Litmus_HW.HW_fence ISB -> fprintf oc "ISB"
  | Litmus_HW.CMPIMM (src, imm) -> fprintf oc "CMP %a, #%d" pp_32reg src imm
  | Litmus_HW.CMP src -> fprintf oc "CMP %a, #0" pp_32reg src
  | Litmus_HW.BEQ lbl -> fprintf oc "BEQ %s" lbl
  | Litmus_HW.BNZ lbl -> fprintf oc "BNE %s" lbl
  | Litmus_HW.J lbl -> fprintf oc "B %s" lbl
  | Litmus_HW.LBL lbl -> fprintf oc "%s:" lbl

let arm8_of_lit name lt =
  Mk_litmus_HW.hw_lit_of_lit name arm8_specific_params lt

let pp oc lt = Litmus_HW.pp "AArch64" pp_reg_full pp_ins oc lt
