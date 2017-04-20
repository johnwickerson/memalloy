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

(** Printing a hardware litmus test as an ARM8 litmus test *)

open Format
open General_purpose
open Litmus_HW

type fence = DMB | DMBLD | DMBST | ISB

let mk_fence attrs =
  match List.mem "dmb" attrs,
        List.mem "dmbst" attrs,
        List.mem "dmbld" attrs,
        List.mem "isb" attrs with
  | true, false, false, false -> DMB
  | false, true, false, false -> DMBST
  | false, false, true, false -> DMBLD
  | false, false, false, true -> ISB
  | _ -> failwith "Invalid fence attributes!"
   
(** Print a register in 64-bit mode *)
let pp_64reg oc (_,r) = fprintf oc "X%d" r

(** Print a register in 32-bit mode *)
let pp_32reg oc (_,r) = fprintf oc "W%d" r

(** Print a register qualified with thread identifier *)
let pp_reg_full oc (t,r) = fprintf oc "%d:X%d" t r

(** Print an instruction *)
let pp_ins oc = function
  | Access a ->
     (match a.dir, a.off, a.sta with
      | LD, None, None ->
	 fprintf oc "LD%s%sR %a, [%a]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.dst pp_64reg a.src
      | LD, Some off, None ->
	 fprintf oc "LD%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.dst pp_64reg a.src pp_32reg off
      | ST, None, None ->
	 fprintf oc "ST%s%sR %a, [%a]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.src pp_64reg a.dst
      | ST, None, Some sta ->
	 fprintf oc "ST%s%sR %a, %a, [%a]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg sta pp_32reg a.src pp_64reg a.dst
      | ST, Some off, None ->
	 fprintf oc "ST%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg a.src pp_64reg a.dst pp_32reg off
      | ST, Some off, Some sta ->
	 fprintf oc "ST%s%sR %a, %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_32reg sta pp_32reg a.src pp_64reg a.dst pp_32reg off
      | _, _, _ -> assert false)
  | ADD (dst, src, v) ->
     fprintf oc "ADD %a, %a, #%d"
	     pp_32reg dst pp_32reg src v
  | EOR (dst, src1, src2) ->
     fprintf oc "EOR %a, %a, %a"
	     pp_32reg dst pp_32reg src1 pp_32reg src2
  | MOV (dst, v) ->
     fprintf oc "MOV %a, #%d" pp_32reg dst v
  | HW_fence DMB -> fprintf oc "DMB SY"
  | HW_fence DMBLD -> fprintf oc "DMB LB"
  | HW_fence DMBST -> fprintf oc "DMB ST"
  | HW_fence ISB -> fprintf oc "ISB"
  | CMP src -> fprintf oc "CMP %a, #0" pp_32reg src
  | BNZ lbl -> fprintf oc "BNE %s" lbl
  | J lbl -> fprintf oc "B %s" lbl
  | LBL lbl -> fprintf oc "%s:" lbl

let arm8_of_lit name lt =
  let use_status_reg = true in
  Mk_litmus_HW.hw_lit_of_lit name use_status_reg mk_fence lt
             
let pp oc lt = Litmus_HW.pp "AArch64" pp_reg_full pp_ins oc lt
