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

(** Printing a hardware litmus test as a PowerPC litmus test *)

open! Format
open! General_purpose
open Litmus_HW

type fence = SYNC | LWSYNC | EIEIO | ISYNC

let mk_fence attrs =
  match List.mem "sync" attrs,
        List.mem "lwsync" attrs,
        List.mem "eieio" attrs,
        List.mem "isync" attrs with
  | true, false, false, false -> SYNC
  | false, true, false, false -> LWSYNC
  | false, false, true, false -> EIEIO
  | false, false, false, true -> ISYNC
  | _ -> failwith "Invalid fence attributes!"
   
(** Print a register *)
let pp_reg oc (_,r) = fprintf oc "r%d" r

(** Print a register qualified with thread identifier *)
let pp_reg_full oc (t,r) = fprintf oc "%d:r%d" t r

(** Print an instruction *)
let pp_ins oc = function
  | Access a ->
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
         fprintf oc "stwcx %a, 0, %a"
           pp_reg a.src pp_reg a.dst
      | ST, Some off, None ->
         fprintf oc "stw%sx %a, %a, %a"
           (if a.is_exclusive then "c" else "")
           pp_reg a.src pp_reg off pp_reg a.dst
      | _, _, _ -> assert false)
  | ADD (dst, src, v) ->
     fprintf oc "addi %a, %a, %d"
       pp_reg dst pp_reg src v
  | EOR (dst, src1, src2) ->
     fprintf oc "xor %a, %a, %a"
       pp_reg dst pp_reg src1 pp_reg src2
  | MOV (dst, v) ->
     fprintf oc "li %a, %d" pp_reg dst v
  | HW_fence SYNC -> fprintf oc "sync"
  | HW_fence LWSYNC -> fprintf oc "lwsync"
  | HW_fence EIEIO -> fprintf oc "eieio"
  | HW_fence ISYNC -> fprintf oc "isync"         
  | CMP src -> fprintf oc "cmpwi %a, 0" pp_reg src
  | BNZ lbl -> fprintf oc "bne %s" lbl
  | J lbl -> fprintf oc "b %s" lbl
  | LBL lbl -> fprintf oc "%s:" lbl

let ppc_of_lit name lt =
  let use_status_reg = false in
  Mk_litmus_HW.hw_lit_of_lit name use_status_reg mk_fence lt
             
let pp oc lt = Litmus_HW.pp "PPC" pp_reg_full pp_ins oc lt
