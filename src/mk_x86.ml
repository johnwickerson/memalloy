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

(** Printing a hardware litmus test as a x86 litmus test *)

open! Format
open! General_purpose
open Litmus_HW

type fence = MFENCE | LFENCE | SFENCE

let mk_fence attrs =
  match (List.mem "mfence" attrs || List.mem "MFENCE" attrs) with
  | true -> MFENCE
  | _ -> failwith "Invalid fence attributes!"

let mk_tstart reg lbl = [TSTART (reg, lbl)]

let mk_tabort reg imm = [TABORT (reg, imm)]

let mk_tabort_handler reg _tstart_reg = let eax = (fst reg, -1) in [MOVREG (reg, eax)]

let encode_sentinel imm8 =
  assert (0 <= imm8 && imm8 < 256);
  let abt_caused_by_xabort = 0x1 in
  (imm8 lsl 24) lor abt_caused_by_xabort
                                                                 
let x86_specific_params = {
    use_status_reg=false;
    mk_fence; mk_tstart; mk_tabort; mk_tabort_handler;
    encode_sentinel;
}

let reg_name_of_num = function
  | (-1) -> "EAX" (* Reserve EAX for XBEGIN *)
  | 0 -> "EBX"
  | 1 -> "ECX"
  | 2 -> "EDX"
  | 3 -> "EDI"
  | 4 -> "ESI"
  | 5 -> "EBP"
  | 6 -> "ESP"
  | 7 -> "R8D"
  | 8 -> "R9D"
  | 9 -> "R10D"
  | 10 -> "R11D"
  | 11 -> "R12D"
  | 12 -> "R13D"
  | 13 -> "R14D"
  | 14 -> "R15D"
  | _ -> failwith "Bad register number!"

(** Print a register *)

(** Since the generic litmus HW AST uses RISC-like instructions we consume more
 * registers than X86 needs (or has available). To get around this restriction,
 * for each thread [t], we map (at pretty-printing time) from "virtual" generic
 * registers to "physical" x86 registers *)
let tid_to_next_reg = ref []
let tid_to_reg_map = ref []

let physical_reg_of t r =
  if not (List.mem_assoc t !tid_to_reg_map) then (
    tid_to_reg_map := (t, ref [(-1,-1)]) :: !tid_to_reg_map;
    tid_to_next_reg := (t, ref 0) :: !tid_to_next_reg;
  );
  let reg_map = List.assoc t !tid_to_reg_map in
  let next_reg = List.assoc t !tid_to_next_reg in
  if not (List.mem_assoc r !reg_map) then (
    reg_map := (r, !next_reg) :: !reg_map;
    next_reg := !next_reg + 1;
  );
  List.assoc r !reg_map

let pp_reg oc (t,r) =
  fprintf oc "%s" (reg_name_of_num (physical_reg_of t r))

(** Print a register qualified with thread identifier *)
let pp_reg_full oc (t,r) = fprintf oc "%d:%s" t (reg_name_of_num (physical_reg_of t r))

(** Print a location *)
let pp_loc oc = Location.pp oc

(** Print an instruction *)
let pp_ins oc = function
  | Access a ->
     if a.is_exclusive then
       (match a.dir, a.imm, a.loc with
        | LD, Some v, Some _ ->
          fprintf oc "MOV %a, %d"
            pp_reg a.dst v
        | ST, Some _, Some l ->
          let t,r = a.src in
          let reg = (t,r-3) in (* NC: same register as matching LD excl *)
          fprintf oc "XCHG [%a], %a"
            pp_loc l pp_reg reg
        | _, _, _ -> failwith "Bad access [%s]!" (Litmus_HW.mem_access_to_str a))
     else
       (match a.dir, a.imm, a.loc with
        | LD, None, Some l ->
          fprintf oc "MOV %a, [%a]"
            pp_reg a.dst pp_loc l
        | ST, Some v, Some l ->
          fprintf oc "MOV [%a], %d"
            pp_loc l v
        | _, _, _ -> failwith "Bad access [%s]!" (Litmus_HW.mem_access_to_str a))
  | ADD (dst, src, v) ->
     fprintf oc "ADD %a, %a, %d"
       pp_reg dst pp_reg src v
  | ADDREG (dst, src1, src2) ->
     fprintf oc "ADD %a, %a, %a"
       pp_reg dst pp_reg src1 pp_reg src2
  | EOR (dst, src1, src2) ->
     fprintf oc "XOR %a, %a, %a"
       pp_reg dst pp_reg src1 pp_reg src2
  | SHIFT (kind, dst, src, v) ->
     (match kind with
     | Litmus_HW.LSL ->
       fprintf oc "SHL %a, %a, %d"
         pp_reg dst pp_reg src v
     | Litmus_HW.LSR ->
       fprintf oc "SHR %a, %a, %d"
         pp_reg dst pp_reg src v)
  | MOV (_, _) ->
     fprintf oc "" (* deliberately elided *)
  | MOVREG (dst, src) ->
     (match src with
     | (_,-1) -> fprintf oc "MOV %a, %a" pp_reg dst pp_reg src
     | _ -> failwith "MOVREG only expected from TABORT sequence")
  | HW_fence MFENCE -> fprintf oc "MFENCE"
  | HW_fence LFENCE -> fprintf oc "LFENCE"
  | HW_fence SFENCE -> fprintf oc "SFENCE"
  | CMPIMM (src, imm) -> fprintf oc "CMP %a, %d" pp_reg src imm
  | CMP src -> fprintf oc "CMP %a, 0" pp_reg src
  | BEQ lbl -> fprintf oc "JE %s" lbl
  | BNZ lbl -> fprintf oc "JNE %s" lbl
  | J lbl -> fprintf oc "JMP %s" lbl
  | LBL lbl -> fprintf oc "%s:" lbl
  | TSTART (_, lbl) -> fprintf oc "XBEGIN %s" lbl (* ignore reg parameter *)
  | TCOMMIT -> fprintf oc "XEND"
  | TABORT (_, imm)  -> fprintf oc "XABORT %d" imm

(** [map_excls] maps exclusive pairs (LD/ST) so they can be emitted by
 * [pp_ins] as MOV/XCHG *)
let map_excls thd =
  let peephole n = function
    | Access a when a.is_exclusive && a.dir = LD ->
      let is_matching_excl = function
        | Access b when b.is_exclusive && b.dir = ST && b.loc = a.loc -> true
        | _ -> false
      in
      let imm_of = function
        | Access b -> b.imm
        | _ -> failwith ""
      in
      let instr = List.find is_matching_excl (MyList.drop n thd) in
      let expected_val = imm_of instr in
      let a' = {a with imm = expected_val} in
      Access a'
    | x -> x
  in
  let thd' = List.mapi peephole thd in
  let is_st_excl = function
    | Litmus_HW.Access a -> a.Litmus_HW.dir = Litmus_HW.ST && a.Litmus_HW.is_exclusive
    | _ -> false
  in
  let is_bnz_after_st_excl n = function
    | BNZ _ -> is_st_excl (List.nth thd' (n-1))
    | _ -> false
  in
  MyList.filteri (fun n i -> not (is_bnz_after_st_excl n i)) thd'

(** [patch_litmus] post-processes a HW litmus test [lt]:
  * - removing unnecessary RISC MOV instructions and register assignments
  * - mapping exclusives *)
let patch_litmus lt =
  let locs' = if List.mem_assoc (-1) lt.locs then [(-1,[])] else [] in
  let remove_movs thd =
    let is_not_mov = function
      | MOV (_,_) -> false
      | _ -> true
    in
    List.filter is_not_mov thd
  in
  let thds' = List.map remove_movs lt.thds in
  let thds'' = List.map map_excls thds' in
  {Litmus_HW.name = lt.name; locs = locs'; thds = thds''; post = lt.post}

let x86_of_lit name lt =
  let lt' = Mk_litmus_HW.hw_lit_of_lit name x86_specific_params lt in
  patch_litmus lt'

let pp oc lt = Litmus_HW.pp "X86" pp_reg_full pp_ins oc lt
