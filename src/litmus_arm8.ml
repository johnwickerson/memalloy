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

(** Representing and pretty-printing ARM8 litmus tests *)

open Format
open General_purpose

(** Instruction in an ARM8 litmus test *)
type arm8_instruction =
  | LDR of Register.t * Register.t * Register.t option (** load *)
  | STR of Register.t * Register.t * Register.t option (** store *)
  | LDAR of Register.t * Register.t (** load acquire *)
  | STLR of Register.t * Register.t (** store release *)
  | LDXR of Register.t * Register.t (** load exclusive *)
  | STXR of Register.t * Register.t (** store exclusive *)
  | ADD of Register.t * Register.t * int (** addition *)
  | EOR of Register.t * Register.t * Register.t (** exclusive or *)
  | MOV of Register.t * int (** constant *)
  | DMB (** data memory barrier *)
  | DMB_ST (** data memory barrier on stores *)
  | DMB_LD (** data memory barrier on loads *)
  | ISB (** instruction synchronisation barrier *)
  | BNZ of Register.t * int (** branch + label *)
  | LBL of int

let pp_Xreg oc (_,r) = fprintf oc "X%d" r
let pp_Wreg oc (_,r) = fprintf oc "W%d" r

let pp_Xreg_full oc (t,r) = fprintf oc "%d:X%d" t r

let pp_addr oc = function
  | Litmus.Reg tr -> pp_Xreg_full oc tr
  | Litmus.Loc l -> Location.pp oc l

let pp_ins oc = function
  | LDR (dst, src, None) ->
     fprintf oc "LDR %a, [%a]" pp_Wreg dst pp_Xreg src
  | LDR (dst, src, Some off) ->
     fprintf oc "LDR %a, [%a,%a,SXTW]" pp_Wreg dst
	     pp_Xreg src pp_Wreg off
  | STR (src, dst, None) ->
     fprintf oc "STR %a, [%a]" pp_Wreg src pp_Xreg dst
  | STR (src, dst, Some off) ->
     fprintf oc "STR %a, [%a,%a,SXTW]" pp_Wreg src
	     pp_Xreg dst pp_Wreg off
  | LDAR (dst, src) ->
     fprintf oc "LDAR %a, [%a]" pp_Wreg dst pp_Xreg src
  | STLR (src, dst) ->
     fprintf oc "STLR %a, [%a]" pp_Wreg src pp_Xreg dst
  | LDXR (dst, src) ->
     fprintf oc "LDXR %a, [%a]" pp_Wreg dst pp_Xreg src
  | STXR (src, dst) ->
     fprintf oc "STXR %a, [%a]" pp_Wreg src pp_Xreg dst
  | ADD (dst, src, v) ->
     fprintf oc "ADD %a, %a, #%d"
	     pp_Wreg dst pp_Wreg src v
  | EOR (dst, src1, src2) ->
     fprintf oc "EOR %a, %a, %a"
	     pp_Wreg dst pp_Wreg src1 pp_Wreg src2
  | MOV (dst, v) ->
     fprintf oc "MOV %a, #%d" pp_Wreg dst v
  | DMB -> fprintf oc "DMB SY"
  | DMB_ST -> fprintf oc "DMB ST, SY"
  | DMB_LD -> fprintf oc "DMB LD, SY"
  | ISB -> fprintf oc "ISB"
  | BNZ (src, lbl) -> fprintf oc "CBNZ %a, LC%2d" pp_Wreg src lbl
  | LBL lbl -> fprintf oc "LC%2d:" lbl
						    
type t = {
    name: string;
    locs: (Location.t, Register.t list) Assoc.t;
    thds: arm8_instruction list list;
    post: (Litmus.address, Value.t) Assoc.t;
  }

let pp oc lt =
  fprintf oc "AArch64 %s\n" lt.name;
  fprintf oc "{\n";
  let pp_loc (x,rl) =
    fprintf oc "uint64_t %a;\n" Location.pp x;
    let pp_patch r =
      fprintf oc "%a = %a;\n" pp_Xreg_full r Location.pp x
    in
    List.iter pp_patch rl
  in
  List.iter pp_loc lt.locs;
  fprintf oc "}\n";
  let thds = List.map (List.map (asprintf "%a" pp_ins)) lt.thds in
  let longest_thd = MyList.max (List.map List.length thds) in
  let add_head n thd = sprintf "P%d" n :: thd in
  let thds = MyList.mapi add_head thds in
  let rec nops n = if n<=0 then [] else "" :: nops (n-1) in
  let add_nops n thd = thd @ nops (n - List.length thd + 1) in
  let thds = List.map (add_nops longest_thd) thds in
  let longest_str l = MyList.max (List.map String.length l) in
  let rec spaces n = if n<=0 then "" else " " ^ spaces (n-1) in
  let add_spaces n s = s ^ spaces (n - String.length s) in
  let add_spaces_thd thd = List.map (add_spaces (longest_str thd)) thd in
  let thds = List.map add_spaces_thd thds in
  for i = 0 to longest_thd do
    let line = List.map (fun thd -> List.nth thd i) thds in
    MyList.pp_gen " | " pp_str oc line;
    fprintf oc " ;\n"
  done;
  fprintf oc "\n";
  fprintf oc "exists\n";
  fprintf oc "(";
  let pp_cnstrnt oc (a,v) = fprintf oc "%a=%d" pp_addr a v in
  MyList.pp_gen " /\\ " pp_cnstrnt oc lt.post;
  fprintf oc ")\n"
