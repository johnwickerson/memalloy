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

type exclusivity = Exclusive | Not_exclusive
type acqrel = AcqRel | Not_acqrel

type arm8_direction = LD | ST
			     
type arm8_access = {
    dir : arm8_direction;
    dst : Register.t;
    src : Register.t;
    off : Register.t option;
    is_exclusive : bool;
    is_acq_rel : bool;
  }
		 
       
(** Instruction in an ARM8 litmus test *)       
type arm8_instruction =
  | Access of arm8_access (** loads and stores *)
  | ADD of Register.t * Register.t * int (** addition *)
  | EOR of Register.t * Register.t * Register.t (** exclusive or *)
  | MOV of Register.t * int (** constant *)
  | DMB of arm8_direction option (** data memory barrier *)
  | ISB (** instruction synchronisation barrier *)
  | BNZ of Register.t * int (** branch *)
  | LBL of int (** label *)

let pp_Xreg oc (_,r) = fprintf oc "X%d" r
let pp_Wreg oc (_,r) = fprintf oc "W%d" r

let pp_Xreg_full oc (t,r) = fprintf oc "%d:X%d" t r

let pp_addr oc = function
  | Litmus.Reg tr -> pp_Xreg_full oc tr
  | Litmus.Loc l -> Location.pp oc l

let pp_ins oc = function
  | Access a ->
     (match a.dir, a.off with
      | LD, None ->
	 fprintf oc "LD%s%sR %a, [%a]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_Wreg a.dst pp_Xreg a.src
      | LD, Some off ->
	 fprintf oc "LD%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "A" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_Wreg a.dst pp_Xreg a.src pp_Wreg off
      | ST, None ->
	 fprintf oc "ST%s%sR %a, [%a]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_Wreg a.src pp_Xreg a.dst
      | ST, Some off ->
	 fprintf oc "ST%s%sR %a, [%a,%a,SXTW]"
		 (if a.is_acq_rel then "L" else "")
		 (if a.is_exclusive then "X" else "")
		 pp_Wreg a.src pp_Xreg a.dst pp_Wreg off)
  | ADD (dst, src, v) ->
     fprintf oc "ADD %a, %a, #%d"
	     pp_Wreg dst pp_Wreg src v
  | EOR (dst, src1, src2) ->
     fprintf oc "EOR %a, %a, %a"
	     pp_Wreg dst pp_Wreg src1 pp_Wreg src2
  | MOV (dst, v) ->
     fprintf oc "MOV %a, #%d" pp_Wreg dst v
  | DMB None -> fprintf oc "DMB SY"
  | DMB (Some LD) -> fprintf oc "DMB LD"
  | DMB (Some ST) -> fprintf oc "DMB ST"
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
