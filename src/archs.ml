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

(** The architectures supported by the tool *)

open! Format
open! General_purpose
       
type architecture =
  | Basic
  | C
  | Basic_HW
  | X86
  | Power
  | Arm7
  | Arm8
  | PTX
  | OpenCL
  | OCaml

(** Defining a hierarchy of architectures *)
let parent_arch = function
  | Basic                    -> None
  | C                        -> Some Basic
  | Basic_HW                 -> Some Basic
  | X86 | Power | Arm7 | PTX -> Some Basic_HW
  | Arm8                     -> Some Arm7
  | OpenCL                   -> Some C
  | OCaml                    -> Some Basic

(** Convert architecture to Alloy module name *)      
let pp_arch fences_as_relations oc arch =
  let module_name = match arch with
    | Basic    -> "exec"
    | C        -> "exec_C"
    | Basic_HW -> "exec_H"
    | X86      -> "exec_x86"
    | Power    -> "exec_ppc"
    | Arm7     -> "exec_arm7"
    | Arm8     -> "exec_arm8"
    | PTX      -> "exec_ptx"
    | OpenCL   -> "exec_OpenCL"
    | OCaml    -> "exec_OCaml"
  in
  if fences_as_relations then
    fprintf oc "../archs/fences_as_relations/%s" module_name
  else
    fprintf oc "../archs/%s" module_name

(** Convert architecture to Alloy signature name *)
let pp_Arch oc = function
  | Basic    -> fprintf oc "Exec"
  | C        -> fprintf oc "Exec_C"
  | Basic_HW -> fprintf oc "Exec_H"
  | X86      -> fprintf oc "Exec_X86"
  | Power    -> fprintf oc "Exec_PPC"
  | Arm7     -> fprintf oc "Exec_Arm7"
  | Arm8     -> fprintf oc "Exec_Arm8"
  | PTX      -> fprintf oc "Exec_PTX"
  | OpenCL   -> fprintf oc "Exec_OpenCL"
  | OCaml    -> fprintf oc "Exec_OCaml"

(** Convert Alloy signature name to architecture *)
let parse_Arch = function
  | "Exec"        -> Basic
  | "Exec_C"      -> C
  | "Exec_H"      -> Basic_HW
  | "Exec_X86"    -> X86
  | "Exec_PPC"    -> Power
  | "Exec_Arm7"   -> Arm7
  | "Exec_Arm8"   -> Arm8
  | "Exec_PTX"    -> PTX
  | "Exec_OpenCL" -> OpenCL
  | "Exec_OCaml"  -> OCaml
  | x -> failwith "Unexpected architecture %s" x

(** Parse architecture name *)
let parse_arch = function
  | "BASIC"  -> Basic
  | "C"      -> C
  | "HW"     -> Basic_HW
  | "X86"    -> X86
  | "PPC"    -> Power
  | "ARM7"   -> Arm7
  | "ARM8"   -> Arm8
  | "PTX"    -> PTX
  | "OpenCL" -> OpenCL
  | "OCaml"  -> OCaml
  | x -> failwith "Unexpected architecture %s" x

(** All supported architectures *)
let all = ["BASIC"; "C"; "HW"; "X86"; "PPC"; "ARM7";
           "ARM8"; "PTX"; "OpenCL"; "OCaml"]

(** Pre-defined fence sets for given architecture *)
let fence_sets = function
  | X86 -> ["MFENCE"]
  | Power -> ["SYNC"; "LWSYNC"; "ISYNC"]
  | Arm7 | Arm8 -> ["DMB"; "DMBST"; "DMBLD"; "ISB"]
  | PTX -> ["MEMBAR_CTA"; "MEMBAR_GL"; "MEMBAR_SYS"]
  | _ -> []

(** Pre-defined fence relations for given architecture *)
let fence_rels = function
  | X86 -> ["mfence"]
  | Power -> ["sync"; "lwsync"; "isync"]
  | Arm7 | Arm8 -> ["dmb"; "dmbst"; "dmbld"; "isb"]
  | PTX -> ["membar_cta"; "membar_gl"; "membar_sys"]
  | _ -> []

(** Pre-defined event sets for given architecture *)
let arch_sets fences_as_relations arch =
  let rec arch_sets = function
    | Basic -> ["EV"; "W"; "R"; "F"; "NAL"; "IW"]
    | C -> arch_sets Basic @ ["A"; "ACQ"; "REL"; "SC"]
    | Basic_HW -> arch_sets Basic
    | X86 -> arch_sets Basic_HW
    | Power -> arch_sets Basic_HW
    | Arm7 -> arch_sets Basic_HW
    | Arm8 -> arch_sets Arm7 @ ["SCREL"; "SCACQ"]
    | PTX -> arch_sets Basic_HW
    | OpenCL -> arch_sets C @ ["L"; "G"; "FGA"; "REM"; "WG"; "DV"; "SY"]
    | OCaml -> arch_sets Basic @ ["A"]
  in
  let fences = if fences_as_relations then [] else fence_sets arch in
  fences @ arch_sets arch

(** Pre-defined event relations for given architecture *)
let rec arch_rels = function
  | Basic -> ["ad"; "cd"; "co"; "dd"; "ftxn"; "rf"; "sb"; "sloc"; "sthd"; "stxn"]
  | C -> arch_rels Basic
  | Basic_HW -> arch_rels Basic @ ["atom"]
  | X86 -> arch_rels Basic_HW @ ["mfence"]
  | Power -> arch_rels Basic_HW @ ["sync"; "lwsync"; "isync"]
  | Arm7 -> arch_rels Basic_HW @ ["dmb"; "dmbst"; "dmbld"; "isb"]
  | Arm8 -> arch_rels Arm7
  | PTX -> arch_rels Basic_HW @
             ["scta"; "sgl"; "membar_cta"; "membar_gl"; "membar_sys"]
  | OpenCL -> arch_rels C @ ["swg"; "sdv"; "sbar"]
  | OCaml -> arch_rels Basic

(** Sets that should be reduced *)
let arch_min_sets fences_as_relations arch =
  let rec arch_min_sets = function
    | Basic -> []
    | C -> arch_min_sets Basic @ ["A"; "ACQ"; "REL"; "SC"]
    | Basic_HW -> arch_min_sets Basic
    | X86 -> arch_min_sets Basic_HW
    | Power -> arch_min_sets Basic_HW
    | Arm7 -> arch_min_sets Basic_HW
    | Arm8 -> arch_min_sets Arm7 @ ["SCREL"; "SCACQ"]
    | PTX -> arch_min_sets Basic_HW
    | OpenCL -> arch_min_sets C @ ["WG"; "DV"; "SY"]
    | OCaml -> arch_min_sets Basic @ ["A"]
  in
  let fences = if fences_as_relations then [] else fence_sets arch in
  fences @ arch_min_sets arch

let is_hw = function
  | Basic | C | OpenCL | OCaml -> false
  | Basic_HW | X86 | Power | Arm7 | Arm8 | PTX -> true
  
(** Relations that should be reduced *)
let arch_min_rels fences_as_relations arch =
  let fences = if fences_as_relations then fence_rels arch else [] in
  let atom = if is_hw arch then ["atom"] else [] in
  fences @ ["ad"; "cd"; "dd"] @ atom

(** List of all fence relations *)
let all_fences =
  ["dmb"; "dmbst"; "dmbld"; "isb";
   "sync"; "lwsync"; "isync";
   "membar_cta"; "membar_gl"; "membar_sys";
   "mfence"]

(** List of all pairs of relations [(r1,r2)] where membership of [r1] implies membership of [r2] (and hence [r2] need not be drawn) *)
let all_implied_rels =
  ["dmb", "dmbst";
   "dmb", "dmbld";
   "sync", "lwsync";
   "membar_gl", "membar_cta";
   "membar_sys", "membar_gl";
   "membar_sys", "membar_cta"]

(** List of all pairs of sets [(s1,s2)] where membership of [s1] implies membership of [s2] (and hence [s2] need not be drawn) *)
let all_implied_sets =
  ["SC", "ACQ"; "SC", "REL"; "SC", "A";
   "ACQ", "A"; "REL", "A"]

(** List of all sets that should be reduced as much as possible *)
let min_sets = [
    "SC"; "ACQ"; "REL"; "A"; "SCREL"; "SCACQ"; "MFENCE"; "SYNC";
    "LWSYNC"; "ISYNC"; "DMB"; "DMBST"; "DMBLD"; "ISB";
    "MEMBAR_CTA"; "MEMBAR_GL"; "MEMBAR_SYS"
  ]

(** List of all relations that should be reduced as much as possible *)
let min_rels = [
    "ad"; "cd"; "dd"; "dmb"; "dmbst"; "dmbld"; "isb"; "sync";
    "lwsync"; "isync"; "membar_cta"; "membar_gl";
    "membar_sys"; "mfence"; "stxn"
  ]
