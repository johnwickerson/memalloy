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

open Format

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

let pp_arch oc = function
  | Basic -> fprintf oc "../archs/exec"
  | C -> fprintf oc "../archs/exec_C"
  | Basic_HW -> fprintf oc "../archs/exec_H"
  | X86 -> fprintf oc "../archs/exec_x86"
  | Power -> fprintf oc "../archs/exec_ppc"
  | Arm7 -> fprintf oc "../archs/exec_arm7"
  | Arm8 -> fprintf oc "../archs/exec_arm8"
  | PTX -> fprintf oc "../archs/exec_ptx"
  | OpenCL -> fprintf oc "../archs/exec_OpenCL"

let pp_Arch oc = function
  | Basic -> fprintf oc "Exec"
  | C -> fprintf oc "Exec_C"
  | Basic_HW -> fprintf oc "Exec_H"
  | X86 -> fprintf oc "Exec_X86"
  | Power -> fprintf oc "Exec_PPC"
  | Arm7 -> fprintf oc "Exec_Arm7"
  | Arm8 -> fprintf oc "Exec_Arm8"
  | PTX -> fprintf oc "Exec_PTX"
  | OpenCL -> fprintf oc "Exec_OpenCL"

let parse_arch = function
  | "BASIC" -> Basic
  | "C" -> C
  | "HW" -> Basic_HW
  | "X86 TSO" -> X86
  | "PPC" -> Power
  | "ARM7" -> Arm7
  | "ARM8" -> Arm8
  | "PTX" -> PTX
  | "OpenCL" -> OpenCL
  | x -> failwith (asprintf "Unexpected architecture: %s." x)

let rec arch_sets = function
  | Basic ->
     ["ev"; "W"; "R"; "F"; "naL"; "M"]
  | C ->
     arch_sets Basic @ ["A"; "acq"; "rel"; "sc"]
  | Basic_HW ->
     arch_sets Basic @ ["A"; "X"]
  | X86 ->
     arch_sets Basic_HW @ ["MFENCE"]
  | Power ->
     arch_sets Basic_HW @
       ["sync"; "lwsync"; "eieio"; "isync";
	"SYNC"; "LWSYNC"; "EIEIO"; "ISYNC"]
  | Arm7 ->
     arch_sets Basic_HW @
       ["dmb"; "DMB"; "DSB"; "DMBSY"; "dmbst"; "DMBST";
	"dmbld"; "DMBLD"; "isb"; "ISB"; "DSBST"]
  | Arm8 ->
     arch_sets Arm7 @ ["screl"; "scacq"]
  | PTX ->
     arch_sets Basic_HW @
       ["membarcta"; "membargl"; "membarsys"]
  | OpenCL ->
     arch_sets C @
       ["L"; "G"; "fga"; "rem"; "entry_fence";
	"exit_fence"; "wg"; "dv"; "sy"]

let rec arch_rels = function
  | Basic ->
     ["ad"; "addr"; "cd"; "co"; "coe"; "coi"; "ctrl"; "data";
      "dd"; "ext"; "fr"; "fre"; "fri"; "loc"; "po"; "poloc";
      "rf"; "rfe"; "rfi"; "sb"; "sloc"; "sthd"; "thd"]
  | C -> arch_rels Basic
  | Basic_HW -> arch_rels Basic @ ["atom"; "rmw"]
  | X86
  | Power
  | Arm7
  | Arm8 -> arch_rels Basic_HW
  | PTX -> arch_rels Basic_HW @ ["scta"; "sgl"]
  | OpenCL -> arch_rels C @ ["swg"; "sdv"; "sbar"]
