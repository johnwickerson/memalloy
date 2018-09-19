(*
MIT License

Copyright (c) 2018 by John Wickerson and Matt Windsor.

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

(** Printing a litmus test as a OpenCL program *)

open! Format
open! General_purpose

(** Bijection between Nat*Nat and Nat. *)
let cantor_pair (a,b) = (a + b) * (a + b + 1) / 2 + b

let array_global_atomic = "ga"
let array_global_nonatomic = "gn"
let array_local_atomic = "la"
let array_local_nonatomic = "ln"
let array_output = "out"
  
let pp_reg oc (tid,reg) =
  let idx = cantor_pair (tid,reg) in
  fprintf oc "%s[%d]" array_output idx
    
let rec pp_expr k oc = function
  | Litmus.Just x -> k oc x
  | Litmus.Madd (e,r) -> fprintf oc "%a + 0*%a" (pp_expr k) e pp_reg r 
  
let get_ms attrs =
    match List.mem "DV" attrs, List.mem "SY" attrs with
    | false, _ -> "memory_scope_work_group"
    | true, false -> "memory_scope_device"
    | true, true -> "memory_scope_all_svm_devices"
                  
(** [pp_cas_reg] takes an optional register from a CAS instruction.
   If it exists, [pp_cas_reg] returns the result of [pp_reg]; else, it
   emits a reference to 'expected'. *)
let pp_cas_reg oc = function
  | Some rr -> pp_reg oc rr
  | None    -> fprintf oc "expected"

let pp_cas pp_loc oc mo ms obj exp_reg exp des =
  fprintf oc "%a = %a; " pp_cas_reg exp_reg Value.pp exp;
  (* TODO: this needs some more work to get right in LitmusC:
     we shouldn't emit &expr_reg for litmus, but this'd need us to emit
     the register as a pointer somehow. *)
  fprintf oc "/* returns bool */ atomic_compare_exchange_strong_explicit(%a, &%a, %a, %s, memory_order_relaxed, %s)"
    (pp_expr pp_loc) obj
    pp_cas_reg exp_reg
    (pp_expr Value.pp) des
    mo
    ms
  
let pp_instr pp_loc oc = function
  | Litmus.Load (r,le), attrs ->
     if List.mem "A" attrs then
       let mo = Litmus_C.get_mo attrs in
       let ms = get_ms attrs in
       fprintf oc "%a = atomic_load_explicit(&%a, %s, %s)"
         pp_reg r (pp_expr pp_loc) le mo ms
     else
       fprintf oc "%a = %a"
         pp_reg r (pp_expr pp_loc) le
    
  | Litmus.LoadLink (r, obj, exp, des), attrs ->
     (* We model LL/SC as a CAS in C11 witnesses. *)
     let mo = Litmus_C.get_mo attrs in
     let ms = get_ms attrs in
     pp_cas pp_loc oc mo ms obj (Some r) exp des
    
  | Litmus.Store (le,ve), attrs ->
     if List.mem "A" attrs then
       let mo = Litmus_C.get_mo attrs in
       let ms = get_ms attrs in
       fprintf oc "atomic_store_explicit(&%a, %a, %s, %s)"
         (pp_expr pp_loc) le (pp_expr Value.pp) ve mo ms
     else
       fprintf oc "%a = %a"
         (pp_expr pp_loc) le (pp_expr Value.pp) ve

  | Litmus.StoreCnd _, _ ->
     (* We've already emitted a CAS for the LoadLink, so we don't emit
        a separate StoreCnd instruction.  (We still print _something_,
        to avoid throwing off the indentation. *)
     fprintf oc "// elided store-conditional instruction"

  | Litmus.Cas (obj,exp,des), attrs ->
     let mo = Litmus_C.get_mo attrs in
     let ms = get_ms attrs in
     pp_cas pp_loc oc mo ms obj None exp des
    
  | Litmus.Fence, attrs ->
     let mo = Litmus_C.get_mo attrs in
     let ms = get_ms attrs in
     let flags = match List.mem "G" attrs, List.mem "L" attrs with
       | true, true -> "CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE"
       | false, true -> "CLK_LOCAL_MEM_FENCE"
       | true, false -> "CLK_GLOBAL_MEM_FENCE"
       | false, false -> "0"
     in
     fprintf oc "atomic_work_item_fence(%s, %s, %s)" flags mo ms

let no_braces_needed = function
  | Litmus.Basic (Litmus.Cas _, _) -> false
  | _ -> true
                        
(** Pretty-printing of components *)     
let rec pp_component i pp_loc oc = function
  | Litmus.Basic b ->
     fprintf oc "%a%a;\n" mk_indent i (pp_instr pp_loc) b
  | Litmus.If (r,v,[c]) when no_braces_needed c ->
     fprintf oc "%aif (%a == %a)\n" mk_indent i pp_reg r Value.pp v;
     pp_component (i+1) pp_loc oc c
  | Litmus.If (r,v,cs) ->
     fprintf oc "%aif (%a == %a) {\n" mk_indent i pp_reg r Value.pp v;
     List.iter (pp_component (i+1) pp_loc oc) cs;
     fprintf oc "%a}\n" mk_indent i

let partition_locs_in_instr s (a_locs, na_locs) = function
  | Litmus.Load (_,le), attrs
  | Litmus.LoadLink (_,le,_,_), attrs
  | Litmus.Store (le,_), attrs
  | Litmus.StoreCnd (le,_), attrs
  | Litmus.Cas (le,_,_), attrs ->
     let l = Litmus.expr_base_of le in
     begin match List.mem s attrs with
     | true ->
        assert (not (List.mem l a_locs));
        a_locs, MySet.union [l] na_locs
     | false ->
        assert (not (List.mem l na_locs));
        MySet.union [l] a_locs, na_locs
     end
  | _ -> (a_locs, na_locs)
     
let rec partition_locs_in_cmps s locs cs =
  List.fold_left (partition_locs_in_cmp s) locs cs

and partition_locs_in_cmp s locs = function
  | Litmus.Basic b -> partition_locs_in_instr s locs b
  | Litmus.If (_,_,cs) -> partition_locs_in_cmps s locs cs
     
let partition_locs s lt =
  List.fold_left (partition_locs_in_cmps s) ([],[]) lt.Litmus.thds

let rec contains_cas = function
  | Litmus.Basic (Litmus.Cas _, _) -> true
  | Litmus.Basic _ -> false
  | Litmus.If (_,_,cs) -> List.exists contains_cas cs

let rec extract_regs regs = function
  | Litmus.Basic (Litmus.Load (r,_), _) -> MySet.union [r] regs
  | Litmus.Basic _ -> regs
  | Litmus.If (r,_,cs) -> MySet.union [r] (List.fold_left extract_regs regs cs)

let rec first_attrs = function
  | [] -> raise Not_found
  | Litmus.Basic (_, attrs) :: _ -> attrs
  | Litmus.If (_,_,cs) :: _ -> first_attrs cs
  
let pp oc lt =

  let atomic_locs, nonatomic_locs = partition_locs "NAL" lt in
  assert (MySet.equal (atomic_locs @ nonatomic_locs) lt.Litmus.locs);
  
  let global_locs, local_locs = partition_locs "L" lt in
  assert (MySet.equal (global_locs @ local_locs) lt.Litmus.locs);

  let pp_loc oc l =
    let array = match List.mem l global_locs, List.mem l atomic_locs with
      | true, true -> array_global_atomic
      | true, false -> array_global_nonatomic
      | false, true -> array_local_atomic
      | false, false -> array_local_nonatomic
    in
    fprintf oc "%s[%d]" array l
  in

  (* Print kernel type. *)
  fprintf oc "__kernel void foo(\n";
  fprintf oc "  __global atomic_int %s[] /* global, atomic locations */,\n" array_global_atomic;
  fprintf oc "  __global int %s[] /* global, non-atomic locations */,\n" array_global_nonatomic;
  fprintf oc "  __local atomic_int %s[] /* local, atomic locations */,\n" array_local_atomic;
  fprintf oc "  __local int %s[] /* local, non-atomic locations */,\n" array_local_nonatomic;
  fprintf oc "  __global int %s[] /* output */\n" array_output;
  fprintf oc ") {\n";

  (* Get global and local thread identifiers. *)
  fprintf oc "  int lid = get_local_id(0);\n";
  fprintf oc "  int wgid = get_group_id(0);\n";

  let wgid_of tid =
    let rec get_wgid = function
      | [] -> failwith "Expected a wgX attribute here"
      | attr :: attrs ->
         try Scanf.sscanf attr "wg%d" (fun i -> i)
         with Scanf.Scan_failure _ -> get_wgid attrs
    in
    let cs = List.nth lt.Litmus.thds tid in
    get_wgid (first_attrs cs)
  in

  let tids = List.mapi (fun i _ -> i) lt.Litmus.thds in

  let locals = List.filter (fun l -> List.mem l local_locs) lt.Litmus.locs in

  if locals <> [] then (
  
    (* Initialise all local locations. *)
    let max_wgid = MyList.max (List.map wgid_of tids) in
    for wgid = 0 to max_wgid do
      if wgid > 0 then fprintf oc " else " else fprintf oc "  ";
      fprintf oc "if (lid == 0 && wgid == %d) {\n" wgid;
      fprintf oc "    // Initialise local locations\n";
      let initialise l = fprintf oc "    %a = 0;\n" pp_loc l in
      List.iter initialise locals;
      fprintf oc "  }"
    done;
    fprintf oc "\n";
    fprintf oc "\n";
    
    (* Barrier after initialising local locations *)
    fprintf oc "  // Barrier ensures local locations have been initialised.\n";
    fprintf oc "  barrier(CLK_LOCAL_MEM_FENCE);\n";
    fprintf oc "\n";
    
  );
  
  (* Print a function for each thread. *)
  let pp_thd gid cs =
    if gid > 0 then fprintf oc " else " else fprintf oc "  ";
    let wgid = wgid_of gid in 
    fprintf oc "if (lid == %d && wgid == %d) {\n" gid wgid;
    fprintf oc "    // Work-item %d in workgroup %d:\n" gid wgid;
    List.iter (pp_component 2 pp_loc oc) cs;
    fprintf oc "  }"
  in
  List.iteri pp_thd lt.Litmus.thds;
  fprintf oc "\n";
  fprintf oc "}\n";
  fprintf oc "\n";

  let pp_cnstrnt oc (a,v) = match a with
    | Litmus.Reg r -> fprintf oc "%a == %d" pp_reg r v
    | Litmus.Loc l -> fprintf oc "%a == %d" pp_loc l v
  in
  
  fprintf oc "//Postcondition: %a\n"
    (MyList.pp_gen " && " pp_cnstrnt) lt.Litmus.post;

