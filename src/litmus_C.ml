(*
MIT License

Copyright (c) 2018 by John Wickerson.

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

(** Printing a litmus test as a C program *)

open! Format
open! General_purpose

let pp_reg oc (tid,reg) =
  fprintf oc "t%dr%d" tid reg

(** [pp_cas_reg] takes an optional register from a CAS instruction.
    If it exists, [pp_cas_reg] returns the result of [pp_reg]; else,
    it emits a reference to 'expected'. *)
let pp_cas_reg oc = function
  | Some rr -> pp_reg oc rr
  | None    -> fprintf oc "expected"


let rec pp_expr k oc = function
  | Litmus.Just x -> k oc x
  | Litmus.Madd (e,r) -> fprintf oc "%a + 0*%a" (pp_expr k) e pp_reg r

(** [pp_cas oc mo obj exp_reg exp des] prints a compare-and-exchange on
    location [obj], from [exp] to [des].  It loads [exp] into either register
    [exp_reg] (if [exp_reg = Some]) or the 'expected' temporary variable
    (if not). It uses memory order [mo] for success, and relaxed for fail. *)
let pp_cas oc mo obj exp_reg exp des =
  fprintf oc "%a = %a; " pp_cas_reg exp_reg Value.pp exp;
  fprintf oc "/* returns bool */ atomic_compare_exchange_strong_explicit(&%a, &%a, %a, %s, memory_order_relaxed)"
    (pp_expr MyLocation.pp) obj
    pp_cas_reg exp_reg
    (pp_expr Value.pp) des
    mo

let get_mo attrs =
    match List.mem "SC" attrs, List.mem "ACQ" attrs, List.mem "REL" attrs with
    | true, _, _ -> "memory_order_seq_cst"
    | false, true, true -> "memory_order_acq_rel"
    | false, true, false -> "memory_order_acquire"
    | false, false, true -> "memory_order_release"
    | false, false, false -> "memory_order_relaxed"

let pp_instr oc = function
  | Litmus.Load (r,le), attrs ->
     if List.mem "A" attrs then
       let mo = get_mo attrs in
       fprintf oc "%a = atomic_load_explicit(&%a, %s)"
         pp_reg r (pp_expr MyLocation.pp) le mo
     else
       fprintf oc "%a = %a"
         pp_reg r (pp_expr MyLocation.pp) le
  | Litmus.LoadLink (r, obj, exp, des), attrs ->
     (* We model LL/SC as a CAS in C11 witnesses. *)
     let mo = get_mo attrs in
     pp_cas oc mo obj (Some r) exp des
  | Litmus.Store (le,ve), attrs ->
     if List.mem "A" attrs then
       let mo = get_mo attrs in
       fprintf oc "atomic_store_explicit(&%a, %a, %s)"
         (pp_expr MyLocation.pp) le (pp_expr Value.pp) ve mo
     else
       fprintf oc "%a = %a"
         (pp_expr MyLocation.pp) le (pp_expr Value.pp) ve
  | Litmus.StoreCnd _, _ ->
     (* We've already emitted a CAS for the LoadLink, so we don't emit
        a separate StoreCnd instruction.  (We still print _something_,
        to avoid throwing off the indentation. *)
     fprintf oc "// elided store-conditional instruction"
  | Litmus.Cas (obj,exp,des), attrs ->
     let mo = get_mo attrs in
     pp_cas oc mo obj None exp des

  | Litmus.Fence, attrs ->
     let mo = get_mo attrs in
     fprintf oc "atomic_thread_fence(%s)" mo

let no_braces_needed = function
  | Litmus.Basic (Litmus.Cas _, _) -> false
  | _ -> true

(** [semicolon_needed b] asks whether basic component [b] needs a semicolon
    at the end. *)

let semicolon_needed = function
  | Litmus.StoreCnd _ -> false
  | _ -> true

let pp_instr_and_semicolon oc b =
  pp_instr oc b;
  if semicolon_needed (fst b) then fprintf oc ";" else ()

(** Pretty-printing of components *)
let rec pp_component i oc = function
  | Litmus.Basic b ->
     fprintf oc "%a%a\n" mk_indent i pp_instr_and_semicolon b
  | Litmus.If (r,v,[c]) when no_braces_needed c ->
     fprintf oc "%aif (%a == %a)\n" mk_indent i pp_reg r Value.pp v;
     pp_component (i+1) oc c
  | Litmus.If (r,v,cs) ->
     fprintf oc "%aif (%a == %a) {\n" mk_indent i pp_reg r Value.pp v;
     List.iter (pp_component (i+1) oc) cs;
     fprintf oc "%a}\n" mk_indent i

let partition_locs_in_instr (a_locs, na_locs) = function
  | Litmus.Load (_,le), attrs
  | Litmus.LoadLink (_,le,_,_), attrs
  | Litmus.Store (le,_), attrs
  | Litmus.StoreCnd (le,_), attrs
  | Litmus.Cas (le,_,_), attrs ->
     let l = Litmus.expr_base_of le in
     begin match List.mem "NAL" attrs with
     | true ->
        assert (not (List.mem l a_locs));
        a_locs, MySet.union [l] na_locs
     | false ->
        assert (not (List.mem l na_locs));
        MySet.union [l] a_locs, na_locs
     end
  | _ -> (a_locs, na_locs)
     
let rec partition_locs_in_cmps locs cs =
  List.fold_left partition_locs_in_cmp locs cs

and partition_locs_in_cmp locs = function
  | Litmus.Basic b -> partition_locs_in_instr locs b
  | Litmus.If (_,_,cs) -> partition_locs_in_cmps locs cs
     
let partition_locs lt =
  List.fold_left partition_locs_in_cmps ([],[]) lt.Litmus.thds

(** [contains_regless_cas i] checks to see if [i] has at least one
    CAS without a destination register.  (If one exists, we need to
    create a temporary variable to store it.) *)
let rec contains_regless_cas = function
  | Litmus.Basic (Litmus.Cas (_, _, _), _) -> true
  | Litmus.Basic _ -> false
  | Litmus.If (_,_,cs) -> List.exists contains_regless_cas cs

let rec extract_regs regs = function
  | Litmus.Basic (Litmus.Load (r,_), _)
    | Litmus.Basic (Litmus.LoadLink (r,_,_,_), _) -> MySet.union [r] regs
  | Litmus.Basic _ -> regs
  | Litmus.If (r,_,cs) -> MySet.union [r] (List.fold_left extract_regs regs cs)

let pp oc lt =

  let atomic_locs, nonatomic_locs = partition_locs lt in
  assert (MySet.equal (atomic_locs @ nonatomic_locs) lt.Litmus.locs);

  (* Hoists a piece of pthreads harness code in an include guard, so that
     we can disable it if needed. *)
  let if_pthreads thunk =
    fprintf oc "#ifndef NO_PTHREADS\n";
    thunk ();
    fprintf oc "#endif // NO_PTHREADS\n";
  in

  (* Print an include *)
  let pp_incl oc = fprintf oc "#include <%s.h>\n" in

  (* Include standard headers. *)
  fprintf oc "// Hint: try compiling with gcc -std=c11 <name_of_file.c>\n";
  fprintf oc "\n";
  pp_incl oc "stdio";
  pp_incl oc "stdatomic";
  fprintf oc "\n";
  if_pthreads (fun () -> pp_incl oc "pthread");
  fprintf oc "\n";

  (* Declare global variables. *)
  fprintf oc "// Declaring global variables.\n";
  List.iter
    (fprintf oc "atomic_int %a = ATOMIC_VAR_INIT(0);\n" MyLocation.pp)
    atomic_locs;
  List.iter (fprintf oc "int %a = 0;\n" MyLocation.pp) nonatomic_locs;
  fprintf oc "\n";

  (* Declare thread-local registers as global too, so they can be checked in the postcondition. *)
  fprintf oc "// Declaring thread-local variables at global scope\n";
  fprintf oc "// so they can be checked in the postcondition.\n";
  let regs = List.fold_left (List.fold_left extract_regs) [] lt.Litmus.thds in
  List.iter (fprintf oc "int %a = 0;\n" pp_reg) regs;
  fprintf oc "\n";

  let pp_thd_name oc = fprintf oc "P%d" in

  (* Print a function for each thread. *)
  let pp_thd tid cs =
    fprintf oc "// Thread %d\n" tid;
    fprintf oc "void %a() {\n" pp_thd_name tid;
    if List.exists contains_regless_cas cs then
      fprintf oc "  int expected;\n";
    List.iter (pp_component 1 oc) cs;
    fprintf oc "}\n";
    fprintf oc "\n";
  in
  List.iteri pp_thd lt.Litmus.thds;

  (* Print the test harness, using pthreads. *)
  let pp_pthread_harness () =
    let tids = List.mapi (fun i _ -> i) lt.Litmus.thds in

    let pp_thd_wrapper_name oc = fprintf oc "thread%d" in

    (* Print a pthread thread stub for each thread. *)
    let pp_thd_wrapper tid =
      fprintf oc "// Thread %d: pthread wrapper\n" tid;
      fprintf oc "void *%a(void *unused) {\n" pp_thd_wrapper_name tid;
      fprintf oc "  %a();\n" pp_thd_name tid;
      fprintf oc "  pthread_exit(0);\n";
      fprintf oc "}\n";
      fprintf oc "\n";
    in
    List.iter pp_thd_wrapper tids;

    (* Begin main() routine. *)
    fprintf oc "int main() {\n";
    fprintf oc "\n";

    (* Declare thread-id variables. *)
    fprintf oc "  // Declaring thread-id variables.\n";
    List.iter (fprintf oc "  pthread_t tid%d;\n") tids;
    fprintf oc "\n";

    (* Launch threads. *)
    fprintf oc "  // Launching threads.\n";
    List.iter (fun tid ->
        fprintf oc "  pthread_create(&tid%d, NULL, thread%d, NULL);\n" tid tid
      ) tids;
    fprintf oc "\n";

    (* Join threads. *)
    fprintf oc "  // Joining threads.\n";
    List.iter (fprintf oc "  pthread_join(tid%d, NULL);\n") tids;
    fprintf oc "\n";

    (* Check postcondition. *)
    let pp_cnstrnt oc (a,v) = match a with
      | Litmus.Reg r -> fprintf oc "%a == %d" pp_reg r v
      | Litmus.Loc l -> fprintf oc "%a == %d" MyLocation.pp l v
    in
    fprintf oc "  int result = %a;\n"
            (MyList.pp_gen " && " pp_cnstrnt) lt.Litmus.post;
    fprintf oc "  printf(\"Result: %%d\\n\", result);\n";
    fprintf oc "  return (result);\n";
    fprintf oc "\n";
    fprintf oc "}\n"
  in
  if_pthreads pp_pthread_harness
