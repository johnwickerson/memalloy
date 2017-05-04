(*
MIT License

Copyright (c) 2017 by John Wickerson and Nathan Chong

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

(** Converting a .cat model into an .als file *)

open! Format
open! General_purpose
open Cat_syntax

(* Nasty global variables *)
let verbose = ref false
let fencerels = ref false
let cat_dir : string ref = ref "models"
let out_dir : string ref = ref "."

(** Parse the given .cat file into an abstract syntax tree *)
let parse_file cat_path =
  let cat_path = Filename.concat !cat_dir cat_path in
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  Cat_parser.main Cat_lexer.token lexbuf
       
(** {2 Unfolding recursive definitions} *)

let add_subscript = sprintf "%s_%d"

(** [sub_subscript xs d e] appends a subscript [d] to each variable in [e] that is in the list [xs] *)
let rec sub_subscript xs d = function
  | Empty -> Empty
  | Var x -> Var (if List.mem x xs then add_subscript x d else x)
  | Arg _ -> failwith "Did not expect parameter within recursive def."
  | App (f, es) -> App (f, List.map (sub_subscript xs d) es)
  | Op1 (o, e) -> Op1 (o, sub_subscript xs d e)
  | Op (o, es) -> Op (o, List.map (sub_subscript xs d) es)
				    
(** If [xes] comprises the recursive definitions {i x=e(x,y)} and {i y=f(x,y)}, then [unfold_defs xes u] produces a sequence of non-recursive definitions corresponding to [u] unrollings of [xes]. For instance, [unfold_defs xes 2] produces the sequence {i x0=0}, {i y0=0}, {i x1=e(x0,y0)}, {i y1=f(x0,y0)}, {i x=e(x1,y1)}, {i y=f(x1,y1)}. *)
let unfold_defs xes u =
  let xs = List.map fst xes in
  let rec unfold_defs' d =
    if d = 0 then
      let upd_bind (x,_) = Let (add_subscript x 0, [], Empty) in
      List.map upd_bind xes
    else
      let iter_d =
	let upd_bind (x,e) =
	  let x' = if d=u then x else add_subscript x d in
	  Let (x', [], sub_subscript xs (d-1) e) in
	List.map upd_bind xes
      in
      unfold_defs' (d - 1) @ iter_d
  in
  unfold_defs' u

(** [unfold_instrs u instrs] unrolls each recursive definition in [instrs] by factor [u]. *)
let rec unfold_instrs u = function
  | [] -> []
  | LetRec xes :: instrs ->
     unfold_defs xes u @ unfold_instrs u instrs
  | other_instr :: instrs ->
     other_instr :: unfold_instrs u instrs
				  
(** {2 Determining Alloy types} *)


type set_or_rel = Set | Rel

let pp_set_or_rel oc = function
  | Set -> fprintf oc "set"
  | Rel -> fprintf oc "rel"
    
type def_info = {
    args : set_or_rel list;
    withsc : bool
  }

type ax_info = {
    cnstrnt : cnstrnt;
    withsc_ax : bool
  }

(** Create a typing environment containing the pre-defined sets and relations for the given architecture *)
let build_env withsc arch =
  let rels = Archs.arch_rels arch in
  let rels = if withsc then "s" :: rels else rels in
  let sets = Archs.arch_sets !fencerels arch in
  let mk_info x = (x, {args=[]; withsc=false}) in
  (List.map mk_info rels) @ (List.map mk_info sets)

let alloy_type_of = function Set -> "set E" | Rel -> "E->E"
    
(** Look up type of variable's arguments in typing environment *)
let lookup_var env x =
  try List.assoc x env with Not_found ->
    failwith "Variable %s is unbound in %a"
      x (MyList.pp (fun oc (x,_) -> pp_str oc x)) env

(** Determine type of variable by inspecting the case of its first letter *)
let type_of_var x =
  let first_char = try x.[0] with Invalid_argument -> assert false in
  let c = Char.code first_char in
  if c >= Char.code 'A' && c <= Char.code 'Z' then Set
  else if c >= Char.code 'a' && c <= Char.code 'z' then Rel
  else failwith "Variable %s has an invalid name" x

(** [type_as env tgt expr] attempts to show that [expr] has type [tgt] under environment [env]. It returns [()] if it succeeds and aborts execution if it fails *)
let rec type_as env tgt = function
  | Empty -> ()
  | Var x when tgt = type_of_var x ->
     if (lookup_var env x <> []) then
       failwith "Missing argument(s) for function %s" x;     
  | Arg x when tgt = type_of_var x -> ()
  | App (f, args) when tgt = type_of_var f ->
     if (List.length args <> List.length lookup_var env f) then
       failwith "Wrong number of arguments for function %s" f;
     List.iter2 (type_as env) (lookup_var env f) args
  | Op1 (Set_to_rln,e) when tgt = Rel -> type_as env Set e
  | Op1 (Comp,e) -> type_as env tgt e
  | Op1 (Star,e) | Op1 (Plus,e) | Op1 (Opt,e) | Op1 (Inv,e)
       when tgt = Rel -> type_as env Rel e
  | Op1 (Domain,e) | Op1 (Range,e) when tgt = Set ->
     type_as env Rel e
  | Op (Seq,es) when tgt = Rel ->
     List.iter (type_as env Rel) es
  | Op (Diff,es) | Op (Union,es) | Op (Inter,es) ->
     List.iter (type_as env tgt) es
  | Op (Cross,es) when tgt = Rel ->
     if (List.length es <> 2) then
       failwith "Cross product must have exactly two operands";
     List.iter (type_as env Set) es
  | e -> failwith "Couldn't type %a as a %s"
           pp_expr e pp_set_or_rel tgt

(** Returns the typing environment obtained by processing the instructions in the given .cat file *)
let rec type_file path =
  let model_type, withsc, model = parse_file path in
  let arch = Archs.parse_arch model_type in
  let env = build_env withsc arch in
  List.fold_left type_instr env model
  
(** [type_instr env instr] updates the typing environment [env] with any new entries introduced by processing the instruction [instr] *)
and type_instr env = function
  | Let (x,args,e) ->
     (* NB: we assume all arguments to a function are 
        non-functional themselves *)
     let add_arg a = (a,{args=[]; withsc=false}) in
     let env' = (List.map add_arg args) @ env in
     let tgt = type_of_var x in
     type_as env' tgt e;
     let arg_types = List.map type_of_var args in
     (x, arg_types) :: env
  | LetRec xes ->
     let add_binding (x,_) = (x,{args=[]; withsc=false}) in
     let env = (List.map add_binding xes) @ env in
     let type_binding (x,e) =
       type_as env Rel e;
       if type_of_var x <> Rel then
         failwith "Let-recs can only define relations, not sets"
     in
     List.iter type_binding xes;
     env
  | Axiom _ -> env 
  | Include path -> type_file path @ env

(** Returns the list of axioms declared in the given .cat file *)
let rec extract_axioms path =
  let model_type, withsc, model = parse_file path in
  let arch = Archs.parse_arch model_type in
  List.fold_left (extract_axioms_instr arch withsc) [] model 

(** [extract_axioms_instr axs ins] updates the axiom list [axs] with any new axioms introduced by the instruction [ins] *)
and extract_axioms_instr arch withsc_ax axs = function
  | Axiom (cnstrnt,_,_,n) -> (n,{cnstrnt; withsc_ax}) :: axs
  | Include path -> extract_axioms path @ axs
  | _ -> axs
				    
(** {2 Generating Alloy code} *)

let als_of_shape oc = function
  | Acyclic -> fprintf oc "is_acyclic"
  | Irreflexive -> fprintf oc "irreflexive"
  | IsEmpty -> fprintf oc "is_empty"

(** [replace_vars_with_args args e] converts the variables in [e] that are listed in [args] into "arguments" (which are treated differently when generating Alloy code) *)
let rec replace_vars_with_args args = function
  | Empty -> Empty
  | Var x -> if List.mem x args then Arg x else Var x
  | Arg _ -> failwith "Did not expect an Arg here."
  | App (f,es) -> App (f, List.map (replace_vars_with_args args) es)
  | Op1 (o,e) -> Op1 (o, replace_vars_with_args args e)
  | Op (o,es) -> Op (o, List.map (replace_vars_with_args args) es)

(** Cat expression to Alloy expression *)
let als_of_expr env tgt oc = function
  | Empty when tgt = Set -> fprintf oc "none"
  | Empty when tgt = Rel -> fprintf oc "none -> none"
  | Var x when tgt = type_of_var x ->
     let var_info = lookup_var env x in
     if var_info.args <> [] then
       failwith "Missing argument(s) to function %s" x;
     fprintf oc "%s[e,X%s]" x
       (if var_info.withsc then ",s" else "")
  | Arg x when tgt = type_of_var x ->
     fprintf oc "%s" x
  | App (f,es) when tgt = type_of_var f ->
     let var_info = lookup_var env f in
     let tes =
       try List.combine var_info.args es with Invalid_argument ->
         failwith "Wrong number of arguments for function %s" f
     in
     fprintf oc "%s[%a,e,X%s]" f
       (MyList.pp_gen "," (fun oc (t,e) -> als_of_expr env t oc e)) tes
       (if var_info.withsc then ",s" else "")
  | Op1 (Set_to_rln, e) when tgt = Rel ->
     fprintf oc "stor[%a]" (als_of_expr env Set) e
  | Op1 (Star,e) when tgt = Rel ->
     fprintf oc "*(%a)" (als_of_expr env Rel) e
  | Op1 (Plus,e) when tgt = Rel ->
     fprintf oc "^(%a)" (als_of_expr env Rel) e
  | Op1 (Opt,e) when tgt = Rel ->
     fprintf oc "rc[%a]" (als_of_expr env Rel) e
  | Op1 (Inv,e) when tgt = Rel ->
     fprintf oc "~(%a)" (als_of_expr env Rel) e
  | Op1 (Comp,e) when tgt = Set ->
     let univ = Var "ev" in
     als_of_expr env Set oc (Op (Diff, [univ; e]))
  | Op1 (Comp,e) when tgt = Rel ->
     let univ = Op(Cross, [Var "ev"; Var "ev"]) in
     als_of_expr env Rel oc (Op (Diff, [univ; e])) 
  | Op1 (Domain, e) when tgt = Set ->
     fprintf oc "dom[%a]" (als_of_expr env Rel) e
  | Op1 (Range, e) when tgt = Set ->
     fprintf oc "ran[%a]" (als_of_expr env Rel) e
  | Op (Seq,es) when tgt = Rel ->
     MyList.pp_gen " . " (fparen (als_of_expr env Rel)) oc es
  | Op (Union,es) ->
     MyList.pp_gen " + " (fparen (als_of_expr env tgt)) oc es
  | Op (Diff,es) ->
     MyList.pp_gen " - " (fparen (als_of_expr env tgt)) oc es
  | Op (Inter,es) ->
     MyList.pp_gen " & " (fparen (als_of_expr env tgt)) oc es
  | Op (Cross,es) when tgt = Rel ->
     if (List.length es <> 2) then
       failwith "Cross product must have exactly two operands";
     MyList.pp_gen " -> " (fparen (als_of_expr env Set)) oc es
  | e -> failwith "Couldn't type %a as a %s"
           pp_expr e pp_set_or_rel tgt

(** [als_of_axiom oc (s,e)] converts the axiom [s(e)] into an Alloy expression, which is sent to [oc] *) 
let als_of_axiom env oc (s, e) =
  fprintf oc "%a[%a]" als_of_shape s (als_of_expr env Rel) e

let als_of_cnstrnt oc = function
  | Provision -> fprintf oc "consistent"
  | UndefUnless -> fprintf oc "racefree"
  | Deadness -> fprintf oc "dead"

(** Generates the first part of the Alloy file *)
let preamble cat_path model_name arch oc =
  fprintf oc "/* Automatically generated from %s on %s at %s */\n\n"
	  cat_path (MyUnix.today ()) (MyUnix.now ());
  fprintf oc "module %s[E]\n" model_name;
  fprintf oc "open %a[E]\n\n" (Archs.pp_arch !fencerels) arch
	  
(** Generates the final part of the Alloy file *)
let postamble withsc arch axs oc c =
  let axs = List.filter (fun (_,ax_info) -> ax_info.cnstrnt = c) axs in
  let extra_rels = Archs.arch_rels_min arch in
  fprintf oc "pred %a[e:E, X:%a%a] {\n"
	  als_of_cnstrnt c Archs.pp_Arch arch
	  als_args_of_extra_rels extra_rels;
  if withsc then fprintf oc "  some s:E->E {\n";
  if withsc then fprintf oc "    wf_s[e,X%a,s]\n"
			 als_of_extra_rels extra_rels;
  let indent = if withsc then "    " else "  " in
  let pp_ax (n,ax_info) =
    fprintf oc "%s%s[e,X%a%s]\n" indent n
	    als_of_extra_rels ax_info.extra_rels_ax
	    (if ax_info.withsc_ax then ",s" else "")
  in
  List.iter pp_ax (List.rev axs);
  if withsc then fprintf oc "  }\n";
  fprintf oc "}\n"
	  
(** [als_of_instr withsc arch u oc (env, axs, defs) ins] converts the .cat instruction [ins] into Alloy code (which is sent to [oc]), assuming architecture [arch], unrolling factor [u], typing environment [env], axiom list [axs], definition list [defs], and [withsc] controlling whether the {i S} order is included. Returns an updated typing environment, axiom list, and definition list. *)
let rec als_of_instr withsc arch unrolling oc (env, axs) = function
  | Let (x,args,e) ->
     let e = replace_vars_with_args args e in
     let e = if withsc then replace_vars_with_args ["s"] e else e in
     let x_type = type_of_var x in
     let pp_arg oc arg =
       fprintf oc "%s:%s," arg (alloy_type_of (type_of_var arg))
     in
     fprintf oc "fun %s [%se:E, X:%a%s] : %s {\n"
       x (MyList.pp_gen "" pp_arg) args Archs.pp_Arch arch
       (if withsc then ", s:E->E" else "")
       (alloy_type_of x_type);
     fprintf oc "  %a\n" (als_of_expr env x_type) e;
     fprintf oc "}\n\n";
     let new_env = (x, {args=List.map type_of_var args; withsc})) in
     (new_env :: env, axs)
  | LetRec _ ->
     failwith "Recursive definition should have already been removed."
  | Axiom (cnstrnt,s,e,n) ->
     fprintf oc "pred %s [e:E, X:%a%s] {\n"
       n Archs.pp_Arch arch
       (if withsc then ", s:E->E" else "");
     let e = if withsc then replace_vars_with_args ["s"] e else e in
     fprintf oc "  %a\n" (als_of_axiom env) (s, e);
     fprintf oc "}\n\n";
     (env, (n, {cnstrnt; withsc_ax=withsc}) :: axs)
  | Include cat_path ->
     let env',axs' = als_of_file true unrolling cat_path in
     fprintf oc "open %s[E]\n\n" (Filename.chop_extension cat_path);
     (env' @ env, axs @ axs')

and als_of_file interm_model unrolling cat_path =
  let model_name =
    Filename.chop_extension (Filename.basename cat_path)
  in
  let als_path = sprintf "%s.als" model_name in
  if !verbose then
    printf "Converting %s to %s.\n" cat_path als_path;
  let als_path = Filename.concat "models" als_path in
  let oc = open_out als_path in
  let ppf = formatter_of_out_channel oc in
  let model_type, withsc, cat_model = parse_file cat_path in
  let arch = Archs.parse_arch model_type in
  let cat_model = unfold_instrs unrolling cat_model in
  preamble cat_path model_name arch ppf;
  let env = build_env withsc arch in
  let env,axs =
    List.fold_left
      (als_of_instr withsc arch unrolling ppf) (env, []) cat_model
  in
  if (not interm_model) then begin
      postamble withsc arch axs ppf Provision;
      postamble false arch axs ppf UndefUnless;
      postamble false arch axs ppf Deadness
    end;
  close_out oc;
  env,axs
  
(** {2 Processing command-line input} *)
	       
let get_args () =
  let cat_path : string ref = ref "" in
  let unrolling_factor : int ref = ref 3 in
  let intermediate_model : bool ref = ref false in
  let speclist = [
      ("-fencerels", Arg.Set fencerels,
       "Encode fences as relations rather than events");
      ("-u", Arg.Set_int unrolling_factor,
       sprintf "Number of times to unroll recursive definitions (optional, default=%d)" !unrolling_factor);
      ("-i", Arg.Set intermediate_model,
       sprintf "Intermediate model; do not generate `consistent` predicate (optional, default=%b)" !intermediate_model);
      ("-o", Arg.Set_string out_dir,
       sprintf "Output directory (default=%s)" !out_dir);
      ("-verbose", Arg.Set verbose, sprintf "default=%b" !verbose);
    ]
  in
  let usage_msg =
    "A translator from the .cat format into the .als (Alloy) format.\nUsage: `cat2als [options] <cat_file.cat>`.\nOptions available:"
  in
  Arg.parse speclist (fun filename -> cat_path := filename) usage_msg;
  begin match Sys.file_exists !cat_path with
  | false -> failwith "Could not find cat file %s" !cat_path
  | _ -> ()
  end;
  let dir = Filename.dirname !cat_path in
  let cat_file = Filename.basename !cat_path in
  let _ = cat_dir := dir in
  cat_file, !unrolling_factor, !intermediate_model
		  
let main () =
  let cat_path, unrolling_factor, interm_model = get_args () in
  assert (unrolling_factor >= 0);
  als_of_file interm_model unrolling_factor cat_path;
  exit 0
    
let _ = main ()
