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

(** Converting a .cat model into an .als file *)

open Format
open General_purpose
open Cat_syntax

(** Parse the given .cat file into an abstract syntax tree *)
let parse_file cat_path =
  let cat_path = Filename.concat "models" cat_path in
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  Cat_parser.main Cat_lexer.token lexbuf
       
(** {2 Unfolding recursive definitions} *)

let add_subscript = sprintf "%s_%d"

(** [sub_subscript xs d e] appends a subscript [d] to each variable in [e] that is in the list [xs] *)
let rec sub_subscript xs d = function
  | Empty_rln -> Empty_rln
  | Var x -> Var (if List.mem x xs then add_subscript x d else x)
  | Arg x -> failwith "Did not expect parameter within recursive def."
  | App (f, es) -> App (f, List.map (sub_subscript xs d) es)
  | Op1 (o, e) -> Op1 (o, sub_subscript xs d e)
  | Op (o, es) -> Op (o, List.map (sub_subscript xs d) es)
				    
(** If [xes] comprises the recursive definitions {i x=e(x,y)} and {i y=f(x,y)}, then [unfold_defs xes u] produces a sequence of non-recursive definitions corresponding to [u] unrollings of [xes]. For instance, [unfold_defs xes 2] produces the sequence {i x0=0}, {i y0=0}, {i x1=e(x0,y0)}, {i y1=f(x0,y0)}, {i x=e(x1,y1)}, {i y=f(x1,y1)}. *)
let unfold_defs xes u =
  let xs = List.map fst xes in
  let rec unfold_defs' d =
    if d = 0 then
      let upd_bind (x,_) = Let (add_subscript x 0, [], Empty_rln) in
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

(** Create a typing environment and list of definitions containing the pre-defined sets and relations for the given architecture *)
let build_env withsc arch =
  let rels = Archs.arch_rels arch in
  let rels = if withsc then "s" :: rels else rels in
  let sets = Archs.arch_sets arch in
  let env = 
    (List.map (fun a -> (a,([],Rel))) rels) @
      (List.map (fun a -> (a,([],Set))) sets)
  in
  let defs =
    (List.map (fun a -> (a,false)) rels) @
      (List.map (fun a -> (a,false)) sets)
  in
  env, defs

  let alloy_type_of = function Set -> "set E" | Rel -> "E->E"

(** Print environment (for debugging) *)
let pp_env oc =
  List.iter (fun (x,(_,t)) -> fprintf oc "%s:%s\n" x (pp_typ t))
	    
(** Look up type of variable in typing environment *)
let type_of_var env x =
  try List.assoc x env with Not_found ->
    failwith "Variable %s is unbound in [\n%a]" x pp_env env

(** If [es] is a non-empty list of expressions, all with type {i t}, [type_list env es] returns {i t}. *)
let rec type_list env = function
  | [] -> assert false
  | e :: es ->
     let t = type_of env e in
     try
       let e' = List.find (fun e' -> type_of env e' <> t) es in
       failwith "%a has type %s but %a has type %s"
		pp_expr e (pp_typ t)
		pp_expr e' (pp_typ (type_of env e'))
     with Not_found -> t

(** [type_of env e] returns the type of the expression [e] according to the typing environment [env]. *)
and type_of env = function
  | Empty_rln -> Rel
  | Var x -> begin
      match type_of_var env x with
      | [], ret_type -> ret_type
      | _, _ ->
	 failwith "Missing argument for %s." x
    end
  | Arg x -> Rel (* assume arguments are relational *)
  | App (f, args) ->
     let args_type, ret_type = type_of_var env f in
     let num_formals = List.length args_type in
     let num_actuals = List.length args in
     if num_formals != num_actuals then
       failwith "%s expects %d arguments, given %d" f
		num_formals num_actuals;
     assert
       (List.for_all2 (fun a t -> type_of env a = t) args args_type);
     ret_type
  | Op1 (Set_to_rln,e) ->
     assert (type_of env e = Set); Rel
  | Op1 (Comp Set,e) ->
     assert (type_of env e = Set); Set
  | Op1 (Star,e)
  | Op1 (Plus,e)
  | Op1 (Opt,e)
  | Op1 (Inv,e)
  | Op1 (Comp Rel,e)
  | Op1 (Select(_,_), e) ->
     assert (type_of env e = Rel); Rel
  | Op1 (Domain,e)
  | Op1 (Range,e) ->
     assert (type_of env e = Rel); Set
  | Op (Seq,es) ->
     assert (List.for_all (fun e -> type_of env e = Rel) es);
     Rel
  | Op (Diff,es)
  | Op (Union,es)
  | Op (Inter,es) ->
     assert (es != []);
     type_list env es
  | Op (Cross,es) ->
     assert (List.length es = 2);
     assert (type_of env (List.nth es 0) = Set);
     assert (type_of env (List.nth es 1) = Set);
     Rel

(** Returns the typing environment obtained by processing the instructions in the given .cat file *)
let rec type_file path =
  let model_type, withsc, model = parse_file path in
  let arch = Archs.parse_arch model_type in
  let env,_ = build_env withsc arch in
  List.fold_left type_instr env model

(** [type_instr env instr] updates the typing environment [env] with any new entries introduced by processing the instruction [instr] *)
and type_instr env = function
  | Let (x,args,e) ->
     (* NB we assume all args are relations, not sets *)
     let env' = (List.map (fun a -> (a,([],Rel))) args) @ env in
     let def_type = type_of env' e in
     let arg_type = List.map (fun _ -> Rel) args in
     (x, (arg_type, def_type)) :: env
  | LetRec xes ->
     (List.map (fun (x,e) -> (x, ([], Rel))) xes) @ env
  | Axiom _ -> env 
  | Include path -> type_file path @ env

(** Returns the list of definitions in the given .cat file, and whether each is in 'withsc' mode *)
let rec extract_defs path =
  let _, withsc, model = parse_file path in
  List.fold_left (extract_defs_instr withsc) [] model

(** [extract_defs_instr defs ins] updates the definition list [axs] with any new definitions introduced by the instruction [ins] *)
and extract_defs_instr withsc defs = function
  | Let (x,_,_) -> (x,withsc) :: defs
  | LetRec xes -> (List.map (fun (x,_) -> (x,withsc)) xes) @ defs
  | Include path -> extract_defs path @ defs
  | _ -> defs

(** Returns the list of axioms declared in the given .cat file *)
let rec extract_axioms path =
  let _, withsc, model = parse_file path in
  List.fold_left (extract_axioms_instr withsc) [] model 

(** [extract_axioms_instr axs ins] updates the axiom list [axs] with any new axioms introduced by the instruction [ins] *)
and extract_axioms_instr withsc axs = function
  | Axiom (c,_,_,n) -> (n,c,withsc) :: axs
  | Include path -> extract_axioms path @ axs
  | _ -> axs
				    
(** {2 Generating Alloy code} *)

let als_of_access_type = function
  | WriteRead -> Var "M"
  | Write -> Var "W"
  | Read -> Var "R"
  | Plain -> Op (Diff, [Var "ev"; Var "locked"])
  | Atomic -> Var "locked"

let als_of_shape oc = function
  | Acyclic -> fprintf oc "is_acyclic"
  | Irreflexive -> fprintf oc "irreflexive"
  | IsEmpty -> fprintf oc "is_empty"

(** Cat expression to Alloy expression *)
let als_of_expr defs oc e =
  let rec als_of_expr' oc = function
    | Empty_rln -> fprintf oc "none -> none"
    | Var x ->
       let withsc =
	 try List.assoc x defs
	 with Not_found -> failwith "Unbound variable %s" x
       in
       fprintf oc "%s[e,X%s]" x (if withsc then ",s" else "")
    | Arg x -> fprintf oc "%s" x
    | App (f,es) ->
       let withsc = List.assoc f defs in
       fprintf oc "%s[%a,e,X%s]" f (MyList.pp "," als_of_expr') es
	       (if withsc then ",s" else "")
    | Op1 (Set_to_rln,e) -> fprintf oc "stor[%a]" als_of_expr' e
    | Op1 (Star,e) -> fprintf oc "*(%a)" als_of_expr' e
    | Op1 (Plus,e) -> fprintf oc "^(%a)" als_of_expr' e
    | Op1 (Opt,e) -> fprintf oc "rc[%a]" als_of_expr' e
    | Op1 (Inv,e) -> fprintf oc "~(%a)" als_of_expr' e
    | Op1 (Comp t,e) ->
       let univ =
	 match t with
	 | Set -> Var "ev"
	 | Rel -> Op(Cross, [Var "ev"; Var "ev"])
       in
       als_of_expr' oc (Op (Diff, [univ; e]))
    | Op1 (Select(t1,t2), e) ->
       let prod = Op(Cross, List.map als_of_access_type [t1;t2]) in
       als_of_expr' oc (Op (Inter, [e; prod]))
    | Op1 (Domain, e) -> fprintf oc "dom[%a]" als_of_expr' e
    | Op1 (Range, e) -> fprintf oc "ran[%a]" als_of_expr' e
    | Op (Seq,es) -> MyList.pp " . " (fparen als_of_expr') oc es
    | Op (Union,es) -> MyList.pp " + " (fparen als_of_expr') oc es
    | Op (Diff,es) -> MyList.pp " - " (fparen als_of_expr') oc es
    | Op (Inter,es) -> MyList.pp " & " (fparen als_of_expr') oc es
    | Op (Cross,es) -> MyList.pp " -> " (fparen als_of_expr') oc es
  in
  als_of_expr' oc e

(** [replace_vars_with_args args e] converts the variables in [e] that are listed in [args] into "arguments" (which are treated differently when generating Alloy code) *)
let rec replace_vars_with_args args = function
  | Empty_rln -> Empty_rln
  | Var x -> if List.mem x args then Arg x else Var x
  | Arg x -> failwith "Did not expect an Arg here."
  | App (f,es) -> App (f, List.map (replace_vars_with_args args) es)
  | Op1 (o,e) -> Op1 (o, replace_vars_with_args args e)
  | Op (o,es) -> Op (o, List.map (replace_vars_with_args args) es)

(** [als_of_axiom oc (s,e)] converts the axiom [s(e)] into an Alloy expression, which is sent to [oc] *) 
let als_of_axiom defs oc (s, e) =
  fprintf oc "%a[%a]" als_of_shape s (als_of_expr defs) e

let als_of_cnstrnt oc = function
  | Provision -> fprintf oc "consistent"
  | UndefUnless -> fprintf oc "racefree"
  | Deadness -> fprintf oc "dead"

(** Generates the first part of the Alloy file *)
let preamble cat_path model_name arch oc =
  fprintf oc "/* Automatically generated from %s on %s at %s */\n\n"
	  cat_path (MyUnix.today ()) (MyUnix.now ());
  fprintf oc "module %s[E]\n" model_name;
  fprintf oc "open %a[E]\n\n" Archs.pp_arch arch

(** Generates the final part of the Alloy file *)
let postamble withsc arch axs oc c =
  let axs = List.filter (fun (_,c',_) -> c = c') axs in
  fprintf oc "pred %a[e:E, X:%a] {\n"
	  als_of_cnstrnt c Archs.pp_Arch arch;
  if withsc then fprintf oc "  some s:E->E {\n";
  if withsc then fprintf oc "    wf_s[e,X,s]\n";
  let indent = if withsc then "    " else "  " in
  let pp_ax (n,_,withsc) =
    fprintf oc "%s%s[e,X%s]\n" indent n (if withsc then ",s" else "")
  in
  List.iter pp_ax (List.rev axs);
  if withsc then fprintf oc "  }\n";
  fprintf oc "}\n"
	  
(** [als_of_instr withsc arch u oc (env, axs, defs) ins] converts the .cat instruction [ins] into Alloy code (which is sent to [oc]), assuming architecture [arch], unrolling factor [u], typing environment [env], axiom list [axs], definition list [defs], and [withsc] controlling whether the {i S} order is included. Returns an updated typing environment, axiom list, and definition list. *)
let rec als_of_instr withsc arch unrolling oc (env, axs, defs) = function
  | Let (x,args,e) ->
     (* NB we assume all args are relations, not sets *)
     let e = replace_vars_with_args args e in
     let e = if withsc then replace_vars_with_args ["s"] e else e in
     let def_type = type_of env e in
     let args_str = List.fold_left (sprintf "%s%s:E->E,") "" args in
     fprintf oc "fun %s [%se:E, X:%a%s] : %s {\n"
	     x args_str Archs.pp_Arch arch
	     (if withsc then ", s:E->E" else "")
	     (alloy_type_of def_type);
     fprintf oc "  %a\n" (als_of_expr defs) e;
     fprintf oc "}\n\n";
     let new_env = (x, (List.map (fun _ -> Rel) args, def_type)) in
     (new_env :: env, axs, (x,withsc) :: defs)
  | LetRec _ ->
     failwith "Recursive definition should have already been removed."
  | Axiom (c,s,e,n) ->
     fprintf oc "pred %s [e:E, X:%a%s] {\n" n Archs.pp_Arch arch
	     (if withsc then ", s:E->E" else "");
     let e = if withsc then replace_vars_with_args ["s"] e else e in
     fprintf oc "  %a\n" (als_of_axiom defs) (s, e);
     fprintf oc "}\n\n";
     (env, (n,c,withsc) :: axs, defs)
  | Include cat_path ->
     als_of_file true unrolling cat_path;
     let env' = type_file cat_path in
     let axs' = extract_axioms cat_path in
     let defs' = extract_defs cat_path in
     fprintf oc "open %s[E]\n\n" (Filename.chop_extension cat_path);
     (env' @ env, axs @ axs', defs @ defs')

and als_of_file interm_model unrolling cat_path =
  let model_name =
    Filename.chop_extension (Filename.basename cat_path)
  in
  let als_path = sprintf "%s.als" model_name in
  printf "Converting %s to %s.\n" cat_path als_path;
  let als_path = Filename.concat "models" als_path in
  let oc = open_out als_path in
  let ppf = formatter_of_out_channel oc in
  let model_type, withsc, cat_model = parse_file cat_path in
  let arch = Archs.parse_arch model_type in
  let cat_model = unfold_instrs unrolling cat_model in
  preamble cat_path model_name arch ppf;
  let env,defs = build_env withsc arch in
  let _,axs,_ =
    List.fold_left
      (als_of_instr withsc arch unrolling ppf) (env, [], defs) cat_model
  in
  if (not interm_model) then begin
      postamble withsc arch axs ppf Provision;
      postamble false arch axs ppf UndefUnless;
      postamble false arch axs ppf Deadness
    end;
  close_out oc
	    

	    (*	  
(** {2 Processing command-line input} *)
	       
let get_args () =
  let cat_path : string list ref = ref [] in
  let default_unrolling_factor = 3 in
  let unrolling_factor : int list ref = ref [] in
  let intermediate_model : bool ref = ref false in
  let speclist = [
      ("-u", Arg.Int (set_list_ref unrolling_factor),
       sprintf "Number of times to unroll recursive definitions (optional, default=%d)" default_unrolling_factor);
      ("-i", Arg.Set intermediate_model,
       sprintf "Intermediate model; do not generate `consistent` predicate (optional)");
    ]
  in
  let usage_msg =
    "A translator from the .cat format into the .als (Alloy) format.\nUsage: `cat2als [options] <cat_file.cat>`.\nOptions available:"
  in
  Arg.parse speclist (set_list_ref cat_path) usage_msg;
  let bad_arg () =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  let cat_path = get_only_element bad_arg !cat_path in
  let unrolling_factor =
    get_lone_element bad_arg default_unrolling_factor !unrolling_factor
  in
  (cat_path, unrolling_factor, !intermediate_model)

let check_args (cat_path, unrolling_factor, interm_model) =
  assert (unrolling_factor >= 0)
		  
let main () =
  let (cat_path, unrolling_factor, interm_model) = get_args () in
  check_args (cat_path, unrolling_factor, interm_model);
  let model_name =
    Filename.chop_extension (Filename.basename cat_path)
  in
  let als_file = sprintf "%s.als" model_name in
  let als_dir = Filename.concat Filename.parent_dir_name "models_als" in
  let als_path = Filename.concat als_dir als_file in
  if Sys.file_exists als_path then
    failwith "Target Alloy file already exists.";
  let oc = formatter_of_out_channel (open_out als_path) in
  let (model_type, cat_model) = parse_file cat_path in
  let arch = Archs.parse_arch model_type in
  let cat_model = unfold_instrs unrolling_factor cat_model in
  preamble cat_path model_name arch oc;
  let env = build_env arch in
  let _,axs =
    List.fold_left (als_of_instr arch oc) (env, []) cat_model
  in
  begin
  if (not interm_model) then
    let mk_postamble c =
      postamble arch c (List.filter (fun (_,c') -> c = c') axs) oc
    in
    List.iter mk_postamble [Provision; UndefUnless; Deadness]
  end;
  exit 0
    
let _ = main ()     
	     *)