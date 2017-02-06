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
open General_purpose
open Cat_syntax

let debug = debug false

(***********************************)
(* Unfolding recursive definitions *)
(***********************************)

let add_subscript = sprintf "%s_%d"

let rec sub_subscript xs d = function
  | Empty_rln -> Empty_rln
  | Var x -> Var (if List.mem x xs then add_subscript x d else x)
  | Arg x -> failwith "Did not expect parameter within recursive def."
  | App (f, es) -> App (f, List.map (sub_subscript xs d) es)
  | Op1 (o, e) -> Op1 (o, sub_subscript xs d e)
  | Op (o, es) -> Op (o, List.map (sub_subscript xs d) es)
				    
let rec unfold_defs' xs d = function
  | [] -> []
  | (x,e) :: rest ->
     Let (add_subscript x d, [], sub_subscript xs (d-1) e) ::
       unfold_defs' xs d rest
	    
let rec unfold_defs xes d u =
  let xs = List.map fst xes in
  if d = 0 then
    List.map (fun (x,_) -> Let (add_subscript x 0, [], Empty_rln)) xes
  else
    let iter_d =
      if d = u then
	List.map
	  (fun (x,e) -> Let (x, [], sub_subscript xs (d-1) e)) xes
      else
	unfold_defs' xs d xes
    in
    unfold_defs xes (d - 1) u @ iter_d
				    
let rec unfold_instrs u = function
  | [] -> []
  | LetRec xes :: instrs ->
     unfold_defs xes u u @ unfold_instrs u instrs
  | other_instr :: instrs ->
     other_instr :: unfold_instrs u instrs
				  
(***************************)
(* Determining Alloy types *)
(***************************)

let build_env arch = 
  (List.map (fun a -> (a,([],Rel))) (Archs.arch_rels arch)) @
  (List.map (fun a -> (a,([],Set))) (Archs.arch_sets arch))

let alloy_type_of = function Set -> "set E" | Rel -> "E->E"

let pp_env oc =
  List.iter (fun (x,(_,t)) -> fprintf oc "%s:%s\n" x (pp_typ t))
	    
let type_of_var env x =
  try List.assoc x env with Not_found ->
    failwith (asprintf "Variable %s is unbound in [\n%a]" x pp_env env)
     
let rec type_list env e = function
  | [] -> type_of env e
  | e' :: es ->
     let t = type_of env e and t' = type_of env e' in
     if t = t' then
       type_list env e es
     else
       failwith (asprintf
		   "%a has type %s but %a has type %s"
		   pp_expr e (pp_typ t) pp_expr e' (pp_typ t'))
	     
and type_of env = function
  | Empty_rln -> Rel
  | Var x -> begin
      match type_of_var env x with
      | [], ret_type -> ret_type
      | _, _ ->
	 failwith (asprintf "Missing argument for %s." x)
    end
  | Arg x -> Rel (* assume arguments are relational *)
  | App (f, args) ->
     let args_type, ret_type = type_of_var env f in
     let num_formals = List.length args_type in
     let num_actuals = List.length args in
     if num_formals != num_actuals then
       failwith (asprintf "%s expects %d arguments; given %d"
		   f num_formals num_actuals);
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
     type_list env (List.hd es) (List.tl es)
  | Op (Cross,es) ->
     assert (List.length es = 2);
     assert (type_of env (List.nth es 0) = Set);
     assert (type_of env (List.nth es 1) = Set);
     Rel
       
let parse_file cat_path =
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  Cat_parser.main Cat_lexer.token lexbuf

let rec type_instr env = function
  | Let (x,args,e) ->
     (* NB we assume all args are relations, not sets *)
     let env' = (List.map (fun a -> (a,([],Rel))) args) @ env in
     let def_type = type_of env' e in
     (x, (List.map (fun _ -> Rel) args, def_type)) :: env
  | LetRec xes ->
     (List.map (fun (x,e) -> (x, ([], Rel))) xes) @ env
  | Axiom _ -> env 
  | Include path -> type_file path @ env
		  
and type_file path =
  let model_type, model = parse_file path in
  let arch = Archs.parse_arch model_type in
  let env = build_env arch in
  List.fold_left type_instr env model

let rec extract_axioms_instr axs = function
  | Axiom (c,_,_,n) -> (n,c)::axs
  | Include path -> extract_axioms path @ axs
  | _ -> axs

and extract_axioms path =
  let (_, model) = parse_file path in
  List.fold_left extract_axioms_instr [] model
				    
(*************************)
(* Generating Alloy code *)
(*************************)
		       
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
		       
let rec als_of_expr oc = function
  | Empty_rln -> fprintf oc "none -> none"
  | Var x -> fprintf oc "%s[e,X]" x
  | Arg x -> fprintf oc "%s" x
  | App (f,es) -> fprintf oc "%s[%ae,X]" f als_of_exprs es
  | Op1 (Set_to_rln,e) -> fprintf oc "stor[%a]" als_of_expr e
  | Op1 (Star,e) -> fprintf oc "*(%a)" als_of_expr e
  | Op1 (Plus,e) -> fprintf oc "^(%a)" als_of_expr e
  | Op1 (Opt,e) -> fprintf oc "rc[%a]" als_of_expr e
  | Op1 (Inv,e) -> fprintf oc "~(%a)" als_of_expr e
  | Op1 (Comp t,e) ->
     let univ =
       match t with
	 Set -> Var "ev"
       | Rel -> Op(Cross, [Var "ev"; Var "ev"])
     in
     als_of_expr oc (Op (Diff, [univ; e]))
  | Op1 (Select(t1,t2), e) ->
     let prod = Op(Cross, List.map als_of_access_type [t1;t2]) in
     als_of_expr oc (Op (Inter, [e; prod]))
  | Op1 (Domain, e) -> fprintf oc "dom[%a]" als_of_expr e
  | Op1 (Range, e) -> fprintf oc "ran[%a]" als_of_expr e
  | Op (Seq,es) -> fprintf_iter " . " (fparen als_of_expr) oc es
  | Op (Union,es) -> fprintf_iter " + " (fparen als_of_expr) oc es
  | Op (Diff,es) -> fprintf_iter " - " (fparen als_of_expr) oc es
  | Op (Inter,es) -> fprintf_iter " & " (fparen als_of_expr) oc es
  | Op (Cross,es) -> fprintf_iter " -> " (fparen als_of_expr) oc es

and als_of_exprs oc = function
  | [] -> fprintf oc ""
  | e :: es -> fprintf oc "%a, %a" als_of_expr e als_of_exprs es

let rec replace_vars_with_args args = function
  | Empty_rln -> Empty_rln
  | Var x -> if List.mem x args then Arg x else Var x
  | Arg x -> failwith "Did not expect an Arg here."
  | App (f,es) -> App (f, List.map (replace_vars_with_args args) es)
  | Op1 (o,e) -> Op1 (o, replace_vars_with_args args e)
  | Op (o,es) -> Op (o, List.map (replace_vars_with_args args) es)
		       
let als_of_instr arch oc (env, axs) = function
  | Let (x,args,e) ->
     (* NB we assume all args are relations, not sets *)
     let e = replace_vars_with_args args e in
     let def_type = type_of env e in
     let args_str = List.fold_left (sprintf "%s%s:E->E,") "" args in
     fprintf oc "fun %s [%se:E, X:%a] : %s {\n"
	     x args_str Archs.pp_Arch arch (alloy_type_of def_type);
     fprintf oc "  %a\n" als_of_expr e;
     fprintf oc "}\n\n";
     let env' =
       (x, (List.map (fun _ -> Rel) args, def_type)) :: env
     in
     (env', axs)
  | LetRec _ ->
     failwith "Recursive definition should have been removed."
  | Axiom (c,s,e,n) ->
     fprintf oc "pred %s [e:E, X:%a] {\n" n Archs.pp_Arch arch; 
     fprintf oc "  %a[%a]\n" als_of_shape s als_of_expr e;
     fprintf oc "}\n\n";
     (env, (n,c) :: axs)
  | Include path ->
     let env' = type_file path in
     let axs' = extract_axioms path in
     fprintf oc "open %s[E]\n\n" (chop_extension ".cat" path);
     (env' @ env, axs @ axs')

let als_of_cnstrnt oc = function
  | Provision -> fprintf oc "consistent"
  | UndefUnless -> fprintf oc "racefree"
  | Deadness -> fprintf oc "dead"
		   
let preamble cat_path model_name arch oc =
  fprintf oc "// %a\n\n" Archs.pp_Arch arch;
  fprintf oc "/* Automatically generated from %s on %s at %s */\n\n"
	  cat_path (today ()) (now ());
  fprintf oc "module %s[E]\n" model_name;
  fprintf oc "open %a[E]\n\n" Archs.pp_arch arch

let postamble arch c axs oc =
  fprintf oc "pred %a[e:E, X:%a] {\n"
	  als_of_cnstrnt c Archs.pp_Arch arch;
  List.iter (fun (n,_) -> fprintf oc "  %s[e,X]\n" n) (List.rev axs);
  fprintf oc "}\n"
			
let als_of_model intermediate env cat_path model_name arch oc instrs =
  preamble cat_path model_name arch oc;
  let _,axs = List.fold_left (als_of_instr arch oc) (env, []) instrs
  in
  if (not intermediate) then
    let mk_postamble c =
      postamble arch c (List.filter (fun (_,c') -> c = c') axs) oc
    in
    List.iter mk_postamble [Provision; UndefUnless; Deadness]

(*********************************)
(* Processing command-line input *)
(*********************************)
	       
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
  let env = build_env arch in
  let cat_model = unfold_instrs unrolling_factor cat_model in
  debug "Cat model: %a" pp_instrs cat_model;
  als_of_model interm_model env cat_path model_name arch oc cat_model; 
  exit 0
    
let _ = main ()     
