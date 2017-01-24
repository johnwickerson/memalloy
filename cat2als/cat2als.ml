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

let debug = debug true
				
(*************************************)
(* Renaming a few built-in variables *)
(*************************************)

let rename_var = function
  | "addr" -> "ad"
  | "A" -> "scacq"
  | "ctrl" -> "cd"
  | "data" -> "dd"
  | "DMBSY" -> "dmb"
  | "DMBST" -> "dmbst"
  | "DMBLD" -> "dmbld"
  | "int" -> "sthd"
  | "ISB" -> "isb"
  | "L" -> "screl"
  | "po" -> "sb"
  | "rmw" -> "atom"
  | "loc" -> "sloc"
  | x -> x
		       
let rec rename_vars_in_expr = function
  | Empty_rln -> Empty_rln
  | Var x -> Var (rename_var x)
  | App (f,es) -> App (rename_var f, List.map rename_vars_in_expr es)
  | Op1 (o,e) -> Op1 (o, rename_vars_in_expr e)
  | Op (o,es) -> Op (o, List.map rename_vars_in_expr es)
		       
let rename_vars_in_instr = function
  | Let (x,args,e) ->
     Let (rename_var x, List.map rename_var args,
	  rename_vars_in_expr e)
  | LetRec xes ->
     let xes' =
       List.map (fun (x,e) ->
	 (rename_var x, rename_vars_in_expr e)) xes
     in
     LetRec (xes')
  | Test (t,e,n) -> Test (t, rename_vars_in_expr e, n)
		       
let rename_vars_in_model = List.map rename_vars_in_instr

(***********************************)
(* Unfolding recursive definitions *)
(***********************************)

let u = ref 5

let add_subscript = sprintf "%s_%d"

let rec sub_subscript xs d = function
  | Empty_rln -> Empty_rln
  | Var x -> Var (if List.mem x xs then add_subscript x d else x)
  | App (f, es) -> App (f, List.map (sub_subscript xs d) es)
  | Op1 (o, e) -> Op1 (o, sub_subscript xs d e)
  | Op (o, es) -> Op (o, List.map (sub_subscript xs d) es)
				    
let rec unfold_defs' xs d = function
  | [] -> []
  | (x,e) :: rest ->
     Let (add_subscript x d, [], sub_subscript xs (d-1) e) ::
       unfold_defs' xs d rest
	    
let rec unfold_defs xes d =
  let xs = List.map fst xes in
  if d = 0 then
    List.map (fun (x,_) -> Let (add_subscript x 0, [], Empty_rln)) xes
  else
    let iter_d =
      if d = !u then
	List.map
	  (fun (x,e) -> Let (x, [], sub_subscript xs (d-1) e)) xes
      else
	unfold_defs' xs d xes
    in
    unfold_defs xes (d - 1) @ iter_d
				    
let rec unfold_instrs = function
  | [] -> []
  | LetRec xes :: instrs ->
     unfold_defs xes !u @ unfold_instrs instrs
  | other_instr :: instrs ->
     other_instr :: unfold_instrs instrs
				    
(***************************)
(* Determining Alloy types *)
(***************************)

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
  | App (f, args) ->
     let (args_type, ret_type) = type_of_var env f in
     assert (List.length args_type = List.length args);
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
				    
(*************************)
(* Generating Alloy code *)
(*************************)
		       
let pp_access_type = function
  | WriteRead -> Op (Union, [Var "W"; Var "R"])
  | Write -> Var "W"
  | Read -> Var "R"
  | Plain -> Op (Diff, [Var "ev"; Var "A"])
  | Atomic -> Var "A"
		  
let rec als_of_expr oc = function
  | Empty_rln -> fprintf oc "none"
  | Var x -> fprintf oc "%s[e,X]" x
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
     let prod = Op(Cross, List.map pp_access_type [t1;t2]) in
     als_of_expr oc (Op (Inter, [e; prod]))
  | Op1 (Domain, e) -> fprintf oc "(%a).univ" als_of_expr e
  | Op1 (Range, e) -> fprintf oc "univ.(%a)" als_of_expr e
  | Op (Seq,es) -> fprintf_iter "." als_of_expr oc es
  | Op (Union,es) -> fprintf_iter "+" als_of_expr oc es
  | Op (Diff,es) -> fprintf_iter "-" als_of_expr oc es
  | Op (Inter,es) -> fprintf_iter "&" als_of_expr oc es
  | Op (Cross,es) -> fprintf_iter "->" als_of_expr oc es

and als_of_exprs oc = function
  | [] -> fprintf oc ""
  | e :: es -> fprintf oc "%a, %a" als_of_expr e als_of_exprs es

let pp_test_type = function
  | Acyclic -> "is_acyclic"
  | Irreflexive -> "irreflexive"
  | IsEmpty -> "is_empty"
    
let rec als_of_instrs env class_name ax_list oc = function
  | [] -> ax_list
  | Let (x,args,e) :: instrs ->
     (* NB we assume all args are relations, not sets *)
     let env' = (List.map (fun a -> (a,([],Rel))) args) @ env in
     let def_type = type_of env' e in
     let args_str = List.fold_left (sprintf "%s%s:E->E,") "" args in
     fprintf oc "fun %s [%se:E, X:%s] : %s {\n"
	     x args_str class_name (alloy_type_of def_type);
     fprintf oc "  %a\n" als_of_expr e;
     fprintf oc "}\n";
     let env' =
       (x, (List.map (fun _ -> Rel) args, def_type)) :: env
     in
     als_of_instrs env' class_name ax_list oc instrs
  | LetRec xes :: instrs ->
     failwith
       (asprintf "Recursive definition should have been removed.")
  | Test (t,e,n) :: instrs ->
     fprintf oc "pred %s [e:E, X:%s] {\n" n class_name; 
     fprintf oc "  %s[%a]\n" (pp_test_type t) als_of_expr e;
     fprintf oc "}\n";
     als_of_instrs env class_name (n :: ax_list) oc instrs

let pp_preamble cat_path model_name class_path oc =
  fprintf oc "/* Automatically generated from %s on %s at %s */\n\n"
	  cat_path (today ()) (now ());
  fprintf oc "module %s[E]\n" model_name;
  fprintf oc "open %s[E]\n\n" class_path

let pp_postamble class_name axiom_list oc =
  fprintf oc "pred consistent[e:E, X:%s] {\n" class_name;
  List.iter (fprintf oc "  %s[e,X]\n") axiom_list;
  fprintf oc "}\n"
			
let als_of_model env cat_path model_name class_path class_name oc instrs =
  pp_preamble cat_path model_name class_path oc;
  let axiom_list = als_of_instrs env class_name [] oc instrs in
  pp_postamble class_name axiom_list oc

(*******************************************)
(* The classes of execution that we handle *)
(*******************************************)
	       
type exec_class =
  | Basic_exec
  | C_exec
  | Basic_HW_exec
  | X86_exec
  | Power_exec
  | Arm7_exec
  | Arm8_exec

let parse_exec_class = function
  | "Exec" -> Basic_exec
  | "Exec_C" -> C_exec
  | "Exec_H" -> Basic_HW_exec
  | "Exec_x86" -> X86_exec
  | "Exec_PPC" -> Power_exec
  | "Exec_Arm7" -> Arm7_exec
  | "Exec_Arm8" -> Arm8_exec
  | x -> failwith (asprintf "Unexpected execution class: %s." x)

let rec class_sets = function
  | Basic_exec ->
     ["ev"; "W"; "R"; "F"; "naL"]
  | C_exec ->
     class_sets Basic_exec @ ["A"; "acq"; "rel"; "sc"]
  | Basic_HW_exec ->
     class_sets Basic_exec @ ["A"; "X"]
  | X86_exec ->
     class_sets Basic_HW_exec @ ["MFENCE"]
  | Power_exec ->
     class_sets Basic_HW_exec @ ["sync"; "lwsync"]
  | Arm7_exec ->
     class_sets Basic_HW_exec @ ["dmb"; "dmbst"; "dmbld"; "isb"]
  | Arm8_exec ->
     class_sets Arm7_exec @ ["screl"; "scacq"]

let rec class_rels = function
  | Basic_exec ->
     ["ad"; "cd"; "co"; "coe"; "coi"; "dd"; "ext"; "fr";
      "fre"; "fri"; "poloc"; "rf"; "rfe"; "rfi";
      "sb"; "sloc"; "sthd"]
  | C_exec ->
     class_rels Basic_exec
  | Basic_HW_exec ->
     class_rels Basic_exec @ ["atom"]
  | X86_exec
  | Power_exec
  | Arm7_exec
  | Arm8_exec ->
     class_rels Basic_HW_exec

(*********************************)
(* Processing command-line input *)
(*********************************)
	       
let get_args () =
  let class_path : string list ref = ref [] in
  let class_name : string list ref = ref [] in
  let als_path : string list ref = ref [] in
  let cat_path : string list ref = ref [] in
  let speclist = [
      ("-cn", Arg.String (set_list_ref class_name),
       "Execution class name (mandatory)");
      ("-cp", Arg.String (set_list_ref class_path),
       "Execution class path (mandatory)");
      ("-o", Arg.String (set_list_ref als_path),
       "Output ALS file (mandatory)");
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
  let class_name = get_only_element bad_arg !class_name in
  let class_path = get_only_element bad_arg !class_path in
  let cat_path = get_only_element bad_arg !cat_path in
  let als_path = get_only_element bad_arg !als_path in
  (cat_path, als_path, class_path, class_name)

let check_args (cat_path, als_path, class_path, class_name) =
  assert (Filename.check_suffix cat_path ".cat");
  assert (Filename.check_suffix als_path ".als");
  if Sys.file_exists als_path then
    failwith (asprintf "Target Alloy file already exists.")
  
let parse_file cat_path =
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  Cat_parser.main Cat_lexer.token lexbuf
		  
let main () =
  let (cat_path, als_path, class_path, class_name) = get_args () in
  check_args (cat_path, als_path, class_path, class_name);
  let model_name =
    Filename.chop_extension (Filename.basename als_path)
  in
  let oc = formatter_of_out_channel (open_out als_path) in
  let cat_model = parse_file cat_path in
  let exec_class = parse_exec_class class_name in
  let env =
    (List.map (fun a -> (a,([],Rel))) (class_rels exec_class)) @
      (List.map (fun a -> (a,([],Set))) (class_sets exec_class))
  in
  let cat_model = rename_vars_in_model cat_model in
  let cat_model = unfold_instrs cat_model in
  debug "Cat model: %a" pp_instrs cat_model;
  als_of_model
    env cat_path model_name class_path class_name oc cat_model; 
  exit 0
    
let _ = main ()     
