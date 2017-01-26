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
				
(*************************************)
(* Renaming a few built-in variables *)
(*************************************)

let rename_var = function
  | "A" -> "scacq"
  | "int" -> "sthd"
  | "L" -> "screl"
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
  | Include path -> Include path
		       
let rename_vars_in_model = List.map rename_vars_in_instr

(***********************************)
(* Unfolding recursive definitions *)
(***********************************)

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

let pp_arch oc = function
  | Basic_exec -> fprintf oc "../archs/exec"
  | C_exec -> fprintf oc "../archs/exec_C"
  | Basic_HW_exec -> fprintf oc "../archs/exec_H"
  | X86_exec -> fprintf oc "../archs/exec_x86"
  | Power_exec -> fprintf oc "../archs/exec_ppc"
  | Arm7_exec -> fprintf oc "../archs/exec_arm7"
  | Arm8_exec -> fprintf oc "../archs/exec_arm8"

let pp_Arch oc = function
  | Basic_exec -> fprintf oc "Exec"
  | C_exec -> fprintf oc "Exec_C"
  | Basic_HW_exec -> fprintf oc "Exec_H"
  | X86_exec -> fprintf oc "Exec_X86"
  | Power_exec -> fprintf oc "Exec_PPC"
  | Arm7_exec -> fprintf oc "Exec_Arm7"
  | Arm8_exec -> fprintf oc "Exec_Arm8"

let parse_exec_class = function
  | "BASIC" -> Basic_exec
  | "Exec_C" -> C_exec
  | "Exec_H" -> Basic_HW_exec
  | "X86 TSO" -> X86_exec
  | "PPC" -> Power_exec
  | "Exec_Arm7" -> Arm7_exec
  | "Exec_Arm8" -> Arm8_exec
  | x -> failwith (asprintf "Unexpected execution class: %s." x)

let rec class_sets = function
  | Basic_exec ->
     ["ev"; "W"; "R"; "F"; "naL"; "M"]
  | C_exec ->
     class_sets Basic_exec @ ["A"; "acq"; "rel"; "sc"]
  | Basic_HW_exec ->
     class_sets Basic_exec @ ["A"; "X"]
  | X86_exec ->
     class_sets Basic_HW_exec @ ["MFENCE"]
  | Power_exec ->
     class_sets Basic_HW_exec @
       ["sync"; "lwsync"; "eieio"; "isync";
	"SYNC"; "LWSYNC"; "EIEIO"; "ISYNC"]
  | Arm7_exec ->
     class_sets Basic_HW_exec @
       ["dmb"; "DMBSY"; "dmbst"; "DMBST"; "dmbld"; "DMBLD"; "isb"; "ISB"]
  | Arm8_exec ->
     class_sets Arm7_exec @ ["screl"; "scacq"]

let rec class_rels = function
  | Basic_exec ->
     ["ad"; "addr"; "cd"; "co"; "coe"; "coi"; "ctrl"; "data";
      "dd"; "ext"; "fr"; "fre"; "fri"; "po"; "poloc"; "rf";
      "rfe"; "rfi"; "sb"; "sloc"; "sthd"]
  | C_exec ->
     class_rels Basic_exec
  | Basic_HW_exec ->
     class_rels Basic_exec @ ["atom"]
  | X86_exec
  | Power_exec
  | Arm7_exec
  | Arm8_exec ->
     class_rels Basic_HW_exec

let build_env exec_class = 
  (List.map (fun a -> (a,([],Rel))) (class_rels exec_class)) @
  (List.map (fun a -> (a,([],Set))) (class_sets exec_class))
				  
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
  | Test (t,e,n) -> env 
  | Include path -> type_file path @ env
		  
and type_file path =
  let (model_type, model) = parse_file path in
  let exec_class = parse_exec_class model_type in
  let env = build_env exec_class in
  List.fold_left type_instr env model
				    
(*************************)
(* Generating Alloy code *)
(*************************)
		       
let pp_access_type = function
  | WriteRead -> Op (Union, [Var "W"; Var "R"])
  | Write -> Var "W"
  | Read -> Var "R"
  | Plain -> Op (Diff, [Var "ev"; Var "locked"])
  | Atomic -> Var "locked"
		  
let rec als_of_expr oc = function
  | Empty_rln -> fprintf oc "none -> none"
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
    
let rec als_of_instrs env exec_class ax_list oc = function
  | [] -> ax_list
  | Let (x,args,e) :: instrs ->
     (* NB we assume all args are relations, not sets *)
     let env' = (List.map (fun a -> (a,([],Rel))) args) @ env in
     let def_type = type_of env' e in
     let args_str = List.fold_left (sprintf "%s%s:E->E,") "" args in
     fprintf oc "fun %s [%se:E, X:%a] : %s {\n"
	     x args_str pp_Arch exec_class (alloy_type_of def_type);
     fprintf oc "  %a\n" als_of_expr e;
     fprintf oc "}\n\n";
     let env' =
       (x, (List.map (fun _ -> Rel) args, def_type)) :: env
     in
     als_of_instrs env' exec_class ax_list oc instrs
  | LetRec _ :: _ ->
     failwith "Recursive definition should have been removed."
  | Test (t,e,n) :: instrs ->
     fprintf oc "pred %s [e:E, X:%a] {\n" n pp_Arch exec_class; 
     fprintf oc "  %s[%a]\n" (pp_test_type t) als_of_expr e;
     fprintf oc "}\n\n";
     als_of_instrs env exec_class (n :: ax_list) oc instrs
  | Include path :: instrs ->
     let env' = type_file path in
     fprintf oc "open %s[E]\n\n" (chop_extension ".cat" path);
     als_of_instrs (env' @ env) exec_class ax_list oc instrs

let pp_preamble cat_path model_name exec_class oc =
  fprintf oc "/* Automatically generated from %s on %s at %s */\n\n"
	  cat_path (today ()) (now ());
  fprintf oc "module %s[E]\n" model_name;
  fprintf oc "open %a[E]\n\n" pp_arch exec_class

let pp_postamble exec_class axiom_list oc =
  if axiom_list != [] then begin
      fprintf oc "pred consistent[e:E, X:%a] {\n" pp_Arch exec_class;
      List.iter (fprintf oc "  %s[e,X]\n") axiom_list;
      fprintf oc "}\n"
    end
			
let als_of_model env cat_path model_name exec_class oc instrs =
  pp_preamble cat_path model_name exec_class oc;
  let axiom_list = als_of_instrs env exec_class [] oc instrs in
  pp_postamble exec_class axiom_list oc

(*********************************)
(* Processing command-line input *)
(*********************************)
	       
let get_args () =
  let als_path : string list ref = ref [] in
  let cat_path : string list ref = ref [] in
  let default_unrolling_factor = 3 in
  let unrolling_factor : int list ref = ref [] in
  let speclist = [
      ("-o", Arg.String (set_list_ref als_path),
       "Output ALS file (mandatory)");
      ("-u", Arg.Int (set_list_ref unrolling_factor),
       sprintf "Number of times to unroll recursive definitions (optional, default=%d)" default_unrolling_factor);
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
  let als_path = get_only_element bad_arg !als_path in
  let unrolling_factor =
    get_lone_element bad_arg default_unrolling_factor !unrolling_factor
  in
  (cat_path, als_path, unrolling_factor)

let check_args (cat_path, als_path, unrolling_factor) =
  assert (Filename.check_suffix als_path ".als");
  assert (unrolling_factor >= 0);
  if Sys.file_exists als_path then
    failwith "Target Alloy file already exists."
		  
let main () =
  let (cat_path, als_path, unrolling_factor) = get_args () in
  check_args (cat_path, als_path, unrolling_factor);
  let model_name =
    Filename.chop_extension (Filename.basename als_path)
  in
  let oc = formatter_of_out_channel (open_out als_path) in
  let (model_type, cat_model) = parse_file cat_path in
  let exec_class = parse_exec_class model_type in
  let env = build_env exec_class in
  let cat_model = rename_vars_in_model cat_model in
  let cat_model = unfold_instrs unrolling_factor cat_model in
  debug "Cat model: %a" pp_instrs cat_model;
  als_of_model env cat_path model_name exec_class oc cat_model; 
  exit 0
    
let _ = main ()     
