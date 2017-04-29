(*
MIT License

Copyright (c) 2017 by John Wickerson and Tyler Sorensen.

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

(** Comparing two .soln files *)

open Format
open General_purpose

let parse_soln path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  Soln_parser.soln Soln_lexer.token lexbuf
   
let get_args () =
  let soln_paths = ref [] in
  let speclist = [] in
  let usage_msg =
    "Comparing two solns.\nUsage: `compare_solns <soln1.soln> <soln2.soln>`. Prints '<=' on stdout if soln1 is less than or equal to soln2, and '>' otherwise."
  in
  Arg.parse speclist (set_list_ref soln_paths) usage_msg;
  let soln1_path, soln2_path = match !soln_paths with
    | [p1;p2] -> p1,p2
    | _ -> raise (Arg.Bad "Expected two .soln files")
  in
  soln1_path, soln2_path
                      
let main () =
  let soln1_path, soln2_path = get_args () in
  let soln1 = parse_soln soln1_path in
  let soln2 = parse_soln soln2_path in
  let answer = match soln1, soln2 with
  | Soln.Single x1, Soln.Single x2 ->
     Exec.subexec x1 x2
  | Soln.Double (x1,y1,pi1), Soln.Double(x2,y2,pi2) ->
     let cmp1 = Exec.subexec x1 x2 in
     let cmp2 = Exec.subexec y1 y2 in
     cmp1 && cmp2 && (pi1 = pi2) (* probably not quite correct *)
  | _ -> failwith "Incompatible solution types!"
  in
  if answer then printf "yes" else printf "no";
  exit 0  

let _ = main ()
