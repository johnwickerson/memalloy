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

(** Extension to the List module *)

open Format
open List
open General_purpose
       
let exists_pair f xs ys =
  exists (fun x -> exists (f x) ys) xs

let the = function
  | [x] -> x
  | _ -> raise Not_found

let rec pp_gen s f oc = function
  | [] -> ()
  | [x] -> f oc x
  | x :: xs -> f oc x; fprintf oc "%s" s; pp_gen s f oc xs

let pp f oc xs =
  let rec pp = function
    | [] -> ()
    | [x] -> f oc x
    | x :: xs -> f oc x; fprintf oc "; "; pp xs
  in
  fprintf oc "["; pp xs; fprintf oc "]"
						 
let mapi f xs =
  let rec mapi n f = function
    | [] -> []
    | x::xs -> f n x :: mapi (n+1) f xs
  in
  mapi 0 f xs

let iteri f xs =
  let rec iteri n f = function
    | [] -> ()
    | x::xs -> f n x; iteri (n+1) f xs
  in
  iteri 0 f xs
					     
let max xs =
  hd (rev (sort compare xs))
