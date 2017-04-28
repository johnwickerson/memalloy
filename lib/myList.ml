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
  let rec mapi n = function
    | [] -> []
    | x::xs -> f n x :: mapi (n+1) xs
  in
  mapi 0 xs

let iteri_gen g f xs =
  let rec iteri_gen n = function
    | [] -> ()
    | [x] -> f n x;
    | x::xs -> f n x; g (); iteri_gen (n+1) xs
  in
  iteri_gen 0 xs
  
let iteri f xs = iteri_gen (fun () -> ()) f xs

let foldi f a xs =
  let rec foldi n = function
    | [] -> a
    | x::xs -> f n x (foldi (n+1) xs)
  in
  foldi 0 xs
			     
let max xs =
  hd (rev (sort compare xs))

let cartesian xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)

let sum xs = List.fold_left (+) 0 xs
