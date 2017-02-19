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

(** Association lists *)

open Format
open List
open General_purpose

type ('k,'v) t = ('k * 'v) list

let remove_assocs ks =
  filter (fun (k,_) -> not (mem k ks))

let strong_assoc map x =
  try assoc x map with Not_found -> assert false

let permute_vals (v1,v2) kvs =
  let permute = function
    | k,v when v=v1 -> k,v2
    | k,v when v=v2 -> k,v1
    | k,v -> k,v
  in
  map permute kvs
			     
(** Example: [invert_map [(k1,v1);(k2,v2);(k3;v1)] = [(v1,[k1;k3]);(v2,[k2])]] *)
let invert_map kvs =
  let add_entry vks (k,v) =
    let ks = try assoc v vks with Not_found -> [] in
    (v, k :: ks) :: remove_assocs [v] vks
  in
  fold_left add_entry [] kvs

let key_list kvs = map fst kvs
let val_list kvs = map snd kvs
