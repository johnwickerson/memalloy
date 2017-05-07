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

open! Format
open! General_purpose
   
type ('k,'v) t = ('k * 'v) list

let remove_assocs ks =
  List.filter (fun (k,_) -> not (List.mem k ks))

(** [strong_assoc map x] returns the value associated with key [x] in [map], and fails if that key is not present *)
let strong_assoc map x =
  try List.assoc x map with Not_found -> assert false

(** [permute_vals (v1,v2) kvs] updates the map [kvs] so that any key that mapped to [v1] now maps to [v2], and vice versa *)
let permute_vals (v1,v2) kvs =
  let permute = function
    | k,v when v=v1 -> k,v2
    | k,v when v=v2 -> k,v1
    | k,v -> k,v
  in
  List.map permute kvs

(** Update an multi-association list (in which the values are lists) *)
let add_assocs map (k,vs) =
  let vs' = try List.assoc k map with Not_found -> [] in
  (k, vs@vs') :: remove_assocs [k] map
			     
(** Example: [invert_map [(k1,v1);(k2,v2);(k3;v1)] = [(v1,[k1;k3]);(v2,[k2])]] *)
let invert_map kvs =
  let add_entry vks (k,v) =
    let ks = try List.assoc v vks with Not_found -> [] in
    (v, k :: ks) :: remove_assocs [v] vks
  in
  List.fold_left add_entry [] kvs

(** Example: [group_map [(k1,v1);(k1,v2);(k2,v3)] = [(k1,[v1;v2]);(k2,[v3])]] *)
let group_map kvs =
  let add_entry kvs (k,v) =
    let vs = try List.assoc k kvs with Not_found -> [] in
    (k, v :: vs) :: remove_assocs [k] kvs
  in
  List.fold_left add_entry [] kvs

let key_list kvs = List.map fst kvs
let val_list kvs = List.map snd kvs
