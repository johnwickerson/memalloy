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

(** Relations (implemented as lists of pairs) *)

open! Format
open! General_purpose

type 'a t = ('a * 'a) list

let invert r =
  List.map (fun (e,e') -> (e',e)) r

let dom r = List.map fst r
let rng r = List.map snd r

(** [compare r e e'] returns [-1] if [(e,e')] is in [r], [1] if [(e',e)] is in [r], and [0] if [e] and [e'] are unrelated by [r]. *) 
let compare r e e' =
  if List.mem (e,e') r then -1 else
    if List.mem (e',e) r then 1 else 0

(** [remove_edges r1 r2 r] returns the relation [r] but without any edges that are in [r1;r2] *)
let remove_edges r1 r2 r =
  let is_transitive (e,e') =
    MyList.exists_pair (fun (e1,e1') (e2,e2') ->
			e1 = e && e1' = e2 && e2' = e') r1 r2
  in
  List.filter (fun edge -> not (is_transitive edge)) r

(** [remove_transitive_edges r] returns the relation [r] but without any transitive edges. It is assumed that [r] is transitive to begin with. *)
let remove_transitive_edges r = remove_edges r r r

(** [partition r es] returns a list of partitions of [es], with two elements of [es] being in the same partition iff they are related (in either direction) by [r]. *)
let partition r es =
  let rec find_partition e = function
    | [] -> raise Not_found
    | (e',i) :: _ when List.mem (e,e') r || List.mem (e',e) r -> i
    | _ :: map -> find_partition e map
  in
  let partition_helper (i, map) e =
    try let i' = find_partition e map in (i, (e,i')::map)
    with Not_found -> (i+1, (e,i)::map)
  in
  snd (List.fold_left partition_helper (0, []) es)

let union = MySet.union
let diff = MySet.diff
let inter = MySet.inter

(** Relational composition. [compose r1 r2] gives the list of all [(a,c)] for which there exists a [b] for which [(a,b)] is in [r1] and [(b,c)] is in [r2]. *)
let compose r1 r2 =
  let glom ((a,b),(b',c)) = if b=b' then [(a,c)] else [] in
  MyList.remove_dupes (List.concat (List.map glom (MyList.cartesian r1 r2)))

(** [trancl r] calculates the transitive closure of the relation [r]. *)
let trancl r =
  let rec trancl n = if n = 0 then [] else union r (compose r (trancl (n-1))) in
  trancl (List.length r)

(** [sq s] calculates the cartesian product of the set [s] with itself. *)
let sq s = MyList.cartesian s s
