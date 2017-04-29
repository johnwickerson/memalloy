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

(** Datatype for representing a series-parallel order *)

open Format
open General_purpose

type 'a t =
  | Lf of 'a
  | Br of 'a t list list

(** [map f spo] goes through [spo], replacing each leaf value [x] with [f x] *)
let rec map f = function
  | Lf x -> Lf (f x)
  | Br oss -> Br (List.map (List.map (map f)) oss)

(** [mapi f spo] goes through [spo], replacing the [n]th leaf value [x] with [f n x] *)
let mapi f o =
  let rec mapi n = function
    | Lf x -> n+1, Lf (f n x)
    | Br oss ->
       let n,oss = mapi'' n oss in
       n, Br oss
  and mapi'' n = function
    | [] -> n, []
    | os::oss ->
       let n,os = mapi' n os in
       let n,oss = mapi'' n oss in
       n, os::oss
  and mapi' n = function
    | [] -> n, []
    | o::os ->
       let n,o = mapi n o in
       let n,os = mapi' n os in
       n, o::os
  in
  snd (mapi 0 o)

(** [mk_rename spo] builds an association list that associates each leaf value with its traversal order in [spo] *)
let mk_rename o =
  let rec mk_rename (n,acc) = function
    | Lf x -> n+1, (x,n)::acc
    | Br oss -> mk_rename_par (n,acc) oss
  and mk_rename_par (n,acc) = function
    | [] -> n,acc
    | os::oss -> mk_rename_par (mk_rename_seq (n,acc) os) oss
  and mk_rename_seq (n,acc) = function
    | [] -> n,acc
    | o::os -> mk_rename_seq (mk_rename (n,acc) o) os
  in
  List.rev (snd (mk_rename (0,[]) o))

(** [pp pp_Lf oc spo] pretty-prints [spo] on channel [oc], using [pp_Lf] to pretty-print leaf values *)
let pp pp_Lf oc spo =
  let rec pp n = function
    | Lf x -> pp_Lf oc (n,x); n+1
    | Br oss ->
       fprintf oc "Br [";
       let n = pp_par n oss in
       fprintf oc "]";
       n
  and pp_par n = function
    | [] -> n
    | os::oss ->
       fprintf oc "[";
       let n = pp_seq n os in
       fprintf oc "]";
       if oss=[] then n else (
         fprintf oc " + ";
         pp_par n oss
       )
  and pp_seq n = function
    | [] -> n
    | o::os ->
       (* fprintf oc "["; *)
       let n = pp n o in
       (* fprintf oc "]"; *)
       if os=[] then n else (
         fprintf oc "; ";
         pp_seq n os
       )
  in
  let _ = pp 0 spo in ()

(** [perms spo] returns the list of all permutations of [spo]. Permutations are obtained by permuting elements that are in parallel, but preserving the order of elements that are in series. *)
let perms spo =
  (* e.g. insertions (1,[2;3]) = [[1;2;3];[2;1;3];[2;3;1]] *)
  let rec insertions (x,xs) = match xs with
    | [] -> [[x]]
    | h::t -> (x::xs) :: (List.map (fun y -> h::y) (insertions (x,t)))
  in                      
  let rec perms = function
    | Lf x -> [Lf x]
    | Br oss -> List.map (fun oss -> Br oss) (perms_par oss)
  and perms_par = function
    | [] -> [[]]
    | os::oss ->
       let prod = MyList.cartesian (perms_seq os) (perms_par oss) in
       List.concat (List.map insertions prod)
  and perms_seq = function
    | [] -> [[]]
    | o::os ->
       let prod = MyList.cartesian (perms o) (perms_seq os) in
       List.map (fun (o,os) -> o::os) prod
  in
  perms spo
  
let rec size = function
  | Lf _ -> 1
  | Br oss -> MyList.sum (List.map size_list oss)
and size_list os = MyList.sum (List.map size os)
  
let rec is_sorted = function
  | Lf _ -> true
  | Br oss -> 
     let sizes = List.map size_list oss in
     (List.sort compare sizes = sizes)
     && List.for_all (List.for_all is_sorted) oss

let sorted_perms o = List.filter is_sorted (perms o)

(** Converts an spo with [unit] leaves into a one with [int] leaves, where each leaf is labelled with its traversal position *)
let add_numbers o = mapi (fun n () -> n) o
  
     
(*
  let o =
let thd1 = [Br [[Lf 1; Lf 2]; [Lf 3]; [Lf 4]]; Lf 5] in
let thd2 = [Lf 6; Lf 7] in
Br[thd1;thd2]

  perms o
*)
