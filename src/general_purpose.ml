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

(** Some general-purpose definitions *)

open Format

(** Fails with an error message, given as a formatted string *)
let failwith fmt =
  let b = Buffer.create 512 in
  let ppf = formatter_of_buffer b in
  let k =
    pp_print_flush ppf ();
    failwith (Buffer.contents b)
  in
  kfprintf k ppf fmt

let set_list_ref r v = r := (v :: !r)

let get_only_element k = function [x] -> x | _ -> k ()

let get_lone_element k z = function [x] -> x | [] -> z | _ -> k ()

let get_only_two_elements k = function [x;y] -> x,y | _ -> k ()

let rec fprintf_iter s f oc = function
  | [] -> ()
  | [x] -> f oc x
  | x :: xs -> f oc x; fprintf oc "%s" s; fprintf_iter s f oc xs

let fparen f oc x = fprintf oc "(%a)" f x

let pp_str oc s = fprintf oc "%s" s
let pp_pair oc (s,s') = fprintf oc "(%s,%s)" s s'

let today() =
  let open Unix in
  let t = localtime (time ()) in
  sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let now() =
  let open Unix in
  let t = localtime (time ()) in
  sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec

let chop_extension extn path =
  if Filename.check_suffix path extn then
    Filename.chop_extension path
  else
    failwith "File %s does not have extension \"%s\"." path extn

let last ss = List.hd (List.rev ss)
      
let base_of s = last (Str.split (Str.regexp_string "/") s)

let remove_assocs ks =
  List.filter (fun (k,_) -> not (List.mem k ks))

let union xs ys =
  let cons_unique res x = if List.mem x res then res else x::res in
  List.fold_left cons_unique ys xs

let inter xs ys =
  List.filter (fun x -> List.mem x ys) xs

let diff xs ys =
  List.filter (fun x -> not (List.mem x ys)) xs

let invert_rel r =
  List.map (fun (e,e') -> (e',e)) r

type ('k,'v) map = ('k * 'v) list

let strong_assoc map x =
  try List.assoc x map with Not_found -> assert false

(** Example: [invert_map [(k1,v1);(k2,v2);(k3;v1)] = [(v1,[k1;k3]);(v2,[k2])]] *)
let invert_map kvs =
  let add_entry vks (k,v) =
    let ks = try List.assoc v vks with Not_found -> [] in
    (v, k :: ks) :: remove_assocs [v] vks
  in
  List.fold_left add_entry [] kvs

let key_list kvs = List.map fst kvs
let val_list kvs = List.map snd kvs

let compare r e e' = if List.mem (e,e') r then -1 else 1

let exists_pair f xs ys =
  List.exists (fun x -> List.exists (f x) ys) xs

let remove_transitive_edges r =
  let is_transitive (e,e') =
    exists_pair (fun (e1,e1') (e2,e2') ->
      e1 = e && e1' = e2 && e2' = e') r r
  in
  List.filter (fun edge -> not (is_transitive edge)) r

(** [partition true r es] returns a list of partitions of [es], with two elements of [es] being in the same partition iff they are related (in either direction) by [r]. [partition false r es] is similar, but each partitions contains elements that are {i not} related (in either direction) by [r]. *)
let partition invert r es =
  let rec find_related e = function
    | [] -> raise Not_found
    | (e',i) :: _ when
	   if invert then List.mem (e,e') r
	   else not (List.mem (e,e') r) && not (List.mem (e',e) r)
      -> i
    | _ :: map -> find_related e map
  in
  let partition_helper (i, map) e =
    try let i' = find_related e map in (i, (e,i')::map)
    with Not_found -> (i+1, (e,i)::map)
  in
  snd (List.fold_left partition_helper (0, []) es)


