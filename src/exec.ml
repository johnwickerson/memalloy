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

(** Datatype for representing executions *)

open! Format
open! General_purpose
	  
(** An execution is a list of named event sets and a list of named event relations *)
type t = {
    sets : (string * Evt.t MySet.t) MySet.t;
    rels : (string * Evt.t Rel.t) MySet.t;
  }

(** Basic pretty-printing of executions *)
let pp_exec oc exec =
  let pp_set (name,tuples) =
    fprintf oc "Set: %s={%a}\n" name
	    (MyList.pp_gen "," Evt.pp) tuples
  in
  let pp_rel (name,tuples) =
    fprintf oc "Rel: %s={%a}\n" name
	    (MyList.pp_gen "," (fparen (Pair.pp Evt.pp "," Evt.pp)))
	    tuples
  in
  List.iter pp_set exec.sets;
  List.iter pp_rel exec.rels

(** The empty execution *)
let empty_exec = { sets = []; rels = [] }

(** {2 Setters and getters } *)

(** [get_set x s] returns the set of [s] events in [x], giving the empty set if [s] is not a field of [x] *)
let get_set x s = try List.assoc s x.sets with Not_found -> []

(** [get_sets x e] returns a list of sets of [x] to which [e] belongs *)
let get_sets x e =
  List.map fst (List.filter (fun (_,es) -> List.mem e es) x.sets)

(** [get_rel x r] returns the set of [r] edges in [x], giving the empty relation if [r] is not a field of [x] *)
let get_rel x r = try List.assoc r x.rels with Not_found -> []

(** [update_rel x (r_name, r)] returns an execution that is the same as [x] but the relation named [r_name] now comprises the edges in [r] *)
let update_rel x (r_name, r) =
  { x with rels = (r_name, r) :: (Assoc.remove_assocs [r_name] x.rels) }

(** [update_set x (s_name, s)] returns an execution that is the same as [x] but the set named [s_name] now comprises the events in [s] *)   
let update_set x (s_name, s) =
  { x with sets = (s_name, s) :: (Assoc.remove_assocs [s_name] x.sets) }

(** {2 Removing redundant edges and event-labels } *)
    
(** [remove_transitive r_name x] returns an execution that is the same as [x] but the relation named [r_name] has been replaced with its intransitive core *)
let remove_transitive r_name x =
  let r = get_rel x r_name in
  let r = Rel.remove_transitive_edges r in
  update_rel x (r_name, r)

(** [remove_stale_rfs x] returns an execution that is the same as [x] but the "stale" {i rf}-edges (i.e. those that are also in {i co;rf}) have been removed *)
let remove_stale_rfs x =
  let rf = get_rel x "rf" in
  let co = get_rel x "co" in
  let r = Rel.remove_edges co rf rf in
  update_rel x ("rf", r)

let tidy_exec x =
  let x = remove_stale_rfs x in
  let sb = get_rel x "sb" in
  let reduce_fence_rel x f_name =
    let f = get_rel x f_name in
    let f = Rel.remove_edges sb f f in
    let f = Rel.remove_edges f sb f in
    update_rel x (f_name, f)
  in
  let all_fences =
    ["dmb"; "dmbst"; "dmbld"; "isb";
     "sync"; "lwsync"; "isync";
     "mfence";
     "membar_cta"; "membar_gl"; "membar_sys"]
  in
  let x = List.fold_left reduce_fence_rel x all_fences in
  let remove_implied_rel x (r1_name, r2_name) =
    let r1 = get_rel x r1_name in
    let r2 = get_rel x r2_name in
    let r2 = List.filter (fun ee -> not (List.mem ee r1)) r2 in
    update_rel x (r2_name, r2)
  in
  let x = List.fold_left remove_implied_rel x Archs.all_implied_rels in
  let remove_implied_set x (s1_name, s2_name) =
    let s1 = get_set x s1_name in
    let s2 = get_set x s2_name in
    let s2 = List.filter (fun e -> not (List.mem e s1)) s2 in
    update_set x (s2_name, s2)
  in
  let x = List.fold_left remove_implied_set x Archs.all_implied_sets in
  x
	     
(** {2 Resolving locations, threads and values } *)

(** A collection of maps that identify the thread, location, value written, and value read (where applicable) of each event *)
type execution_maps = {
    thd_map : (Evt.t, Tid.t) Assoc.t;
    loc_map : (Evt.t, MyLocation.t) Assoc.t;
    wval_map : (Evt.t, Value.t) Assoc.t;
    rval_map : (Evt.t, Value.t) Assoc.t;
  }

let pp_maps oc maps =
  fprintf
    oc "thd_map = {%a}, loc_map = {%a}"
    (MyList.pp_gen "," (Pair.pp Evt.pp "=" Tid.pp)) maps.thd_map
    (MyList.pp_gen "," (Pair.pp Evt.pp "=" MyLocation.pp)) maps.loc_map

(** Build a map from each write event to the value that it writes. Initial writes, if present, write zero; all other writes write positive integers. The value written by each write event corresponds to its position in the coherence order. *)
let mk_wval_map loc_map co ws iws =
  let loc_map = List.filter (fun (e,_) -> List.mem e ws) loc_map in
  let loc_classes = Assoc.val_list (Assoc.invert_map loc_map) in
  let loc_classes = List.map (List.sort (Rel.compare co)) loc_classes in
  let rec mk_val (i,res) e = match i, List.mem e iws with
    | 0, false -> mk_val (1,res) e
    | _ -> (i+1, (e,i)::res)
  in
  let tag_with_indices es =
    snd (List.fold_left mk_val (0,[]) es)
  in
  List.concat (List.map tag_with_indices loc_classes)

(** Build a map from each read event to the value that it reads. If the event has an incoming reads-from edge, then this value is the value written by the source of that edge. If the event has no incoming reads-from edge, then this value is 0. *)
let mk_rval_map wval_map rf =
  let find_src e = try
      let w = List.assoc e (Rel.invert rf) in
      try List.assoc w wval_map
      with Not_found -> failwith "Expected write to have a value"
    with Not_found -> 0
  in
  List.map (fun e -> (e, find_src e))

(** Create maps that identify threads, locations and values for each event in the given execution *)
let resolve_exec x =
  let iws = get_set x "IW" in
  let rs = get_set x "R" in
  let ws = get_set x "W" in
  let rw = MySet.union rs ws in
  let nI = MySet.diff (get_set x "EV") iws in
  let thd_map = Rel.partition (get_rel x "sthd") nI in
  let loc_map = Rel.partition (get_rel x "sloc") rw in
  let wval_map = mk_wval_map loc_map (get_rel x "co") ws iws in
  let rval_map = mk_rval_map wval_map (get_rel x "rf") rs in
  { thd_map = thd_map; loc_map = loc_map;
    wval_map = wval_map; rval_map = rval_map }

(** [rectify_maps (x,xmaps) (y,ymaps) pi] returns a new set of execution maps for [y] that is consistent with the threads/locations/values used for [x] according to the mapping [pi]. For instance, if an event {i e} in [x] has location {i l} according to [xmaps], and [pi] relates {i e} to {i e'} in [y], then {i e'} will also have location {i l} in the returned execution maps. *)
let rectify_maps (x,xmaps) (y,ymaps) pi =
  let xev = get_set x "EV" in
  let yev = get_set y "EV" in
  let fix map' map e =
    let is_mismatch e' =
      try List.mem (e',e) pi && List.assoc e' map' <> List.assoc e map
      with Not_found -> false
    in
    try
      let e' = List.find is_mismatch xev in
      let v = Assoc.strong_assoc map e in
      let v' = Assoc.strong_assoc map' e' in
      printf "Permuting %d and %d!\n" v v';
      Assoc.permute_vals (v, v') map
    with Not_found -> map
  in
  printf "Permuting threads!\n";
  let thd_map = List.fold_left (fix xmaps.thd_map) ymaps.thd_map yev in
  printf "Permuting locations!\n";
  let loc_map = List.fold_left (fix xmaps.loc_map) ymaps.loc_map yev in
  { ymaps with thd_map = thd_map; loc_map = loc_map }

(** Partition the events [es] of execution [x] so that each partition is strongly sequenced before the next. *)
let rec partition_seq x es =
  if List.length es = 1 then
    [es]
  else
    let sb = Rel.trancl (get_rel x "sb") in
    let not_sb = Rel.diff (Rel.sq es) sb in
    let not_sa = Rel.diff (Rel.sq es) (Rel.invert sb) in
    let unseq = Rel.trancl (Rel.inter not_sb not_sa) in
    let partitions = Assoc.val_list (Assoc.invert_map (Rel.partition unseq es)) in
    let compare_partitions p1 p2 = Rel.compare sb (List.hd p1) (List.hd p2) in
    let partitions = List.sort compare_partitions partitions in
    let permuted_partitions = List.map (partition_par x) partitions in
    List.map List.concat (MyList.n_cartesian permuted_partitions)

(** Partition the events [es] of execution [x] so that there is no sequencing between different partitions. *)
and partition_par x es =
  if List.length es = 1 then
    [es]
  else
    let sb = Rel.trancl (get_rel x "sb") in
    let partitions = Assoc.val_list (Assoc.invert_map (Rel.partition sb es)) in
    let compare_partitions es1 es2 = List.compare_lengths es1 es2 in
    let partitions = List.sort compare_partitions partitions in
    let permuted_partitions = List.map (partition_seq x) partitions in
    List.map List.concat
      (List.concat (List.map (MyList.lin_extns compare_partitions)
                      (MyList.n_cartesian permuted_partitions)))

(** [valid_evt_orders x] returns a list of lists of [x]'s events, where each list is a valid event order. Valid means that the order respects the sequenced-before relation, and keeps events in the same thread (and other unsequenced collections) together. *)
let valid_evt_orders x =
  let iws = get_set x "IW" in
  let ev = get_set x "EV" in
  let nI = MySet.diff ev iws in
  let thd_map = Rel.partition (get_rel x "sthd") nI in
  let thd_classes = Assoc.val_list (Assoc.invert_map thd_map) in
  let nI_orders = List.concat (List.map (partition_seq x) thd_classes) in
  let iw_orders = MyList.perms iws in
    List.map (fun (iw_order, nI_order) -> iw_order @ nI_order)
      (MyList.cartesian iw_orders nI_orders)

(** [rename evt_order x] produces a new execution that is isomorphic to [x] but the events are permuted into the order given by [evt_order]. *)
let rename evt_order x =
  let ev = get_set x "EV" in
  let renaming_map = List.combine evt_order ev in
  let rename e = List.assoc e renaming_map in
  let rename_set s = List.sort Evt.compare (List.map rename s) in
  let rename_rel r =
    List.sort Evt.compare_pair (List.map (fun (e,e') -> (rename e, rename e')) r)
  in
  let sets = List.map (fun (s_name, s) -> (s_name, rename_set s)) x.sets in
  let rels = List.map (fun (r_name, r) -> (r_name, rename_rel r)) x.rels in
  {sets;rels;}

(** [hash_exec oc x] produces, on output channel [oc], a hash representing all the isomorphic variants of the execution [x] *)
let hash_exec oc x =
  let mk_hash evt_order =
      let b = Buffer.create 512 in
      let oc_b = formatter_of_buffer b in
      let () = fprintf oc_b "%a" pp_exec (rename evt_order x) in
      Buffer.contents b
  in
  let hashes = List.map mk_hash (valid_evt_orders x) in
  let hashes = List.sort String.compare hashes in
  (* MyList.pp_gen "+\n" pp_str oc hashes *)
  let hashes = List.map (fun h -> Digest.to_hex (Digest.string h)) hashes in
  MyList.pp_gen "+" pp_str oc hashes
    
let hash_double_exec oc (_x,_y,_pi) =
  fprintf oc "NOT DONE YET!"
  
(** {2 Useful for testing } *)

let e0,e1,e2,e3,e4,e5,e6,e7,e8,e9 =
  "E$0", "E$1", "E$2", "E$3", "E$4", "E$5", "E$6", "E$7", "E$8", "E$9"

let sb = Rel.trancl [e5,e2; e2,e1; e2,e3; e1,e4; e3,e4; 
                      e5,e7; e7,e8; e8,e6; e4,e6; ]
let test_exec = {
    sets = [ "EV",  [e0;e1;e2;e3;e4;e5;e6;e7;e8;e9];
             "IW",  [e0;e9];
             "R",   [e0;e1]; ];
    rels = [ "sthd", Rel.sq [e1;e2;e3;e4;e5;e6;e7;e8];
             "sb",   sb; ]
  }
  
