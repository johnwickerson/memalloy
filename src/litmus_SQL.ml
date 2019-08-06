(*
MIT License

Copyright (c) 2017 by Michail Pardalos.

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

(** Printing a litmus test as an SQL litmus test *)

open! Format
open! General_purpose

type isolation_level =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

let pp_isolation_level = function
  | ReadUncommitted -> "READ UNCOMMITTED"
  | ReadCommitted   -> "READ COMMITTED"
  | RepeatableRead  -> "REPEATABLE READ"
  | Serializable    -> "SERIALIZABLE"

(**
   Get the isolation level of a transaction by checking the isolation level of
   its first event
*)
let isolation_level_of_transaction = function
  | ((Litmus.Basic (_, attrs))::_) ->
    if List.mem "RU" attrs then ReadUncommitted else
    if List.mem "RC" attrs then ReadCommitted else
    if List.mem "RR" attrs then RepeatableRead else
    if List.mem "SER" attrs then Serializable else
      failwith ""
  | ((Litmus.If _)::_) ->
    failwith "Unexpected if in SQL litmus test"
  | [] ->
    failwith "Unexpected empty transaction in SQL litmus test"

let pp_reg oc (tid,reg) = fprintf oc "t%dr%d" tid reg

let pp_instr oc = function
  | Litmus.Load ((_, reg),Just loc), _ ->
    fprintf oc "SELECT val AS r%d FROM table WHERE loc=\"%a\""
      reg
      MyLocation.pp loc
  | Litmus.Store (Just loc, Just value), _ ->
    fprintf oc "UPDATE table SET val=%d WHERE loc=\"%a\""
      value
      MyLocation.pp loc
  | Litmus.Fence, attrs -> if List.mem "C" attrs
    then fprintf oc "COMMIT TRANSACTION"
    else failwith "Non-commit fence in SQL litmus test"
  | Litmus.Store (Just _, _), _ ->
    failwith "Unexpected location expression in SQL litmus test"
  | Litmus.Store (_, _), _ ->
    failwith "Unexpected value expression in SQL litmus test"
  | Litmus.Load (_,Madd _), _ ->
    failwith "Unexpected location expression in SQL litmus test"
  | Litmus.LoadLink _, _ ->
    failwith "Unexpected LoadLink instruction in SQL litmus test"
  | Litmus.StoreCnd _, _ ->
    failwith "Unexpected StoreCnd instruction in SQL litmus test"
  | Litmus.Cas _, _ ->
    failwith "Unexpected Cas instruction in SQL litmus test"

let pp_transaction oc tid cs =
  let transaction_isolation_level = pp_isolation_level (isolation_level_of_transaction cs) in
  fprintf oc "-- Transaction %d\n" tid;
  fprintf oc "BEGIN TRANSACTION\n";
  fprintf oc "SET ISOLATION LEVEL %s\n" transaction_isolation_level;
  MyList.pp_gen ";\n" (Litmus.pp_component pp_instr) oc cs;
  fprintf oc ";\n\n"

let pp_location_table oc locs =
  fprintf oc "-- Table definition\n";
  fprintf oc "CREATE TABLE table (\n";
  fprintf oc "    loc varchar(5) PRIMARY KEY,\n";
  fprintf oc "    val integer NOT NULL\n";
  fprintf oc ")\n";

  (* Insert initial rows *)
  let pp_insert_row loc =
    fprintf oc "INSERT INTO table VALUES (\"%a\", 0);\n"
      MyLocation.pp loc
  in
  List.iter pp_insert_row locs;
  fprintf oc "\n\n"

let pp oc lt =
  pp_location_table oc lt.Litmus.locs;
  List.iteri (pp_transaction oc) lt.Litmus.thds;
  fprintf oc "Final: ";
  let pp_cnstrnt oc (a,v) = fprintf oc "%a==%d" Litmus.pp_addr a v in
  MyList.pp_gen " && " pp_cnstrnt oc lt.post
