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

open! Format

(** Fails with an error message, given as a formatted string *)
let failwith fmt =
  let b = Buffer.create 512 in
  let k ppf =
    pp_print_flush ppf ();
    failwith (Buffer.contents b)
  in
  kfprintf k (formatter_of_buffer b) fmt

let set_list_ref r v = r := (v :: !r)
let set_option_ref r v = r := Some v

let fparen f oc x = fprintf oc "(%a)" f x

let pp_str oc s = fprintf oc "%s" s

(** [range i j] returns [i, i+1, ..., j] *)
let rec range i j = if i > j then [] else i :: (range (i+1) j)

(** [count p] returns the first non-negative integer that does not satisfy the predicate [p] *)
let count p =
  let rec count_helper i = if p i then count_helper (i+1) else i in
  count_helper 0

(** Identity function *)
let iden x = x
