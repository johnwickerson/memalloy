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

(** String manipulation *)

open! Format
open! General_purpose

(** [startswith s t] holds if [s] starts with the substring [t] *)
let startswith s t =
  let s_len = String.length s in
  let t_len = String.length t in
  if t_len > s_len then false else
    let s_start = String.sub s 0 t_len in
    s_start = t

(** [endswith s t] holds if [s] ends with the substring [t] *)
let endswith s t =
  let s_len = String.length s in
  let t_len = String.length t in
  if t_len > s_len then false else
    let s_end = String.sub s (s_len - t_len) t_len in
    s_end = t

(** If [s] ends with [suf], then [chop_suffix suf s] returns [s] without [suf], otherwise it just returns [s] *)
let chop_suffix suf s =
  if endswith s suf then
    let s_len = String.length s in
    let suf_len = String.length suf in
    String.sub s 0 (s_len - suf_len)
  else s

(** If [s] starts with [pre], then [chop_prefix pre s] returns [s] without [pre], otherwise it just returns [s] *)
let chop_prefix pre s =
  if startswith s pre then
    let s_len = String.length s in
    let pre_len = String.length pre in
    String.sub s pre_len (s_len - pre_len)
  else s


(** If [str] starts with [prefix] then return the string without [prefix] otherwise None *)
let chop_prefix_opt pre s =
  if startswith s pre
  then Some (chop_prefix pre s)
  else None

(** [pad_right n s] returns the string of length [n] formed by adding spaces to the end of [s] (or by trimming the end off [s] if [s] is already longer than [n]). *)
let pad_right n s =
  let s_length = String.length s in
  if s_length > n then
    String.sub s 0 n
  else
    s ^ String.make (n - s_length) ' '
  
