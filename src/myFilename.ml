(*
MIT License

Copyright (c) 2018 by John Wickerson.

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

(** Extensions to the Filename module. *)

open! Format
open! General_purpose

open Filename

(** Concatenate a list of directories to build a single path. E.g. [concat ["/usr/local"; "bin"; "ocaml"]] produces "/usr/local/bin/ocaml". *)
let concat xs = List.fold_left concat "" xs

(** [iter f dir] applies [f] to each filename in directory [dir]. Each filename provided to [f] is prefixed with [dir]. For instance, if the directory ["foo/bar"] contains files ["a"], ["b"], and ["c"], then [iter f "foo/bar"] will call [f "foo/bar/a"], [f "foo/bar/b"], and [f "foo/bar/c"]. *) 
let iter f dir =
  let files_arr = Sys.readdir dir in
  let paths_arr = Array.map (fun file -> concat [dir; file]) files_arr in
  Array.iter f paths_arr
